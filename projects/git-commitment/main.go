package main

import (
	"flag"
	"fmt"
	"strings"
	"sync"

	"tinygo.org/x/bluetooth"
)

var KnownServiceUUIDs = []bluetooth.UUID{
	bluetooth.ServiceUUIDCyclingSpeedAndCadence,
	bluetooth.ServiceUUIDCyclingPower,
	bluetooth.ServiceUUIDHeartRate,

	// General controllable device, seems more involved.
	// bluetooth.ServiceUUIDFitnessMachine,
}

var KnownServiceCharacteristicUUIDs = map[bluetooth.UUID][]bluetooth.UUID{
	bluetooth.ServiceUUIDCyclingPower: {
		bluetooth.CharacteristicUUIDCyclingPowerMeasurement,
		bluetooth.CharacteristicUUIDCyclingPowerFeature,
		// TODO:
		// Not a standardized characteristic, but this is offered by KICKR.
		// See GoldenCheetah source for some use examples:
		// https://github.com/GoldenCheetah/GoldenCheetah/blob/master/src/Train/BT40Device.cpp
		//
		// var WahooKickrControlCharacteristic = bluetooth.ParseUUID(
		// 	"a026e005-0a7d-4ab3-97fa-f1500f9feb8b"
		// )
		// TODO: Also, how does this one work?
		// bluetooth.CharacteristicUUIDCyclingPowerControlPoint,
	},
	bluetooth.ServiceUUIDHeartRate: {
		bluetooth.CharacteristicUUIDHeartRateMeasurement,
	},
}
var KnownServiceNames = map[bluetooth.UUID]string{
	bluetooth.ServiceUUIDCyclingSpeedAndCadence: "Cycling Speed and Cadence",
	bluetooth.ServiceUUIDCyclingPower:           "Cycling Power",
	bluetooth.ServiceUUIDHeartRate:              "Heart Rate",
}

type MetricKind int

const (
	MetricHeartRate = iota
	MetricCyclingPower
	MetricCyclingSpeed
	MetricCyclingCadence
)

type DeviceMetric struct {
	kind MetricKind
}

type MetricSink struct {
}

type MetricSource struct {
	sinks []chan DeviceMetric

	svc *bluetooth.DeviceService
	ch  *bluetooth.DeviceCharacteristic
}

func NewMetricSource(
	svc *bluetooth.DeviceService,
	ch *bluetooth.DeviceCharacteristic,
) MetricSource {
	return MetricSource{
		sinks: []chan DeviceMetric{},
		svc:   svc,
		ch:    ch,
	}
}

func (src *MetricSource) Name() string {
	switch src.ch.UUID() {
	case bluetooth.CharacteristicUUIDCyclingPowerMeasurement:
		return "Cycling Power Measure"
	case bluetooth.CharacteristicUUIDCyclingPowerFeature:
		return "Cycling Power Feature"
	case bluetooth.CharacteristicUUIDHeartRateMeasurement:
		return "Heart Rate"
	}
	return fmt.Sprintf("<unknown: %s>", src.ch.UUID().String())

}

func (src *MetricSource) AddSink(sink chan DeviceMetric) {
	src.sinks = append(src.sinks, sink)

	// Start listenening first time we add a sink
	if len(src.sinks) == 1 {
		src.ch.EnableNotifications(src.handleNotification)
	}
}

func (src *MetricSource) handleNotification(buf []byte) {
	fmt.Printf("%s: got %+v\n", src.Name(), buf)

	// TODO
	switch src.ch.UUID() {
	case bluetooth.CharacteristicUUIDCyclingPowerMeasurement:
	case bluetooth.CharacteristicUUIDCyclingPowerFeature:
	case bluetooth.CharacteristicUUIDHeartRateMeasurement:
	}
}

func scanDevices() {
	adapter := bluetooth.DefaultAdapter
	fmt.Println("Starting device scan...")

	if err := adapter.Enable(); err != nil {
		fmt.Println("FATAL: Failed to enable BLE")
		panic(err)
	}

	// Keep track of addresses we've already looked ad
	addrsChecked := map[string]bool{}

	onScanResult := func(bt *bluetooth.Adapter, result bluetooth.ScanResult) {
		if _, seen := addrsChecked[result.Address.String()]; seen {
			return
		}
		addrsChecked[result.Address.String()] = true

		serviceNames := []string{}
		for _, s := range KnownServiceUUIDs {
			if !result.HasServiceUUID(s) {
				continue
			}

			serviceNames = append(serviceNames, KnownServiceNames[s])
		}

		// No matching services, skip this device.
		if len(serviceNames) == 0 {
			return
		}

		fmt.Printf("%s %-20s %-20s [RSSI:%d]\n",
			result.Address.String(),
			result.LocalName(),
			strings.Join(serviceNames, ","),
			result.RSSI,
		)
	}

	if err := adapter.Scan(onScanResult); err != nil {
		fmt.Println("FATAL: Failed to scan for devices")
		panic(err)
	}

	fmt.Println("Scan complete.")
}

var (
	flagScan                    = flag.Bool("scan", false, "scan for nearby devices")
	flagHeartRateAddr           = flag.String("hr", "", "address for heart rate device")
	flagCyclingPowerAddr        = flag.String("power", "", "address for cycling power device")
	flagCyclingSpeedCadenceAddr = flag.String("speed", "", "address for cycling speed/cadence device")
)

func init() {
	flag.Parse()
}

func main() {
	if *flagScan {
		scanDevices()
		return
	}

	adapter := bluetooth.DefaultAdapter
	if err := adapter.Enable(); err != nil {
		fmt.Println("FATAL: Failed to enable BLE")
		panic(err)
	}

	deviceChan := make(chan *bluetooth.Device)

	wg := sync.WaitGroup{}

	connectRetry := func(addr string) {
		uuid, err := bluetooth.ParseUUID(addr)
		if err != nil {
			fmt.Printf("FATAL: bad UUID given: <%s>\n", addr)
			panic(err)
		}

		cp := bluetooth.ConnectionParams{}
		for {
			// TODO: bluetooth.Address bit is not cross-platform.
			device, err := adapter.Connect(bluetooth.Address{uuid}, cp)
			if err != nil {
				fmt.Printf("WARN: connect to <%s> failed: %+v\n", addr, err)
				continue
			}
			deviceChan <- device
			wg.Done()
			break
		}
	}

	if *flagHeartRateAddr != "" {
		wg.Add(1)
		go connectRetry(*flagHeartRateAddr)
	}
	if *flagCyclingPowerAddr != "" {
		wg.Add(1)
		go connectRetry(*flagCyclingPowerAddr)
	}
	if *flagCyclingSpeedCadenceAddr != "" {
		wg.Add(1)
		go connectRetry(*flagCyclingPowerAddr)
	}

	go func() {
		wg.Wait()
		close(deviceChan)
	}()

	for device := range deviceChan {
		println("discovering device services")
		services, err := device.DiscoverServices(KnownServiceUUIDs)
		if err != nil {
			panic(err)
		}

		for _, service := range services {
			println("found service", service.UUID().String())
			chars, err := service.DiscoverCharacteristics([]bluetooth.UUID{})
			if err != nil {
				panic(err)
			}

			for _, char := range chars {
				println("found characteristic", char.UUID().String())

				char.EnableNotifications(func(buf []byte) {
					println(char.UUID().String(), "data:", uint8(buf[0]))
				})
			}
		}
	}

	println("that's all!")
	select {}
}
