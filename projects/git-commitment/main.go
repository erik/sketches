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
	// https://www.bluetooth.com/specifications/specs/cycling-power-service-1-1/
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
var (
	KnownServiceNames = map[bluetooth.UUID]string{
		bluetooth.ServiceUUIDCyclingSpeedAndCadence: "Cycling Speed and Cadence",
		bluetooth.ServiceUUIDCyclingPower:           "Cycling Power",
		bluetooth.ServiceUUIDHeartRate:              "Heart Rate",
	}
	KnownCharacteristicNames = map[bluetooth.UUID]string{
		bluetooth.CharacteristicUUIDCyclingPowerMeasurement: "Cycling Power Measure",
		bluetooth.CharacteristicUUIDCyclingPowerFeature:     "Cycling Power Feature",
		bluetooth.CharacteristicUUIDHeartRateMeasurement:    "Heart Rate Measurement",
	}
)

type MetricKind int

const (
	MetricHeartRate = iota
	MetricCyclingPower
	MetricCyclingSpeed
	MetricCyclingCadence
)

type DeviceMetric struct {
	kind  MetricKind
	value int
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
	if name, ok := KnownCharacteristicNames[src.ch.UUID()]; ok {
		return name
	}
	return fmt.Sprintf("<unknown: %s>", src.ch.UUID().String())
}

func (src *MetricSource) AddSink(sink chan DeviceMetric) {
	src.sinks = append(src.sinks, sink)

	// Start listenening first time we add a sink
	if len(src.sinks) == 1 {
		handler := src.notificationHandler()
		src.ch.EnableNotifications(handler)
	}
}

func (src *MetricSource) notificationHandler() func([]byte) {
	switch src.ch.UUID() {
	case bluetooth.CharacteristicUUIDCyclingPowerMeasurement:
		return src.handleCyclingPowerMeasurement

	// TODO
	case bluetooth.CharacteristicUUIDCyclingPowerFeature:
	case bluetooth.CharacteristicUUIDHeartRateMeasurement:
		return src.handleHeartRateMeasurement
	}

	return nil
}

func (src *MetricSource) emit(m DeviceMetric) {
	for _, sink := range src.sinks {
		sink <- m
	}
}

const (
	// BPM size, 0 if u8, 1 if u16
	HeartRateFlagSize = 1 << 0

	// 00 unsupported
	// 01 unsupported
	// 10 supported, not detected
	// 11 supported, detected
	HeartRateFlagContactStatus = (1 << 1) | (1 << 2)

	HeartRateFlagHasEnergyExpended = 1 << 3
	HeartRateFlagHasRRInterval     = 1 << 4

	// bits 5-8 reserved
)

func (src *MetricSource) handleHeartRateMeasurement(buf []byte) {
	// malformed
	if len(buf) < 2 {
		return
	}

	flag := buf[0]

	is16Bit := (flag & HeartRateFlagSize) != 0
	contactStatus := (flag & HeartRateFlagContactStatus) >> 1

	contactSupported := contactStatus&(0b10) != 0
	contactFound := contactStatus&(0b01) != 0

	// No use sending this metric if the sensor isn't reading.
	if contactSupported && !contactFound {
		return
	}

	var hr int = int(buf[1])
	if is16Bit {
		hr = (hr << 8) | int(buf[2])
	}

	src.emit(DeviceMetric{
		kind:  MetricHeartRate,
		value: hr,
	})
}

const (
	CyclingPowerFlagHasPedalPowerBalance       = 1 << 0
	CyclingPowerFlagPedalPowerBalanceReference = 1 << 1
	CyclingPowerFlagHasAccumulatedTorque       = 1 << 2
	CyclingPowerFlagAccumulatedTorqueSource    = 1 << 3
	CyclingPowerFlagHasWheelRevolution         = 1 << 4
	CyclingPowerFlagHasCrankRevolution         = 1 << 5
	CyclingPowerFlagHasExtremeForceMagnitudes  = 1 << 6
	CyclingPowerFlagHasExtremeTorqueMagnitudes = 1 << 7
)

// Packet is [FLAG BYTE] [POWER WATTS]
func (src *MetricSource) handleCyclingPowerMeasurement(buf []byte) {
	// fmt.Printf("%s: got %+v\n", src.Name(), buf)
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
			break
		}

		wg.Done()
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
		go connectRetry(*flagCyclingSpeedCadenceAddr)
	}

	go func() {
		wg.Wait()
		close(deviceChan)
	}()

	metricsChan := make(chan DeviceMetric)
	go func() {
		for m := range metricsChan {
			fmt.Printf("Metric: %+v\n", m)
		}
	}()

	for device := range deviceChan {
		fmt.Println("Initializing device...")
		services, err := device.DiscoverServices(KnownServiceUUIDs)
		if err != nil {
			panic(err)
		}

		for _, service := range services {
			if name, ok := KnownServiceNames[service.UUID()]; ok {
				fmt.Printf("\tservice: %s\n", name)
			} else {
				fmt.Printf("\tservice: unknown <%+v>\n", service.UUID().String())
			}

			knownChars := KnownServiceCharacteristicUUIDs[service.UUID()]
			chars, err := service.DiscoverCharacteristics(knownChars)
			if err != nil {
				panic(err)
			}

			for _, char := range chars {
				name := KnownCharacteristicNames[char.UUID()]
				fmt.Printf("\t\tcharacteristic: %s\n", name)

				src := NewMetricSource(&service, &char)
				src.AddSink(metricsChan)
			}
		}
	}

	println("that's all!")
	select {}
}
