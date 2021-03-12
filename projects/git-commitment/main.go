package main

import (
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

func scanDevices() error {
	return nil
}

func main() {
	addrCh := make(chan bluetooth.Addresser)

	adapter := bluetooth.DefaultAdapter

	println("\n\nstarting up")
	adapter.Enable()

	if err := adapter.Scan(func(bt *bluetooth.Adapter, scan bluetooth.ScanResult) {
		services := []bluetooth.UUID{}
		for _, s := range KnownServiceUUIDs {
			if scan.HasServiceUUID(s) {
				services = append(services, s)
			}
		}

		// No matching services, skip this device.
		if len(services) == 0 {
			return
		}

		// NOTE: need to stop scan before we can attempt connection
		if err := bt.StopScan(); err != nil {
			panic(err)
		}

		println("found",
			scan.Address.String(),
			scan.RSSI,
			scan.LocalName(),
		)

		addrCh <- scan.Address
	}); err != nil {
		panic(err)
	}

	println("scan complete")

	var device *bluetooth.Device
	select {
	case addr := <-addrCh:
		println("attempting connection")
		if d, err := adapter.Connect(addr, bluetooth.ConnectionParams{}); err != nil {
			panic(err)
		} else {
			device = d
		}
		println("connection success!")
	}

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

	println("that's all!")
	select {}
}
