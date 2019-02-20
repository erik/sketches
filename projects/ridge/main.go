package main

import (
	"fmt"
	"image"
	_ "image/gif"
	"io"
	"log"
	"net/http"
	"sync"
	"time"
)

// TODO: hardcoding this feels so gross.
// curl -s https://radar.weather.gov/ridge/RadarImg/N0R/ | egrep '\w+N0R_0.gif' -o | sort -u | cut -d'_' -f1
var STATIONS = []string{
	"ABC", "ABR", "ABX", "ACG", "AEC", "AHG", "AIH", "AKC", "AKQ",
	"AMA", "AMX", "APD", "APX", "ARX", "ATX", "BBX", "BGM", "BHX",
	"BIS", "BLX", "BMX", "BOX", "BRO", "BUF", "BYX", "CAE", "CBW",
	"CBX", "CCX", "CLE", "CLX", "CRP", "CXX", "CYS", "DAX", "DDC",
	"DFX", "DGX", "DIX", "DLH", "DMX", "DOX", "DTX", "DVN", "DYX",
	"EAX", "EMX", "ENX", "EOX", "EPZ", "ESX", "EVX", "EWX", "EYX",
	"FCX", "FDR", "FDX", "FFC", "FSD", "FSX", "FTG", "FWS", "GGW",
	"GJX", "GLD", "GRB", "GRK", "GRR", "GSP", "GUA", "GWX", "GYX",
	"HDX", "HGX", "HKI", "HKM", "HMO", "HNX", "HPX", "HTX", "HWA",
	"ICT", "ICX", "ILN", "ILX", "IND", "INX", "IWA", "IWX", "JAX",
	"JBQ", "JGX", "JKL", "JRV", "JUA", "KJK", "KSG", "LBB", "LCH",
	"LGX", "LIX", "LNX", "LOT", "LRX", "LSX", "LTX", "LVX", "LWX",
	"LZK", "MAF", "MAX", "MBX", "MHX", "MKX", "MLB", "MOB", "MPX",
	"MQT", "MRX", "MSX", "MTX", "MUX", "MVX", "MXX", "NKX", "NQA",
	"OAX", "ODN", "OHX", "OKX", "OTX", "PAH", "PBZ", "PDT", "POE",
	"PUX", "QYA", "RAX", "RGX", "RIW", "RLX", "RTX", "SFX", "SGF",
	"SHV", "SJT", "SOX", "SRX", "TBW", "TFX", "TLH", "TLX", "TWX",
	"TYX", "UDX", "UEX", "VAX", "VBX", "VNX", "VTX", "VWX", "YUX",
}

const UpdateInterval = 3 * time.Minute
const BaseImgURL = "https://radar.weather.gov/ridge/RadarImg/N0R/"

func updateStationImage(callsign, lastModified string) (io.ReadCloser, string) {
	URL := fmt.Sprintf("%s%s_N0R_0.gif", BaseImgURL, callsign)

	resp, err := http.Get(URL)
	if err != nil {
		log.Printf("err: failed to update %s: %+v\n", callsign, err)
		return nil, lastModified
	}

	// Don't process the same file twice.
	modified, ok := resp.Header["Last-Modified"]
	if ok && modified[0] == lastModified {
		log.Printf("info: %s unmodified since %s", callsign, lastModified)
		return nil, lastModified
	}

	return resp.Body, modified[0]
}

// RGB color value -> decibel
var ReflectivityGradient = map[uint32]int8{
	0xFFFFFF: 75,
	0x996699: 70,
	0xF800FD: 65,
	0xBC0000: 60,
	0xD40000: 55,
	0xFE0000: 50,
	0xFD9500: 45,
	0xE5BC00: 40,
	0xFDF802: 35,
	0x008E00: 30,
	0x01C501: 25,
	0x02FD02: 20,
	0x0300F4: 15,
	0x019FF4: 10,
	0x04E9E7: 5,
	0x646464: 0,
	0x999966: -5,
	0xCCCC99: -10,
	0x663366: -15,
	0x9854C6: -20,
	0xCC99CC: -25,
	0xCCFFFF: -30,
}

const (
	CHAR_WIDTH   = 80
	CHAR_HEIGHT  = 24
	PIXEL_WIDTH  = 80 * 2
	PIXEL_HEIGHT = 24 * 4
)

func readRidgeImage(reader io.ReadCloser) {
	defer reader.Close()
	img, _, err := image.Decode(reader)
	if err != nil {
		log.Printf("err: failed to decode image: %+v\n", err)
		return
	}

	reflMap := make([]uint32, PIXEL_WIDTH*PIXEL_HEIGHT)
	valMap := make([]uint32, CHAR_WIDTH*CHAR_HEIGHT)

	hit, miss := 0, 0
	bounds := img.Bounds()
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			r, g, b, _ := img.At(x, y).RGBA()
			rgb := ((r & 0x0ff) << 16) |
				((g & 0x0ff) << 8) |
				(b & 0x0ff)

			if _, ok := ReflectivityGradient[rgb]; ok {
				hit++
			} else {
				miss++
			}

		}
	}

	log.Printf("info: hit: %d miss: %d\n", hit, miss)
}

func registerStation(callsign string) {
	ticker := time.NewTicker(UpdateInterval)
	go func() {
		var imageReader io.ReadCloser
		modifiedAt := ""

		// Hack for immediate first ticker execution
		for ; true; <-ticker.C {
			start := time.Now()

			imageReader, modifiedAt = updateStationImage(callsign, modifiedAt)
			if imageReader != nil {
				readRidgeImage(imageReader)
			}

			duration := int64(time.Now().Sub(start) / time.Millisecond)
			fmt.Printf("Tick: %s [%d ms]\n", callsign, duration)
		}
	}()
}

func registerStations(stations []string) {
	for _, callsign := range stations {
		registerStation(callsign)
	}
}

func main() {
	var wg sync.WaitGroup

	// 1. Spawn schedulers for consuming RIDGE data
	registerStations(STATIONS)

	wg.Add(1)
	wg.Wait()
}
