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
	// "ABC", "ABR", "ABX", "ACG", "AEC", "AHG", "AIH", "AKC", "AKQ",
	// "AMA", "AMX", "APD", "APX", "ARX", "ATX", "BBX", "BGM", "BHX",
	// "BIS", "BLX", "BMX", "BOX", "BRO", "BUF", "BYX", "CAE", "CBW",
	// "CBX", "CCX", "CLE", "CLX", "CRP", "CXX", "CYS", "DAX", "DDC",
	// "DFX", "DGX", "DIX", "DLH", "DMX", "DOX", "DTX", "DVN", "DYX",
	// "EAX", "EMX", "ENX", "EOX", "EPZ", "ESX", "EVX", "EWX", "EYX",
	// "FCX", "FDR", "FDX", "FFC", "FSD", "FSX", "FTG", "FWS", "GGW",
	// "GJX", "GLD", "GRB", "GRK", "GRR", "GSP", "GUA", "GWX", "GYX",
	// "HDX", "HGX", "HKI", "HKM", "HMO", "HNX", "HPX", "HTX", "HWA",
	// "ICT", "ICX", "ILN", "ILX", "IND", "INX", "IWA", "IWX", "JAX",
	// "JBQ", "JGX", "JKL", "JRV", "JUA", "KJK", "KSG", "LBB", "LCH",
	// "LGX", "LIX", "LNX", "LOT", "LRX", "LSX", "LTX", "LVX", "LWX",
	// "LZK", "MAF", "MAX", "MBX", "MHX", "MKX", "MLB", "MOB", "MPX",
	// "MQT", "MRX", "MSX", "MTX", "MUX", "MVX", "MXX", "NKX", "NQA",
	// "OAX", "ODN", "OHX", "OKX", "OTX", "PAH", "PBZ", "PDT", "POE",
	// "PUX", "QYA", "RAX", "RGX", "RIW", "RLX", "RTX", "SFX", "SGF",
	// "SHV", "SJT", "SOX", "SRX", "TBW", "TFX", "TLH", "TLX", "TWX",
	// "TYX", "UDX", "UEX", "VAX", "VBX", "VNX", "VTX", "VWX", "YUX",
	"VTX",
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

// TODO: fix awful naming
type colorPair struct {
	color     uint32
	decibel   int8
	termColor int
}

// RGB color value -> decibel
var ReflColorToDb = map[uint32]int8{}
var ReflDbToColor = []colorPair{
	{0xFFFFFF, 75, 37},
	{0x996699, 70, 35},
	{0xF800FD, 65, 35},
	{0xBC0000, 60, 31},
	{0xD40000, 55, 31},
	{0xFE0000, 50, 31},
	{0xFD9500, 45, 31},
	{0xE5BC00, 40, 33},
	{0xFDF802, 35, 33},
	{0x008E00, 30, 32},
	{0x01C501, 25, 32},
	{0x02FD02, 20, 32},
	{0x0300F4, 15, 34},
	{0x019FF4, 10, 94},
	{0x04E9E7, 5, 94},
	{0x646464, 0, 37},
	{0x999966, -5, 90},
	{0xCCCC99, -10, 90},
	{0x663366, -15, 95},
	{0x9854C6, -20, 95},
	{0xCC99CC, -25, 95},
	{0xCCFFFF, -30, 96},
}

func init() {
	ReflColorToDb = make(map[uint32]int8, len(ReflDbToColor))
	for _, pair := range ReflDbToColor {
		ReflColorToDb[pair.color] = pair.decibel
	}
}

const (
	PIXELS_PER_CHAR_X = 2
	PIXELS_PER_CHAR_Y = 4

	OUT_CHAR_WIDTH  = 80
	OUT_CHAR_HEIGHT = 24
)

func scaledIdx(x, y, fw, fh, tw, th uint32) (uint32, uint32) {
	scaledX := float32(x) / float32(fw)
	scaledY := float32(y) / float32(fh)

	return uint32(scaledX * float32(tw)),
		uint32(scaledY * float32(th))
}

func getChar(img image.Image, x, y int) ridgePixel {
	var intensity int32 = 0
	var mask uint16 = 0

	// Braille dots are indexed like so:
	// 1 4
	// 2 5
	// 3 6
	// 7 8
	bitMapping := []uint{
		0, 3,
		1, 4,
		2, 5,
		6, 7,
	}

	bounds := img.Bounds()

	for i := 0; i < PIXELS_PER_CHAR_X; i++ {
		px := (x * PIXELS_PER_CHAR_X) + i + bounds.Min.X
		for j := 0; j < PIXELS_PER_CHAR_Y; j++ {
			py := (y * PIXELS_PER_CHAR_Y) + j + bounds.Min.Y

			r, g, b, _ := img.At(px, py).RGBA()
			rgb := ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)

			if pxDbVal, ok := ReflColorToDb[rgb]; ok {
				intensity += int32(pxDbVal)
				pos := bitMapping[j*2+i]
				mask |= (1 << pos)
			}
		}
	}

	avgIntensity := int8(float32(intensity) / (PIXELS_PER_CHAR_X * PIXELS_PER_CHAR_Y))
	color := int(0)

	for _, p := range ReflDbToColor {
		color = p.termColor
		if p.decibel <= avgIntensity {
			break
		}
	}

	return ridgePixel{
		ch:    rune(0x2800 | mask),
		color: color,
	}
}

type ridgePixel struct {
	ch    rune
	color int
}

func readRidgeImage(reader io.ReadCloser) {
	defer reader.Close()
	img, _, err := image.Decode(reader)
	if err != nil {
		log.Printf("err: failed to decode image: %+v\n", err)
		return
	}

	bounds := img.Bounds()

	iw, ih := bounds.Dx(), bounds.Dy()
	ow, oh := iw/PIXELS_PER_CHAR_X, ih/PIXELS_PER_CHAR_Y

	buf := make([][]ridgePixel, oh)
	for y := 0; y < oh; y++ {
		buf[y] = make([]ridgePixel, ow)
		for x := 0; x < ow; x++ {
			buf[y][x] = getChar(img, x, y)
			fmt.Printf("\033[%dm%c", buf[y][x].color, buf[y][x].ch)
		}
		fmt.Println()
	}
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
