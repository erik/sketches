package main

import (
	"io"
	"log"
	"math"
	"os"
	"time"

	"github.com/muktihari/fit/encoder"
	"github.com/muktihari/fit/kit/bufferedwriter"
	"github.com/muktihari/fit/kit/scaleoffset"
	"github.com/muktihari/fit/kit/semicircles"
	"github.com/muktihari/fit/profile/filedef"
	"github.com/muktihari/fit/profile/mesgdef"
	"github.com/muktihari/fit/profile/typedef"
	"github.com/muktihari/fit/proto"
	geom "github.com/twpayne/go-geom"
	"github.com/twpayne/go-geom/encoding/geojson"
)

func readFeatureCollection() (*geojson.FeatureCollection, error) {
	bytes, err := io.ReadAll(os.Stdin)
	if err != nil {
		return nil, err
	}

	var collection geojson.FeatureCollection
	err = (&collection).UnmarshalJSON(bytes)
	if err != nil {
		return nil, err
	}

	return &collection, nil
}

func writeFIT(fit proto.FIT) error {
	writer := bufferedwriter.New(os.Stdout)
	defer writer.Flush()

	enc := encoder.New(writer)
	return enc.Encode(&fit)
}

type TrackPoint struct {
	Lat     float64
	Lng     float64
	Ele     *float64
	AccDist float64
	Time    time.Time
}

// haversin(θ) function
func hsin(theta float64) float64 {
	return math.Pow(math.Sin(theta/2), 2)
}

// Distance function returns the distance (in meters) between two points of
//
//	a given longitude and latitude relatively accurately (using a spherical
//	approximation of the Earth) through the Haversin Distance Formula for
//	great arc distance on a sphere with accuracy for small distances
//
// point coordinates are supplied in degrees and converted into rad. in the func
//
// distance returned is METERS!!!!!!
// http://en.wikipedia.org/wiki/Haversine_formula
func Distance(lat1, lon1, lat2, lon2 float64) float64 {
	// convert to radians
	// must cast radius as float to multiply later
	var la1, lo1, la2, lo2, r float64
	la1 = lat1 * math.Pi / 180
	lo1 = lon1 * math.Pi / 180
	la2 = lat2 * math.Pi / 180
	lo2 = lon2 * math.Pi / 180

	r = 6378100 // Earth radius in METERS

	// calculate
	h := hsin(la2-la1) + math.Cos(la1)*math.Cos(la2)*hsin(lo2-lo1)

	return 2 * r * math.Asin(math.Sqrt(h))
}

func mapPoints(feat *geojson.Feature) []TrackPoint {
	// TODO: error handling
	lineString := feat.Geometry.(*geom.LineString)
	zIndex := lineString.Layout().ZIndex()

	points := make([]TrackPoint, len(lineString.Coords()))

	coordProps := feat.Properties["coordinateProperties"].(map[string]interface{})
	pointTimes := coordProps["times"].([]interface{})

	prev := lineString.Coord(0)
	accDist := 0.0

	for i, point := range lineString.Coords() {
		var ele *float64 = nil
		if zIndex != -1 {
			ele = &point[zIndex]
		}

		accDist += Distance(
			prev.Y(),
			prev.X(),
			point.Y(),
			point.X(),
		)

		points[i] = TrackPoint{
			Lat:     point.Y(),
			Lng:     point.X(),
			Ele:     ele,
			AccDist: accDist,
			Time:    timestamp(pointTimes[i].(string)),
		}

		prev = point
	}

	return points
}

func timestamp(s string) time.Time {
	layout := "2006-01-02T15:04:05.000Z"
	t, err := time.Parse(layout, s)
	if err != nil {
		return time.Now()
	}
	return t
}

func main() {
	collection, err := readFeatureCollection()
	if err != nil {
		log.Fatalf("failed to parse: %+v\n", err)
	}
	feature := collection.Features[0]
	coursePoints := mapPoints(feature)
	first, last := coursePoints[0], coursePoints[len(coursePoints)-1]

	course := filedef.NewCourse()

	course.FileId = *mesgdef.NewFileId(nil).
		SetType(typedef.FileCourse).
		SetTimeCreated(first.Time).
		SetManufacturer(typedef.ManufacturerDevelopment).
		SetProduct(0).
		SetProductName("Route Mirror")

	name, ok := feature.Properties["name"].(string)
	if !ok {
		name = "Untitled Route"
	}
	course.Course = mesgdef.NewCourse(nil).
		SetName(name).
		SetCapabilities(typedef.CourseCapabilitiesPosition)

	course.Lap = mesgdef.NewLap(nil).
		SetStartTime(first.Time).
		SetTimestamp(first.Time).
		SetStartPositionLat(semicircles.ToSemicircles(first.Lat)).
		SetStartPositionLong(semicircles.ToSemicircles(first.Lng)).
		SetEndPositionLat(semicircles.ToSemicircles(last.Lat)).
		SetEndPositionLong(semicircles.ToSemicircles(last.Lng)).
		SetTotalDistance(uint32(scaleoffset.Discard(last.AccDist, 100, 0)))

	course.Events = append(course.Events,
		mesgdef.NewEvent(nil).
			SetTimestamp(first.Time).
			SetEvent(typedef.EventTimer).
			SetEventType(typedef.EventTypeStart),
	)

	for _, pt := range coursePoints {
		record :=
			mesgdef.NewRecord(nil).
				SetPositionLat(semicircles.ToSemicircles(pt.Lat)).
				SetPositionLong(semicircles.ToSemicircles(pt.Lng)).
				SetTimestamp(pt.Time).
				SetDistance(uint32(scaleoffset.Discard(pt.AccDist, 100, 0)))

		if pt.Ele != nil {
			record.SetAltitude(
				uint16(scaleoffset.Discard(*pt.Ele, 5, 500)),
			)
		}

		course.Records = append(course.Records, record)
	}

	// Convert back to FIT protocol messages
	fit := course.ToFIT(nil)
	writeFIT(fit)
}
