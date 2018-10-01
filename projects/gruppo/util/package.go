package util

import (
	"strings"
	"time"
)

type JSONTime struct{ Time time.Time }

func (j JSONTime) MarshalJSON() ([]byte, error) {
	t := j.Time.Format(time.RFC3339)
	return []byte("\"" + t + "\""), nil
}

func (j *JSONTime) UnmarshalJSON(p []byte) error {
	t, err := time.Parse(
		time.RFC3339,
		strings.Replace(string(p), "\"", "", -1))

	if err != nil {
		return err
	}

	*j = JSONTime{t}

	return nil
}
