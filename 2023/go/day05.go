package main

import "errors"

type Range struct {
	start, length int
}

type Map struct {
	source, destination, length int
}

// maps need to be sorted by source
func (r Range) transform(maps []Map) ([]Range, error) {
	start := r.start
	length := r.length

	if len(maps) == 0 {
		return nil, errors.New("need at least one map")
	}

	i := 0
	res := make([]Range, 0)
	var new_length, destination int
	for length > 0 {
		m := maps[i]
		if start < m.source {
			new_length = min(length, m.source - start)
			destination = start
		}

		res = append(res, Range{
			start: destination,
			length: new_length,
		})
		start += new_length
		length -= new_length
	}

	return res, nil
}

func day05_solution(input string) {

}
