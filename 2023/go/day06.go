package main

import (
	"errors"
	"fmt"
	"math"
	"strconv"
	"strings"
	"time"
)

type Race struct {
	time, record int
}

func winning_times(race Race) int {
	root_discriminant := math.Sqrt(math.Pow(float64(race.time), 2) - 4*float64(race.record))
	t_hi := (float64(race.time) + root_discriminant) / 2
	t_lo := (float64(race.time) - root_discriminant) / 2

	if float64(int(t_hi)) == t_hi {
		t_hi -=1
	}

	if float64(int(t_lo)) == t_lo {
		t_lo += 1
	}

	return int(math.Floor(t_hi)) - int(math.Ceil(t_lo)) + 1
}

func day06_task01(input string) (int, error) {
	lines := strings.Split(input, "\n")
	times := strings.Fields(strings.TrimSpace(strings.Split(lines[0], ":")[1]))
	records := strings.Fields(strings.TrimSpace(strings.Split(lines[1], ":")[1]))

	if len(times) != len(records) {
		return -1, errors.New("parsing went wrong")
	}

	res := 1
	for i := range times {
		time, err := strconv.Atoi(times[i])
		if err != nil {
			return -1, err
		}
		record, err := strconv.Atoi(records[i])
		if err != nil {
			return -1, err
		}

		res *= winning_times(Race{
			time: time,
			record: record,
		})
	}
	return res, nil
}

func day06_task02(input string) (int, error) {
	lines := strings.Split(input, "\n")
	time, err := strconv.Atoi(strings.ReplaceAll(strings.Split(lines[0], ":")[1], " ", ""))
	if err != nil {
		return -1, err
	}
	record, err := strconv.Atoi(strings.ReplaceAll(strings.Split(lines[1], ":")[1], " ", ""))
	if err != nil {
		return -1, err
	}

	return winning_times(Race{
		time: time,
		record: record,
	}), nil
}

func day06_solution(input string) {
	input = strings.TrimSpace(input)

	start := time.Now()
	res, err := day06_task01(input)
	elapsed := time.Since(start)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Task 01: %d\n", res)
	fmt.Printf("Task 01 time: %s\n", elapsed)

	start = time.Now()
	res, err = day06_task02(input)
	elapsed = time.Since(start)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Task 02: %d\n", res)
	fmt.Printf("Task 02 time: %s\n", elapsed)
}
