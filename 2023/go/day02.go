package main

import (
	"fmt"
	"strconv"
	"strings"
)


type MaxDraw struct {
	red int
	green int
	blue int
	id int
}

func admissible(draw, reference MaxDraw) bool {
	return draw.red <= reference.red && draw.green <= reference.green && draw.blue <= reference.blue
}

func power(draw MaxDraw) int {
	return draw.red * draw.green * draw.blue
}

func split(r rune) bool {
	return r == ',' || r == ';'
}

func parse_line(line string) MaxDraw {
	res := strings.Split(line, ":")
	game := res[0]
	id, err := strconv.Atoi(strings.Split(strings.TrimSpace(game), " ")[1])

	if err != nil {
		panic(err)
	}

	rem := res[1]
	counts := make(map[string]int)
	for _, cubes := range strings.FieldsFunc(rem, split) {
		res = strings.Split(strings.TrimSpace(cubes), " ")
		color := res[1]
		count, err := strconv.Atoi(res[0])

		if err != nil {
			panic(err)
		}
		counts[color] = max(counts[color], count)
	}

	return MaxDraw{
		red: counts["red"],
		green: counts["green"],
		blue: counts["blue"],
		id: id,
	}
}

func day02_task01(input string) int {
	reference := MaxDraw{
		red: 12,
		green: 13,
		blue: 14,
		id: 0,
	}

	res := 0
	for _, line := range strings.Split(input, "\n") {
		if len(strings.TrimSpace(line)) == 0 {
			continue
		}
		draw := parse_line(line)
		if admissible(draw, reference) {
			res += draw.id
		}
	}
	return res
}

func day02_task02(input string) int {
	res := 0
	for _, line := range strings.Split(input, "\n") {
		if len(strings.TrimSpace(line)) == 0 {
			continue
		}

		res += power(parse_line(line))
	}

	return res
}

func day02_solution(input string) {
	fmt.Printf("Task 01: %d\n", day02_task01(string(input)))
	fmt.Printf("Task 02: %d\n", day02_task02(string(input)))
}
