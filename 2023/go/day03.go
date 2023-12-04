package main

import (
	"fmt"
	"strings"
	"unicode"
)

type Number struct {
	value int
	neighbors map[rune]bool
}

type Coord struct {
	row int
	col int
}


func is_symbol(r rune) bool {
	return !unicode.IsDigit(r) && r != '.'
}

// First return value is a "set" of symbols
// Second return value is a "set" of engine locations
func get_symbols(row, col int,  grid []string) (map[rune]bool, map[Coord]bool) {
	n_row := len(grid)
	n_col := len(grid[0])

	res := make(map[rune]bool)
	engines := make(map[Coord]bool)
	for _, row_delta := range []int{-1, 0, 1} {
		new_row := row + row_delta
		for _, col_delta := range []int{-1, 0, 1} {
			if row_delta == 0 && col_delta == 0 {
				continue
			}

			new_col := col + col_delta
			if 0 <= new_row && new_row < n_row && 0 <= new_col && new_col < n_col && is_symbol(rune(grid[new_row][new_col])) {
				c := rune(grid[new_row][new_col])
				res[c] = true
				if c == '*' {
					engines[Coord{
						row: new_row,
						col: new_col,
					}] = true
				}
			}
		}
	}

	return res, engines
}

// First return value is a list of numbers with neighboring symbols
// Second return value is a map from engine coordiantes to neighboring
// numbers
func day03_parse(input string) ([]Number, map[Coord][]Number) {
	res := make([]Number, 0)
	engine_dict := make(map[Coord][]Number)
	grid := strings.Split(strings.TrimSpace(input), "\n")
	for row, line := range grid {
		value := 0
		is_number := false
		neighbors := make(map[rune]bool)
		engines := make(map[Coord]bool)
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}

		for col, c := range line {
			if unicode.IsDigit(c) {
				value = value * 10 + int(c - '0')
				is_number = true
				symbols, new_engines := get_symbols(row, col, grid)
				neighbors = union(neighbors, symbols)
				engines = union(engines, new_engines)
				continue
			}

			if is_number {
				number := Number{
					value: value,
					neighbors: neighbors,
				}
				res = append(res, number)
				for coord := range engines {
					engine_dict[coord] = append(engine_dict[coord], number)
				}
			}

			is_number = false
			neighbors = make(map[rune]bool)
			value = 0
			engines = make(map[Coord]bool)
		}

		if is_number {
			number := Number{
				value: value,
				neighbors: neighbors,
			}
			res = append(res, number)
			for coord := range engines {
				engine_dict[coord] = append(engine_dict[coord], number)
			}
		}

	}

	return res, engine_dict
}

// Sum all numbers with at least one neighboring symbol
func day03_task01(numbers []Number) int {
	res := 0
	for _, number := range numbers {
		if len(number.neighbors) > 0 {
			res += number.value
		}

	}
	return res
}

// For all engines with exactly two neighboring numbers
// sum the product of the two numbers
func day03_task02(engines map[Coord][]Number) int {
	res := 0
	for _, numbers := range engines {
		if len(numbers) != 2 {
			continue
		}

		res += numbers[0].value * numbers[1].value
	}

	return res
}

func day03_solution(input string) {
	numbers, engines := day03_parse(input)

	fmt.Printf("Task 01: %d\n", day03_task01(numbers))
	fmt.Printf("Task 02: %d\n", day03_task02(engines))
}
