package main

import (
	"fmt"
	"os"
	"strings"
	"unicode"

	"github.com/urfave/cli/v2"
)

func calibration(line string) int {
	first := 0
	last := 0

	for _, c := range line {
		if unicode.IsDigit(c) {
			last = int(c - '0')
			if first == 0 {
				first = last
			}
		}
	}
	return 10 * first + last
}

func replaceLetterNumbers(line string) string {
	numbers := [...]string{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}
	var res []rune
	for i, c := range line {
		if unicode.IsDigit(c) {
			res = append(res, c)
		} else {
			for j, number := range numbers {
				if strings.HasPrefix(line[i:], number) {
					res = append(res, rune(j + 1) + '0')
				}
			}
		}
	}

	return string(res)
}

func task01(input string) int {
	res := 0
	for _, line := range strings.Split(input, "\n") {
		res += calibration(line)
	}

	return res
}

func task02(input string) int {
	res := 0
	for _, line := range strings.Split(input, "\n") {
		res += calibration(replaceLetterNumbers(line))
	}

	return res
}

func entrypoint(c *cli.Context) error {
	path := c.Args().First()
	input, err := os.ReadFile(path)

	if err != nil {
		panic(err)
	}

	fmt.Printf("Task 01: %d\n", task01(string(input)))
	fmt.Printf("Task 02: %d\n", task02(string(input)))

	return nil
}

func main() {
	app := &cli.App{
		Name: "day01",
		Description: "Advent of Code 2023 - Day 01",
		Action: entrypoint,
	}

	if err := app.Run(os.Args); err != nil {
		panic(err)
	}
}
