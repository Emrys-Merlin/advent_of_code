package main

import (
	"os"
	"strconv"

	"github.com/urfave/cli/v2"
)


var solutions = [...]func(string){
	day01_solution,
	day02_solution,
	day03_solution,
	day04_solution,
	day05_solution,
	day06_solution,
	day07_solution,
}


func entrypoint(c *cli.Context) error {
	day := c.Args().Get(0)
	path := c.Args().Get(1)

	id, err := strconv.Atoi(day)
	id -= 1

	if err != nil {
		panic(err)
	}

	solution := solutions[id]

	input, err := os.ReadFile(path)

	if err != nil {
		panic(err)
	}

	solution(string(input))

	return nil
}

func main() {
	app := &cli.App{
		Name: "aoc2023",
		Description: "Advent of Code 2023",
		Action: entrypoint,
	}

	if err := app.Run(os.Args); err != nil {
		panic(err)
	}
}
