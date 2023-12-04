package main

import (
	"fmt"
	"strconv"
	"strings"
	"time"
)

type Scratchcard struct {
	id int
	reference map[int]bool
	obtained map[int]bool
}

func points(scratchcard Scratchcard) int {
	n_winning := len(intersection(scratchcard.reference, scratchcard.obtained))

	if n_winning == 0 {
		return 0
	}

	res := 1

	for i := 0; i < n_winning - 1; i++ {
		res *= 2
	}

	return res
}

func parse_scratchnumbers(numbers string) map[int]bool {
	res := make(map[int]bool)
	numbers = strings.TrimSpace(numbers)
	for _, str_number := range strings.Split(numbers, " ") {
		if len(str_number) == 0 {
			continue
		}
		number, err := strconv.Atoi(str_number)

		if err != nil {
			panic(err)
		}
		res[number] = true
	}

	return res
}

func parse_scratchcards(input string) []Scratchcard {
	res := make([]Scratchcard, 0)
	for _, line := range strings.Split(input, "\n") {
		parts := strings.Split(line, ":")

		card_id := strings.Split(parts[0], " ")
		id, err := strconv.Atoi(card_id[len(card_id) - 1])

		if err != nil {
			panic(err)
		}

		parts = strings.Split(parts[1], "|")

		reference := parse_scratchnumbers(parts[0])
		obtained := parse_scratchnumbers(parts[1])

		res = append(res, Scratchcard{
			id: id,
			reference: reference,
			obtained: obtained,
		})
	}

	return res
}

func day04_task01(scratchcards []Scratchcard) int {
	res := 0
	for _, scratchcard := range scratchcards {
		res += points(scratchcard)
	}

	return res
}

func day04_task02(scratchcards []Scratchcard) int {
	n_cards := make([]int, len(scratchcards))

	for _, scratchcard := range scratchcards {
		n_winning := len(intersection(scratchcard.reference, scratchcard.obtained))
		n_card := max(1, n_cards[scratchcard.id - 1])

		for i := 0; i < n_winning; i++ {
			if scratchcard.id + i >= len(n_cards) {
				break
			}
			n_cards[scratchcard.id + i] = max(1, n_cards[scratchcard.id + i]) + n_card
		}
	}

	res := 0
	for _, n_card := range n_cards {
		res += max(1, n_card)
	}

	return res
}

func day04_solution(input string) {
	input = strings.TrimSpace(input)

	start := time.Now()
	scratchcards := parse_scratchcards(input)
	elapsed := time.Since(start)
	fmt.Printf("Parse time: %s\n", elapsed)

	start = time.Now()
	fmt.Printf("Task 01: %d\n", day04_task01(scratchcards))
	elapsed = time.Since(start)
	fmt.Printf("Time: %s\n", elapsed)

	start = time.Now()
	fmt.Printf("Task 02: %d\n", day04_task02(scratchcards))
	elapsed = time.Since(start)
	fmt.Printf("Time: %s\n", elapsed)
}
