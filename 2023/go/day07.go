package main

import (
	"cmp"
	"fmt"
	"math"
	"slices"
	"strconv"
	"strings"
	"time"
)

type Hand struct {
	hand string
	bid, hand_value int
	card_value float64
}

func hand_value(hand string, with_jokers bool) int {
	counter := make(map[rune]int)
	for _, card := range hand {
		counter[card]++
	}

	n_jokers := 0
	if with_jokers {
		n_jokers = counter['J']
	}

	card_occurences := []int{0, 0}
	for card, count := range counter {
		if with_jokers && card == 'J' {
			continue
		}
		card_occurences = append(card_occurences, count)
	}

	slices.SortFunc(card_occurences, func(i, j int) int {
		return -cmp.Compare(i, j)
	})

	if card_occurences[0] + n_jokers >= 5 {
		return 6
	}
	if card_occurences[0] + n_jokers >= 4 {
		return 5
	}
	if card_occurences[0] + n_jokers >= 3 {
		rem := 3 - card_occurences[0] - n_jokers
		if card_occurences[1] + rem >= 2 {
			return 4
		}
		return 3
	}
	if card_occurences[0] + n_jokers >= 2 {
		rem := 2 - card_occurences[0] - n_jokers
		if card_occurences[1] + rem >= 2 {
			return 2
		}
		return 1
	}
	return 0
}

// Encode card value in "base 1/13" (i.e. 13 cards)
// E.g. assuming no jokers "T24" would translate to
// 8 * 1/13 + 0 * 1/13^2 + 2* 1/13^3
// 2 = 0th card, T = 8th card, 4 = 2nd card
func card_value(hand string, with_jokers bool) float64 {
	var card_order string
	if with_jokers {
		card_order = "J23456789TQKA"
	} else {
		card_order = "23456789TJQKA"
	}

	card_map := make(map[rune]float64)
	for i, card := range card_order {
		card_map[card] = float64(i)
	}

	base := 1. / float64(len(card_order))
	res := 0.
	for i, card := range hand {
		res += card_map[card] * math.Pow(base, float64(i+1))
	}

	return res
}

func build_hand(hand string, bid int, with_jokers bool) Hand {
	return Hand{
		bid: bid,
		hand_value: hand_value(hand, with_jokers),
		card_value: card_value(hand, with_jokers),
		hand: hand,
	}
}

func compare_hands(h1, h2 Hand) int {
	if h1.hand_value == h2.hand_value {
		return cmp.Compare(h1.card_value, h2.card_value)
	}
	return cmp.Compare(h1.hand_value, h2.hand_value)
}

func day07_parsers(input string, with_jokers bool) int {
	hands := []Hand{}
	for _, line := range strings.Split(input, "\n") {
		fields := strings.Fields(line)
		hand := fields[0]
		bid, err := strconv.Atoi(fields[1])
		if err != nil {
			panic(err)
		}
		hands = append(hands, build_hand(hand, bid, with_jokers))
	}
	slices.SortFunc(hands, compare_hands)

	res := 0
	for i, hand := range hands {
		res += (i+1) * hand.bid
	}
	return res
}


func day07_solution(input string) {
	input = strings.TrimSpace(input)

	start := time.Now()
	fmt.Printf("Task 01: %d\n", day07_parsers(input, false))
	elapsed := time.Since(start)
	fmt.Printf("Time: %s\n", elapsed)

	start = time.Now()
	fmt.Printf("Task 02: %d\n", day07_parsers(input, true))
	elapsed = time.Since(start)
	fmt.Printf("Time: %s\n", elapsed)
}
