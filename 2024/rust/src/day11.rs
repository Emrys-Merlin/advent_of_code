use std::collections::HashMap;

fn parse_input(input: &str) -> Vec<usize> {
    let line = input.lines().next().unwrap();

    line.split(' ')
        .map(|number| number.parse::<usize>().unwrap())
        .collect()
}

fn brute_force(input: &str, limit: usize) -> String {
    let mut stones = parse_input(input);

    for _ in 0..limit {
        let mut new_stones = Vec::new();
        for stone in stones {
            if stone == 0 {
                new_stones.push(1);
                continue;
            }

            let str_stone = stone.to_string();

            if str_stone.len() % 2 == 0 {
                let (left, right) = str_stone.split_at(str_stone.len() / 2);
                new_stones.push(left.parse::<usize>().unwrap());
                new_stones.push(right.parse::<usize>().unwrap());
                continue;
            }

            new_stones.push(stone * 2024)
        }

        stones = new_stones;
    }
    stones.len().to_string()
}

fn hashmap_based(input: &str, limit: usize) -> String {
    let vec_stones = parse_input(input);

    let mut stones: HashMap<usize, usize> =
        HashMap::from_iter(vec_stones.iter().map(|&stone| (stone, 1)));

    for _ in 0..limit {
        let mut new_stones = HashMap::new();
        for (&stone, &count) in stones.iter() {
            if stone == 0 {
                *new_stones.entry(1).or_insert(0) += count;
                continue;
            }

            let str_stone = stone.to_string();

            if str_stone.len() % 2 == 0 {
                let (left, right) = str_stone.split_at(str_stone.len() / 2);
                let left = left.parse::<usize>().unwrap();
                let right = right.parse::<usize>().unwrap();
                *new_stones.entry(left).or_insert(0) += count;
                *new_stones.entry(right).or_insert(0) += count;
                continue;
            }

            *new_stones.entry(stone * 2024).or_insert(0) += count;
        }

        stones = new_stones;
    }

    stones.values().sum::<usize>().to_string()
}

pub fn task01(input: &str) -> String {
    brute_force(input, 25)
}
pub fn task02(input: &str) -> String {
    hashmap_based(input, 75)
}
