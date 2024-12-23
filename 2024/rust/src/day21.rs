use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

impl Point {
    // Only consider sequences with a single turn
    pub fn sequences_to(&self, other: &Point, reverse_row_axis: bool) -> Vec<String> {
        let delta_row = (other.row as isize - self.row as isize) * if reverse_row_axis { -1 } else { 1 };
        let symbol = if delta_row < 0 { "v" } else { "^" };
        let vertical_part = (0..delta_row.abs()).fold("".to_string(), |res, _| res + symbol);

        let delta_col = other.col as isize - self.col as isize;
        let symbol = if delta_col < 0 { "<" } else { ">" };
        let horizontal_part = (0..delta_col.abs()).fold("".to_string(), |res, _| res + symbol);


        if self.col == 0 && other.row == 0 {
            return vec![horizontal_part + &vertical_part + &"A"];
        }

        if self.row == 0 && other.col == 0 {
            return vec![vertical_part + &horizontal_part + &"A"];
        }

        if self.row == other.row  || self.col == other.col {
            return vec![horizontal_part + &vertical_part + &"A"]
        }

        vec![horizontal_part.clone() + &vertical_part + &"A", vertical_part + &horizontal_part + &"A"]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Keypad {
    keys: HashMap<char, Point>,
    reverse_row_axis: bool,
}

impl Keypad {
    pub fn numpad() -> Self {
        let mut keys = HashMap::new();
        keys.insert('0', Point { row: 0, col: 1 });
        keys.insert('A', Point { row: 0, col: 2 });
        keys.insert('1', Point { row: 1, col: 0 });
        keys.insert('2', Point { row: 1, col: 1 });
        keys.insert('3', Point { row: 1, col: 2 });
        keys.insert('4', Point { row: 2, col: 0 });
        keys.insert('5', Point { row: 2, col: 1 });
        keys.insert('6', Point { row: 2, col: 2 });
        keys.insert('7', Point { row: 3, col: 0 });
        keys.insert('8', Point { row: 3, col: 1 });
        keys.insert('9', Point { row: 3, col: 2 });

        Keypad { keys, reverse_row_axis: false }
    }

    pub fn arrowpad() -> Self {
        let mut keys = HashMap::new();
        keys.insert('^', Point { row: 0, col: 1 });
        keys.insert('A', Point { row: 0, col: 2 });
        keys.insert('<', Point { row: 1, col: 0 });
        keys.insert('v', Point { row: 1, col: 1 });
        keys.insert('>', Point { row: 1, col: 2 });


        Keypad { keys, reverse_row_axis: true }
    }

    pub fn generate_sequences(&self) -> HashMap<String, Vec<String>> {
        let mut sequences = HashMap::new();
        for (key, point) in self.keys.iter() {
            for (other_key, other_point) in self.keys.iter() {
                let seqs = point.sequences_to(other_point, self.reverse_row_axis);
                let pair = format!("{}{}", key, other_key);
                sequences.insert(pair, seqs);

            }
        }

        sequences
    }

}

fn count_levels(current_sequence: &String, depth: usize, pair_to_paths_map: &HashMap<String, Vec<String>>, memo: &mut HashMap<(String, usize), usize>) -> usize {
    if depth == 0 {
        return current_sequence.len();
    }

    match memo.get(&(current_sequence.clone(), depth)) {
        Some(count) => { return count.clone() },
        None => {},
    }

    let (_, count) = current_sequence.chars().fold(('A', 0_usize), |(last_char, mut count), char| {
        let pattern = format!("{}{}", last_char, char);
        count += pair_to_paths_map.get(&pattern).unwrap().iter().map(|next_part| {
            count_levels(&next_part, depth - 1, pair_to_paths_map, memo)
        }).min().unwrap();
        (char, count)
    });

    memo.insert((current_sequence.clone(), depth), count);
    count
}

fn task(input: &str, n_robots: usize) -> String {
    let numpad = Keypad::numpad();
    let arrowpad = Keypad::arrowpad();

    let mut pair_to_paths_map = numpad.generate_sequences();
    arrowpad.generate_sequences().iter().for_each(|(k, v)| {
        pair_to_paths_map.insert(k.clone(), v.clone());
    });


    let mut memo = HashMap::new();
    input.lines().fold(0, |acc, line| {
        let min_len = count_levels(&line.to_string(), n_robots, &pair_to_paths_map, &mut memo);
        acc + min_len * line[..line.len() - 1].parse::<usize>().unwrap()
    }).to_string()
}

pub fn task01(input: &str) -> String {
    task(input, 3)
}

pub fn task02(input: &str) -> String {
    task(input, 26)
}
