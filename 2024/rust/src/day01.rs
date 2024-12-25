use std::{collections::HashMap, iter::zip};

fn build_lists(input: &str) -> (Vec<i32>, Vec<i32>) {
    let mut left: Vec<i32> = Vec::new();
    let mut right: Vec<i32> = Vec::new();

    for line in input.lines() {
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() {
            continue;
        }
        let (l, r) = trimmed_line.split_once(" ").expect("Could not split line.");
        left.push(l.trim().parse().expect("Could not parse left side."));
        right.push(r.trim().parse().expect("Could not parse right side."));
    }

    (left, right)
}

fn count(list: Vec<i32>) -> HashMap<i32, i32> {
    let mut counts: HashMap<i32, i32> = HashMap::new();
    for item in list {
        *counts.entry(item).or_insert(0) += 1;
    }
    counts
}

pub fn task01(input: &str) -> String {
    let (mut left, mut right) = build_lists(input);

    left.sort();
    right.sort();

    let mut result = 0;
    for (l, r) in zip(left.iter(), right.iter()) {
        result += (l - r).abs();
    }

    result.to_string()
}

pub fn task02(input: &str) -> String {
    let (left, right) = build_lists(input);

    let counts = count(right);

    let mut result = 0;
    for item in left.iter() {
        let entry = *counts.get(item).unwrap_or(&0);
        result += *item * entry;
    }

    result.to_string()
}
