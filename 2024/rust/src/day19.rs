use std::collections::HashMap;

fn parse_input(input: &str) -> (Vec<String>, Vec<String>) {
    let mut patterns = Vec::new();
    let mut towels = Vec::new();
    for (i, line) in input.lines().enumerate() {
        if line.is_empty() {
            continue;
        }
        if i == 0 {
            patterns = line.split(", ").map(|x| x.to_string()).collect();
            continue;
        }

        towels.push(line.trim().to_string());
    }
    (patterns, towels)
}

fn check_towel(towel: &String, patterns: &Vec<String>, idx: usize) -> bool {
    if idx == towel.len() {
        return true;
    }
    patterns.iter().fold(false, |possible, pattern | {
        possible || (towel[idx..].starts_with(pattern) && check_towel(towel, patterns, idx + pattern.len()))
    })
}

fn count_combinations(towel: &String, patterns: &Vec<String>, memo: &mut HashMap<String, usize>) -> usize {
    if towel.len() == 0 {
        return 1;
    }
    if let Some(&count) = memo.get(towel) {
        return count;
    }
    let count = patterns.iter().fold(0, |count, pattern | {
        count + (if towel.starts_with(pattern) {
            count_combinations(&towel[pattern.len()..].to_string(), patterns, memo)
        } else {
            0
        })
    });
    memo.insert(towel.clone(), count);
    count
}

pub fn task01(input: &str) -> String {
    let (patterns, towels) = parse_input(input);
    towels.iter().map(|towel| {
        check_towel(towel, &patterns, 0) as usize
    }).sum::<usize>().to_string()
}

pub fn task02(input: &str) -> String {
    let (patterns, towels) = parse_input(input);
    towels.iter().map(|towel| {
        count_combinations(towel, &patterns, &mut HashMap::new())
    }).sum::<usize>().to_string()
}
