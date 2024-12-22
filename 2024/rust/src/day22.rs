use std::collections::{HashMap, HashSet};

const MODULUS: usize = 1 << 24;
const N_ITERATIONS: usize = 2_000;

fn parse_input(input: &str) -> Vec<usize> {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn mix_prune(secret: &mut usize, update: &usize) {
    *secret = (*secret ^ *update) % MODULUS;
}

fn update_secret(secret: &mut usize, lookup: &mut HashMap<usize, usize>) {
    if lookup.contains_key(secret) {
        *secret = lookup.get(secret).unwrap().clone();
    } else {
        let old_secret = *secret;
        let mut result = *secret  << 6;
        mix_prune(secret, &result);
        result = *secret >> 5;
        mix_prune(secret, &result);
        result = *secret << 11;
        mix_prune(secret, &result);
        lookup.insert(old_secret, *secret);
    }
}

pub fn task01(input: &str) -> String {
    let mut secrets = parse_input(input);
    let mut lookup = HashMap::with_capacity(MODULUS);

    secrets.iter_mut().map(|secret| {
        for _ in 0..N_ITERATIONS {
            update_secret(secret, &mut lookup);
        }
        *secret
    }).sum::<usize>().to_string()
}

pub fn task02(input: &str) -> String {
    let mut secrets = parse_input(input);
    let mut lookup = HashMap::with_capacity(MODULUS);
    // Map price pattern -> number of bananas
    let mut bananas = HashMap::new();

    secrets.iter_mut().for_each(|secret| {
        let mut price_deltas = Vec::new();
        let mut used_patterns = HashSet::new();

        (0..N_ITERATIONS).fold(0, |last_price, i| {
            update_secret(secret, &mut lookup);

            let price = *secret % 10;

            if i > 0 {
                price_deltas.push(price as isize - last_price as isize);
            }

            if price_deltas.len() >= 4 {
                // String conversion turned out easier (for me) than extracting a fixed sized array or tuple...
                let pattern = price_deltas.iter().rev().take(4).map(|delta| delta.to_string()).collect::<Vec<_>>().join(",");
                // Make sure we only use each pattern once
                if !used_patterns.contains(&pattern) {
                    bananas.insert(pattern.clone(), price + *bananas.get(&pattern).unwrap_or(&0));
                    used_patterns.insert(pattern.clone());
                }
            }
            price
        });
    });

    bananas.values().max().unwrap().to_string()
}
