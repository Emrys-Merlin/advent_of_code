fn parse_input(input: &str) -> (Vec<[usize; 5]>, Vec<[usize; 5]>) {
    let mut locks = Vec::new();
    let mut keys = Vec::new();

    let mut is_lock = true;
    let mut key_or_lock = [0; 5];
    for (i, line) in input.lines().enumerate() {
        match i % 8 {
            7 => {
                if is_lock {
                    locks.push(key_or_lock);
                } else {
                    keys.push(key_or_lock);
                }
                key_or_lock = [0; 5];
                continue;
            }
            0 => {
                is_lock = line.starts_with("#");
            }
            _ => {}
        }

        for (j, char) in line.chars().enumerate() {
            if char == '#' {
                key_or_lock[j] += 1;
            }
        }
    }

    if is_lock {
        locks.push(key_or_lock);
    } else {
        keys.push(key_or_lock);
    }

    (locks, keys)
}

fn add_up(lock: &[usize; 5], key: &[usize; 5]) -> bool {
    (0..5).all(|i| lock[i] + key[i] <= 7)
}

pub fn task01(input: &str) -> String {
    let (locks, keys) = parse_input(input);

    locks
        .iter()
        .flat_map(|lock| {
            keys.iter()
                .filter_map(move |key| if add_up(lock, key) { Some(true) } else { None })
        })
        .count()
        .to_string()
}
