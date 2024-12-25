pub fn task01(input: &str) -> String {
    let mut safe_lines = 0;
    for line in input.lines() {
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() {
            continue;
        }

        let parts: Vec<i32> = trimmed_line
            .split(" ")
            .map(|x| x.trim().parse().expect("Should contain integers"))
            .collect();
        let mut safe: bool = true;

        let mut last = parts[1];
        let mut delta = parts[1] - parts[0];
        if delta.abs() > 3 || delta == 0 {
            continue;
        }

        for i in 2..parts.len() {
            let current = parts[i];
            let new_delta = current - last;
            if new_delta.abs() > 3 || new_delta == 0 || new_delta * delta < 0 {
                safe = false;
                break;
            }
            delta = new_delta;
            last = current;
        }
        if safe {
            safe_lines += 1;
        }
    }

    safe_lines.to_string()
}

pub fn task02(input: &str) -> String {
    let mut safe_lines = 0;
    for line in input.lines() {
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() {
            continue;
        }

        let parts: Vec<i32> = trimmed_line
            .split(" ")
            .map(|x| x.trim().parse().expect("Should contain integers"))
            .collect();

        for skip in 0..parts.len() {
            let mut safe: bool = true;
            let (i0, i1, offset) = if skip == 0 {
                (1, 2, 3)
            } else if skip == 1 {
                (0, 2, 3)
            } else {
                (0, 1, 2)
            };

            let mut last = parts[i1];
            let mut delta = parts[i1] - parts[i0];
            if delta.abs() > 3 || delta == 0 {
                continue;
            }

            for i in offset..parts.len() {
                if i == skip {
                    continue;
                }
                let current = parts[i];
                let new_delta = current - last;
                if new_delta.abs() > 3 || new_delta == 0 || new_delta * delta < 0 {
                    safe = false;
                    break;
                }
                delta = new_delta;
                last = current;
            }
            if safe {
                safe_lines += 1;
                break;
            }
        }
    }
    safe_lines.to_string()
}
