use regex::Regex;

fn task(input: &str) -> i32 {
    let re = Regex::new(r"mul\((?<left>\d+),(?<right>\d+)\)").unwrap();
    re.captures_iter(input)
        .map(|m| {
            let left = m.name("left").unwrap().as_str().parse::<i32>().unwrap();
            let right = m.name("right").unwrap().as_str().parse::<i32>().unwrap();
            left * right
        })
        .sum()
}

pub fn task01(input: &str) -> String {
    let cleaned_input = input.trim();
    task(cleaned_input).to_string()
}

pub fn task02(mut input: &str) -> String {
    input = input.trim();
    let mut active = true;

    let mut result = 0;
    let mut content: &str;
    while input.len() != 0 {
        if active {
            (content, input) = input.split_once("don't()").unwrap_or((input, ""));
            result += task(content);
            active = false;
        } else {
            (_, input) = input.split_once("do()").unwrap_or((input, ""));
            active = true
        }
    }
    result.to_string()
}

#[cfg(test)]
mod tests {
    use super::super::fs_utils::{read_example, read_input};
    use super::*;

    #[test]
    fn test_task01() {
        let input = read_example(3, 1);
        assert_eq!(task01(&input), "161");
    }

    #[test]
    fn test_task02() {
        let input = read_example(3, 2);
        assert_eq!(task02(&input), "48");
    }

    #[test]
    fn run_task01() {
        let input = read_input(3);
        assert_eq!(task01(&input), "179571322");
    }

    #[test]
    fn run_task02() {
        let input = read_input(3);
        assert_eq!(task02(&input), "103811193");
    }
}
