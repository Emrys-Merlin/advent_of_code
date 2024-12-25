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
