fn parse_input(input: &str) -> Vec<(usize, Vec<usize>)> {
    let mut result = Vec::new();
    for line in input.lines() {
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() {
            continue;
        }
        let (test_str, list_str) = trimmed_line.split_once(":").unwrap();
        let test = test_str.parse::<usize>().unwrap();
        let list = list_str
            .trim()
            .split(' ')
            .map(|x| x.parse::<usize>().unwrap())
            .collect();
        result.push((test, list));
    }
    result
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Multiply,
    Cat,
}

impl Operator {
    pub fn exec(&self, a: usize, b: usize) -> usize {
        match self {
            Operator::Add => a + b,
            Operator::Multiply => a * b,
            Operator::Cat => (a.to_string() + &b.to_string()).parse::<usize>().unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    result: usize,
    idx: usize,
    operator: Operator,
}

fn check_test(test_result: &usize, test: &Vec<usize>, operators: &Vec<Operator>) -> bool {
    let mut stack: Vec<State> = Vec::from([State {
        result: 0,
        idx: 0,
        operator: Operator::Add,
    }]);

    while stack.len() > 0 {
        let state = stack.pop().unwrap();
        if state.result == *test_result && state.idx == test.len() {
            return true;
        }

        if state.result > *test_result || state.idx >= test.len() {
            continue;
        }

        let value = test[state.idx];
        let new_result = state.operator.exec(state.result, value);
        let new_idx = state.idx + 1;
        for operator in operators.iter() {
            let new_state = State {
                result: new_result,
                idx: new_idx,
                operator: operator.clone(),
            };
            stack.push(new_state);
        }
    }
    false
}

fn task(input: &str, operators: &Vec<Operator>) -> String {
    let tests = parse_input(input);

    tests
        .iter()
        .filter_map(|(test_result, test)| {
            if check_test(test_result, test, operators) {
                Some(test_result)
            } else {
                None
            }
        })
        .sum::<usize>()
        .to_string()
}

pub fn task01(input: &str) -> String {
    let operators = Vec::from([Operator::Add, Operator::Multiply]);
    task(input, &operators)
}

pub fn task02(input: &str) -> String {
    let operators = Vec::from([Operator::Add, Operator::Multiply, Operator::Cat]);
    task(input, &operators)
}

#[cfg(test)]
mod tests {
    use super::super::fs_utils::{read_example, read_input};
    use super::*;

    #[test]
    fn test_task01() {
        let input = read_example(7, 1);
        assert_eq!(task01(&input), "3749");
    }

    #[test]
    fn run_task01() {
        let input = read_input(7);
        assert_eq!(task01(&input), "42283209483350");
    }

    #[test]
    fn test_task02() {
        let input = read_example(7, 1);
        assert_eq!(task02(&input), "11387");
    }

    #[test]
    fn run_task02() {
        let input = read_input(7);
        assert_eq!(task02(&input), "1026766857276279");
    }
}
