use regex::Regex;

const OFFSET: usize = 10000000000000;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Claw {
    button_a: Point,
    button_b: Point,
    win_position: Point,
    cost_a: usize,
    cost_b: usize,
}

impl Claw {
    pub fn det(&self) -> isize {
        self.button_a.x as isize * self.button_b.y as isize - self.button_a.y as isize * self.button_b.x as isize
    }

    pub fn solve_equation(&self) -> Option<(usize, usize)> {
        let det = self.det();
        if det == 0 {
            return None;
        }

        let det_x = self.win_position.x as isize * self.button_b.y as isize - self.win_position.y as isize * self.button_b.x as isize;
        let det_y = self.button_a.x as isize * self.win_position.y as isize - self.button_a.y as isize * self.win_position.x as isize;

        if det_x % det != 0 || det_y % det != 0 {
            return None;
        }

        let (a, b) = (det_x / det, det_y / det);
        if a < 0 || b < 0 {
            return None;
        }
        Some((a as usize, b as usize))
    }

    pub fn cost(&self) -> Option<usize> {
        let (x, y) = self.solve_equation()?;
        Some(x * self.cost_a + y * self.cost_b)
    }

}

fn parse_line(line: &str) -> Point {
    let re = Regex::new(r"(Button .|Prize): X(\+|=)(?<x>\d+), Y(\+|=)(?<y>\d+)").unwrap();
    let caps = re.captures(line).unwrap();
    Point {
        x: caps.name("x").unwrap().as_str().parse::<usize>().unwrap(),
        y: caps.name("y").unwrap().as_str().parse::<usize>().unwrap(),
    }
}

fn parse_input(input: &str, task02: bool) -> Vec<Claw> {
    let mut point_a = Point { x: 0, y: 0 };
    let mut point_b = Point { x: 0, y: 0 };
    let mut win_position = Point { x: 0, y: 0 };
    let mut claws = Vec::new();
    let cost_a: usize = 3;
    let cost_b: usize = 1;
    for (i, line) in input.lines().enumerate() {
        match i % 4 {
            0 => {
                point_a = parse_line(line);
            }
            1 => {
                point_b = parse_line(line);
            }
            2 => {
                win_position = parse_line(line);
                if task02 {
                    win_position = Point { x: win_position.x + OFFSET, y: win_position.y + OFFSET };
                }
            }
            3 => {
                claws.push(Claw {
                    button_a: point_a,
                    button_b: point_b,
                    win_position,
                    cost_a,
                    cost_b,
                });
            },
            _ => unreachable!(),
        }

    }
    claws.push(Claw {
        button_a: point_a,
        button_b: point_b,
        win_position,
        cost_a,
        cost_b,
    });

    claws
}

pub fn task01(input: &str) -> String {
    let claws = parse_input(input, false);

    claws.iter().filter_map(|claw| claw.cost()).sum::<usize>().to_string()
}

pub fn task02(input: &str) -> String {
    let claws = parse_input(input, true);

    claws.iter().filter_map(|claw| claw.cost()).sum::<usize>().to_string()
}
