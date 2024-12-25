use regex::Regex;
use std::collections::HashMap;

// const N_ROWS: usize = 7;
// const N_COLS: usize = 11;
const N_ROWS: usize = 103;
const N_COLS: usize = 101;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Quadrant {
    UpperLeft,
    LowerLeft,
    UpperRight,
    LowerRight,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Point {
    row: isize,
    col: isize,
}

impl Point {
    pub fn add(&self, other: &Point) -> Point {
        Point {
            row: self.row + other.row,
            col: self.col + other.col,
        }
    }

    pub fn scale(&self, factor: isize) -> Point {
        Point {
            row: self.row * factor,
            col: self.col * factor,
        }
    }

    pub fn quadrant(&self, n_rows: usize, n_cols: usize) -> Option<Quadrant> {
        let mid_rows = (n_rows / 2) as isize;
        let mid_cols = (n_cols / 2) as isize;

        if self.row == mid_rows || self.col == mid_cols {
            return None;
        }

        if self.row < mid_rows {
            if self.col < mid_cols {
                return Some(Quadrant::UpperLeft);
            }
            return Some(Quadrant::UpperRight);
        }

        if self.col < mid_cols {
            return Some(Quadrant::LowerLeft);
        }

        Some(Quadrant::LowerRight)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Robot {
    start: Point,
    velocity: Point,
}

impl Robot {
    pub fn step(&self, n_steps: usize, n_rows: usize, n_cols: usize) -> Point {
        let unbound_end = self.start.add(&self.velocity.scale(n_steps as isize));

        Point {
            row: unbound_end.row.rem_euclid(n_rows as isize),
            col: unbound_end.col.rem_euclid(n_cols as isize),
        }
    }
}

fn parse_input(input: &str) -> Vec<Robot> {
    let re =
        Regex::new(r"p=(?<poscol>\d+),(?<posrow>\d+) v=(?<velcol>-?\d+),(?<velrow>-?\d+)").unwrap();
    let mut robots = Vec::new();
    for line in input.lines() {
        if line.is_empty() {
            continue;
        }

        let capture = re.captures(line).unwrap();
        let start = Point {
            row: capture
                .name("posrow")
                .unwrap()
                .as_str()
                .parse::<isize>()
                .unwrap(),
            col: capture
                .name("poscol")
                .unwrap()
                .as_str()
                .parse::<isize>()
                .unwrap(),
        };
        let velocity = Point {
            row: capture
                .name("velrow")
                .unwrap()
                .as_str()
                .parse::<isize>()
                .unwrap(),
            col: capture
                .name("velcol")
                .unwrap()
                .as_str()
                .parse::<isize>()
                .unwrap(),
        };
        let robot = Robot { start, velocity };
        robots.push(robot);
    }
    robots
}

fn score(robots: &Vec<Robot>, n_rows: usize, n_cols: usize, n_steps: usize) -> usize {
    let counts = robots.iter().fold(HashMap::new(), |mut counts, robot| {
        let quadrant = robot.step(n_steps, n_rows, n_cols).quadrant(n_rows, n_cols);
        match quadrant {
            None => counts,
            Some(quadrant) => {
                *counts.entry(quadrant).or_insert(0_usize) += 1;
                counts
            }
        }
    });

    if counts.len() != 4 {
        return 0;
    }

    counts.values().fold(1_usize, |prod, &count| prod * count)
}

fn render(robots: &Vec<Robot>, n_rows: usize, n_cols: usize, n_steps: usize) -> Vec<Vec<char>> {
    let mut grid = vec![vec!['.'; n_cols]; n_rows];
    for robot in robots {
        let pos = robot.step(n_steps, n_rows, n_cols);
        grid[pos.row as usize][pos.col as usize] = '#';
    }
    grid
}

fn print_grid(grid: &Vec<Vec<char>>) {
    for row in grid {
        println!("{}", row.iter().collect::<String>());
    }
}

pub fn task01(input: &str) -> String {
    let robots = parse_input(input);
    let n_steps = 100_usize;

    score(&robots, N_ROWS, N_COLS, n_steps).to_string()
}

pub fn task02(input: &str) -> String {
    let robots = parse_input(input);
    let max_period = N_ROWS * N_COLS;
    let max_render = 10;

    let mut scores = (1..max_period)
        .map(|n_steps| (n_steps, score(&robots, N_ROWS, N_COLS, n_steps)))
        .collect::<Vec<(usize, usize)>>();
    scores.sort_by(|a, b| a.1.cmp(&b.1));

    for i in 0..max_render {
        let (n_steps, score) = scores[i];
        println!("n_steps: {}, score: {}", n_steps, score);
        let grid = render(&robots, N_ROWS, N_COLS, n_steps);
        print_grid(&grid);
    }

    "Everything printed".to_string()
}
