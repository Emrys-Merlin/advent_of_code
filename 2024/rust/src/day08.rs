use std::collections::{HashMap,HashSet};
use std::cmp::max;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Point {
    row: i32,
    col: i32,
}

impl Point {
    pub fn minus(&self, other: &Point) -> Point {
        Point {
            row: self.row - other.row,
            col: self.col - other.col,
        }
    }

    pub fn plus(&self, other: &Point) -> Point {
        Point {
            row: self.row + other.row,
            col: self.col + other.col,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq,)]
struct Grid {
    n_rows: i32,
    n_cols: i32,
    antennas: HashMap<char, Vec<Point>>,
}

impl Grid {
    fn contains(&self, point: &Point) -> bool {
        0 <= point.col && point.col < self.n_cols && 0 <= point.row &&  point.row < self.n_rows
    }
    fn get_antinode(&self, antenna: char, with_harmonics: bool) -> Option<HashSet<Point>> {
        let points = self.antennas.get(&antenna)?;
        let mut result = HashSet::new();
        let steps = if with_harmonics { max(self.n_cols, self.n_cols) } else { 1 };

        for p in points.iter() {
            for q in points.iter() {
                if p == q {
                    continue;
                }
                let delta =  p.minus(q);
                let mut candidate = if with_harmonics { p.clone() } else { p.plus(&delta) };

                for _ in 0..steps {
                    if self.contains(&candidate) {
                        result.insert(candidate.clone());
                    } else {
                        break;
                    }
                    candidate = candidate.plus(&delta);
                }
            }
        }

        Some(result)
    }

    pub fn get_antinodes(&self, with_harmonics: bool) -> HashSet<Point> {
        let mut result = HashSet::new();
        for antenna in self.antennas.keys() {
            if let Some(antinode) = self.get_antinode(*antenna, with_harmonics) {
                result.extend(antinode);
            }
        }
        result
    }
}

fn parse_input(input: &str) -> Grid {
    let mut antennas: HashMap<char, Vec<Point>> = HashMap::new();
    let mut n_rows = 0;
    let mut n_cols: i32 = 0;
    for (row, line) in input.lines().enumerate() {
        if line.is_empty() {
            continue;
        }
        let signed_row = i32::try_from(row).expect("Too many rows for i32");
        n_rows += 1;
        n_cols = i32::try_from(line.len()).expect("Too many cols for i32");
        for (col, c) in line.chars().enumerate() {
            let signed_col = i32::try_from(col).expect("Too many rolumns for i32");
            if c != '.' {
                antennas.entry(c).or_insert(Vec::new()).push(Point { row: signed_row, col: signed_col });
            }
        }
    }

    Grid {
        n_rows,
        n_cols,
        antennas,
    }
}

pub fn task(input: &str, with_harmonics: bool) -> String {
    let grid = parse_input(input);
    let result = grid.get_antinodes(with_harmonics);
    result.len().to_string()
}

pub fn task01(input: &str) -> String {
    task(input, false)
}

pub fn task02(input: &str) -> String {
    task(input, true)
}