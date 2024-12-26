use std::collections::HashSet;

#[derive(Debug, Clone)]
struct Grid {
    n_rows: usize,
    n_cols: usize,
    obstacles: HashSet<(usize, usize)>,
}

impl Grid {
    pub fn step(&self, point: Point) -> Option<Point> {
        let (row, col) = match point.direction {
            Direction::N => (point.row.checked_sub(1)?, point.col),
            Direction::E => {
                if point.col + 1 >= self.n_cols {
                    return None;
                }
                (point.row, point.col + 1)
            }
            Direction::S => {
                if point.row + 1 >= self.n_rows {
                    return None;
                }
                (point.row + 1, point.col)
            }
            Direction::W => (point.row, point.col.checked_sub(1)?),
        };

        if self.obstacles.contains(&(row, col)) {
            let direction = match point.direction {
                Direction::N => Direction::E,
                Direction::E => Direction::S,
                Direction::S => Direction::W,
                Direction::W => Direction::N,
            };
            return Some(Point {
                row: point.row,
                col: point.col,
                direction,
            });
        }

        return Some(Point {
            row,
            col,
            direction: point.direction,
        });
    }

    pub fn check_loop(&self, point: Point) -> (bool, HashSet<Point>) {
        let mut visited: HashSet<Point> = HashSet::from([point.clone()]);
        let mut current = point;

        loop {
            current = match self.step(current) {
                None => return (false, visited),
                Some(new_point) => {
                    if visited.contains(&new_point) {
                        return (true, visited);
                    }
                    visited.insert(new_point.clone());
                    new_point
                }
            };
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum Direction {
    N,
    E,
    S,
    W,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Point {
    row: usize,
    col: usize,
    direction: Direction,
}

fn parse_input(input: &str) -> (Grid, Option<Point>) {
    let mut obstacles: HashSet<(usize, usize)> = HashSet::new();
    let n_rows = input.lines().count();
    let mut n_cols: usize = 0;
    let mut start: Option<Point> = None;

    for (i, line) in input.lines().enumerate() {
        // TODO check that constant
        n_cols = line.len();
        for (j, c) in line.trim().chars().enumerate() {
            start = match c {
                '#' => {
                    obstacles.insert((i, j));
                    start
                }
                '^' => Some(Point {
                    row: i,
                    col: j,
                    direction: Direction::N,
                }),
                'v' => Some(Point {
                    row: i,
                    col: j,
                    direction: Direction::S,
                }),
                '<' => Some(Point {
                    row: i,
                    col: j,
                    direction: Direction::W,
                }),
                '>' => Some(Point {
                    row: i,
                    col: j,
                    direction: Direction::E,
                }),
                _ => start,
            }
        }
    }

    (
        Grid {
            n_rows,
            n_cols,
            obstacles,
        },
        start,
    )
}

pub fn task01(input: &str) -> String {
    let (grid, prob_point) = parse_input(input);
    let mut point = prob_point.expect("No starting point found");
    let mut visited: HashSet<(usize, usize)> = HashSet::from([(point.row, point.col)]);

    loop {
        point = match grid.step(point) {
            None => break,
            Some(new_point) => {
                visited.insert((new_point.row, new_point.col));
                new_point
            }
        };
    }

    visited.len().to_string()
}

pub fn task02(input: &str) -> String {
    // Slightly less brute force solution for part 2
    // Only place obstacles on the initial path
    let (mut grid, prob_start) = parse_input(input);
    let start = prob_start.expect("No start point found");

    let mut initial_path: HashSet<(usize, usize)> = HashSet::from_iter(
        grid.check_loop(start.clone())
            .1
            .iter()
            .map(|point| (point.row, point.col)),
    );
    initial_path.remove(&(start.row, start.col));

    let mut result = 0;
    for &(i, j) in initial_path.iter() {
        grid.obstacles.insert((i, j));

        if grid.check_loop(start.clone()).0 {
            result += 1;
        };

        grid.obstacles.remove(&(i, j));
    }

    result.to_string()
}

#[cfg(test)]
mod tests {
    use super::super::fs_utils::{read_example, read_input};
    use super::*;

    #[test]
    fn test_task01() {
        let input = read_example(6, 1);
        assert_eq!(task01(&input), "41");
    }

    #[test]
    fn run_task01() {
        let input = read_input(6);
        assert_eq!(task01(&input), "5030");
    }

    #[test]
    fn test_task02() {
        let input = read_example(6, 1);
        assert_eq!(task02(&input), "6");
    }

    #[test]
    fn run_task02() {
        let input = read_input(6);
        assert_eq!(task02(&input), "1928");
    }
}
