use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    N,
    E,
    S,
    W,
}

impl Direction {
    pub fn edge(inside: &Point, outside: &Point) -> Self {
        if inside.row < outside.row {
            Direction::S
        } else if inside.row > outside.row {
            Direction::N
        } else if inside.col < outside.col {
            Direction::E
        } else {
            Direction::W
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

#[derive(Debug, Clone)]
struct Grid {
    grid: Vec<Vec<char>>,
    n_rows: usize,
    n_cols: usize,
}

impl Grid {
    pub fn parse_input(input: &str) -> Self {
        let grid: Vec<Vec<char>> = input
            .lines()
            .filter_map(|line| {
                if line.is_empty() {
                    None
                } else {
                    Some(line.chars().collect())
                }
            })
            .collect();
        let n_rows = grid.len();
        let n_cols = grid[0].len();
        Self {
            grid,
            n_rows,
            n_cols,
        }
    }

    pub fn neighbors(&self, point: &Point) -> Vec<Point> {
        let mut neighbors = Vec::new();
        if point.row > 0 {
            neighbors.push(Point {
                row: point.row - 1,
                col: point.col,
            });
        }
        if point.row < self.n_rows - 1 {
            neighbors.push(Point {
                row: point.row + 1,
                col: point.col,
            });
        }
        if point.col > 0 {
            neighbors.push(Point {
                row: point.row,
                col: point.col - 1,
            });
        }
        if point.col < self.n_cols - 1 {
            neighbors.push(Point {
                row: point.row,
                col: point.col + 1,
            });
        }
        neighbors
    }

    pub fn get(&self, point: &Point) -> Option<char> {
        if point.row >= self.n_rows || point.col >= self.n_cols {
            None
        } else {
            Some(self.grid[point.row][point.col])
        }
    }

    pub fn area_perimeter(&self) -> usize {
        let mut not_visited: HashSet<Point> = HashSet::from_iter(
            (0..self.n_rows).flat_map(|row| (0..self.n_cols).map(move |col| Point { row, col })),
        );

        let mut result = 0;

        while not_visited.len() != 0 {
            let current = not_visited.iter().next().unwrap().clone();
            not_visited.remove(&current);
            let plant_type = self.get(&current).unwrap();
            let mut stack = vec![current];
            let mut perimeter: usize = 0;
            let mut area: usize = 0;

            while stack.len() != 0 {
                let current = stack.pop().unwrap();
                let mut potential_perimeter: usize = 4;
                for neighbor in self.neighbors(&current) {
                    if self.get(&neighbor).unwrap() == plant_type {
                        potential_perimeter -= 1;
                        if not_visited.remove(&neighbor) {
                            stack.push(neighbor);
                        }
                    }
                }
                perimeter += potential_perimeter;
                area += 1;
            }
            result += area * perimeter;
        }
        result
    }

    pub fn area_side(&self) -> usize {
        let mut not_visited: HashSet<Point> = HashSet::from_iter(
            (0..self.n_rows).flat_map(|row| (0..self.n_cols).map(move |col| Point { row, col })),
        );

        let mut result = 0;

        while not_visited.len() != 0 {
            let current = not_visited.iter().next().unwrap().clone();
            not_visited.remove(&current);
            let plant_type = self.get(&current).unwrap();
            let mut stack = vec![current];
            let mut area: usize = 0;
            let mut edges: HashMap<(Direction, usize), HashSet<usize>> = HashMap::new();

            while stack.len() != 0 {
                let current = stack.pop().unwrap();
                for neighbor in self.neighbors(&current) {
                    if self.get(&neighbor).unwrap() != plant_type {
                        let direction = Direction::edge(&current, &neighbor);
                        let (outer, inner) =
                            if direction == Direction::N || direction == Direction::S {
                                (current.row, current.col)
                            } else {
                                (current.col, current.row)
                            };
                        edges.entry((direction, outer)).or_default().insert(inner);
                    } else {
                        if not_visited.remove(&neighbor) {
                            stack.push(neighbor);
                        }
                    }
                }
                if current.col == 0 {
                    edges
                        .entry((Direction::W, current.col))
                        .or_default()
                        .insert(current.row);
                }
                if current.col + 1 == self.n_cols {
                    edges
                        .entry((Direction::E, current.col))
                        .or_default()
                        .insert(current.row);
                }
                if current.row == 0 {
                    edges
                        .entry((Direction::N, current.row))
                        .or_default()
                        .insert(current.col);
                }
                if current.row + 1 == self.n_rows {
                    edges
                        .entry((Direction::S, current.row))
                        .or_default()
                        .insert(current.col);
                }
                area += 1;
            }
            let sides = edges.iter().fold(0, |sides: usize, (_, inner_hash)| {
                let mut inner = Vec::from_iter(inner_hash.iter().map(|&x| x));
                inner.sort();
                sides
                    + inner.iter().enumerate().fold(0, |sides: usize, (i, pos)| {
                        sides
                            + if i == 0 || *pos != inner[i - 1] + 1 {
                                1
                            } else {
                                0
                            }
                    })
            });
            result += area * sides;
        }
        result
    }
}

pub fn task01(input: &str) -> String {
    let grid = Grid::parse_input(input);
    grid.area_perimeter().to_string()
}

pub fn task02(input: &str) -> String {
    let grid = Grid::parse_input(input);
    grid.area_side().to_string()
}

#[cfg(test)]
mod tests {
    use super::super::fs_utils::{read_example, read_input};
    use super::*;

    #[test]
    fn test_task01() {
        let input = read_example(12, 3);
        assert_eq!(task01(&input), "1930");
    }

    #[test]
    fn run_task01() {
        let input = read_input(12);
        assert_eq!(task01(&input), "1451030");
    }

    #[test]
    fn test_task02() {
        let input = read_example(12, 3);
        assert_eq!(task02(&input), "1206");
    }

    #[test]
    fn run_task02() {
        let input = read_input(12);
        assert_eq!(task02(&input), "859494");
    }
}
