use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

impl Point {
    pub fn neighbors(&self) -> Vec<Point> {
        let mut neighbors: Vec<Point> = Vec::new();
        if self.row != 0 {
            neighbors.push(Point { row: self.row - 1, col: self.col });
        }
        if self.col != 0 {
            neighbors.push(Point { row: self.row, col: self.col - 1 });
        }
        neighbors.push(Point { row: self.row + 1, col: self.col });
        neighbors.push(Point { row: self.row, col: self.col + 1 });
        neighbors
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Grid {
    grid: Vec<Vec<usize>>,
    n_rows: usize,
    n_cols: usize,
    trailheads: Vec<Point>,
}

impl Grid {
    pub fn read_input(input: &str) -> Grid {
        let mut grid: Vec<Vec<usize>> = Vec::new();
        let mut n_rows: usize = 0;
        let mut trailheads: Vec<Point> = Vec::new();

        for (row, line) in input.lines().enumerate() {
            let mut row_vec: Vec<usize> = Vec::new();
            for (col, character) in line.chars().enumerate() {
                let value = character.to_string().parse::<usize>().unwrap();
                row_vec.push(value);
                if value == 0 {
                    trailheads.push(Point { row, col });
                }
            }
            grid.push(row_vec);
            n_rows += 1;
        }

        let n_cols = grid[0].len();

        Grid {
            grid,
            n_rows,
            n_cols,
            trailheads,
        }
    }

    pub fn contains(&self, point: &Point) -> bool {
        point.row < self.n_rows && point.col < self.n_cols
    }

    pub fn get(&self, point: &Point) -> Option<usize> {
        if !self.contains(point) {
            return None;
        }
        Some(self.grid[point.row][point.col])
    }

    pub fn neighbors(&self, point: &Point) -> Vec<Point> {
        let value = self.get(point).unwrap();
        point.neighbors().iter().filter_map(|p| {
            if self.get(p).unwrap_or(value) ==  value + 1 {
                Some(p.clone())
            } else {
                None
            }
        }).collect()
    }

    pub fn count_path(&self, start: &Point, use_score: bool) -> usize {
        let mut stack = vec![start.clone()];
        let mut count = 0;
        let mut visited: HashSet<Point> = HashSet::new();

        while stack.len() != 0 {
            let current = stack.pop().unwrap();
            if use_score && visited.contains(&current) {
                continue
            }

            visited.insert(current.clone());

            if self.get(&current).unwrap() == 9 {
                count += 1;
                continue
            }
            stack.extend(self.neighbors(&current));
        }

        count
    }

    pub fn count_paths(&self, use_score: bool) -> usize {
        let mut count = 0;
        for trailhead in &self.trailheads {
            count += self.count_path(trailhead, use_score);
        }
        count
    }
}


pub fn task01(input: &str) -> String {
    let grid = Grid::read_input(input);
    grid.count_paths(true).to_string()
}

pub fn task02(input: &str) -> String {
    let grid = Grid::read_input(input);
    grid.count_paths(false).to_string()
}
