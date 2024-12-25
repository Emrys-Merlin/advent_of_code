use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
};

// const N_ROWS: usize = 7;
// const N_COLS: usize = 7;
// const N_OBSTACLES: usize = 12;
const N_ROWS: usize = 71;
const N_COLS: usize = 71;
const N_OBSTACLES: usize = 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

impl Point {
    pub fn neighbors(&self) -> Vec<Point> {
        let mut neighbors = Vec::new();
        if self.row > 0 {
            neighbors.push(Point {
                row: self.row - 1,
                col: self.col,
            });
        }
        neighbors.push(Point {
            row: self.row + 1,
            col: self.col,
        });
        if self.col > 0 {
            neighbors.push(Point {
                row: self.row,
                col: self.col - 1,
            });
        }
        neighbors.push(Point {
            row: self.row,
            col: self.col + 1,
        });
        neighbors
    }

    pub fn short_string(&self) -> String {
        format!("({},{})", self.col, self.row)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Grid {
    n_rows: usize,
    n_cols: usize,
    falling_obstacles: Vec<Point>,
}

impl Grid {
    pub fn from_input(input: &str) -> Self {
        let n_rows = N_ROWS;
        let n_cols = N_COLS;
        let mut falling_obstacles = Vec::new();

        for lin in input.lines() {
            let (left, right) = lin.split_once(",").unwrap();
            let col = left.parse().unwrap();
            let row = right.parse().unwrap();
            falling_obstacles.push(Point { row, col });
        }

        Grid {
            n_rows,
            n_cols,
            falling_obstacles,
        }
    }

    pub fn neighbors(&self, point: Point) -> Vec<Point> {
        point
            .neighbors()
            .iter()
            .filter_map(|p| {
                if p.row < self.n_rows && p.col < self.n_cols {
                    Some(*p)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn bfs(&self, n_obstacles: usize) -> usize {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back((Point { row: 0, col: 0 }, 0));
        visited.insert(Point { row: 0, col: 0 });

        for p in self.falling_obstacles.iter().take(n_obstacles) {
            visited.insert(*p);
        }

        while queue.len() > 0 {
            let (point, step) = queue.pop_front().unwrap();

            if point.row == self.n_rows - 1 && point.col == self.n_cols - 1 {
                return step;
            }

            for neighbor in self.neighbors(point) {
                if visited.contains(&neighbor) {
                    continue;
                }
                queue.push_back((neighbor, step + 1));
                visited.insert(neighbor);
            }
        }

        0
    }

    pub fn binary_search(&self) -> Point {
        let mut low = 0;
        // assuming that high does not work
        let mut high = self.falling_obstacles.len();
        while low < high {
            let mid = (low + high) / 2;
            let steps = self.bfs(mid);
            if steps == 0 {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
        // subtract one, because "mid" is the first byte that
        // isn't dropped.
        self.falling_obstacles.get(low - 1).unwrap().clone()
    }

    #[allow(dead_code)]
    pub fn to_string(&self, visited: Option<&HashSet<Point>>) -> String {
        let mut grid = vec![vec!['.'; self.n_cols]; self.n_rows];
        let visited = match visited {
            Some(v) => v,
            None => &HashSet::from_iter(self.falling_obstacles.iter().cloned()),
        };
        for p in visited {
            grid[p.row][p.col] = '#';
        }

        let mut s = String::new();
        for row in grid.iter() {
            for ch in row.iter() {
                s.push(*ch);
            }
            s.push('\n');
        }
        s
    }
}

pub fn task01(input: &str) -> String {
    let grid = Grid::from_input(input);
    grid.bfs(N_OBSTACLES).to_string()
}

pub fn task02(input: &str) -> String {
    let grid = Grid::from_input(input);
    grid.binary_search().short_string()
}
