use std::{collections::{BinaryHeap, HashMap, HashSet}, hash::Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    N,
    E,
    S,
    W,
}

impl Direction {
    pub fn rotate_left(&self) -> Self {
        match self {
            Direction::N => Direction::W,
            Direction::W => Direction::S,
            Direction::S => Direction::E,
            Direction::E => Direction::N,
        }
    }

    pub fn rotate_right(&self) -> Self {
        match self {
            Direction::N => Direction::E,
            Direction::E => Direction::S,
            Direction::S => Direction::W,
            Direction::W => Direction::N,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

impl Point {
    pub fn step(&self, dir: Direction) -> Option<Self> {
        match dir {
            Direction::N => Some(Point { row: self.row.checked_sub(1)?, col: self.col }),
            Direction::E => Some(Point { row: self.row, col: self.col + 1 }),
            Direction::S => Some(Point { row: self.row + 1, col: self.col }),
            Direction::W => Some(Point { row: self.row, col: self.col.checked_sub(1)? }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    pos: Point,
    dir: Direction,
    cost: usize,
}

impl State {
    pub fn new_states(&self) -> Vec<State> {
        Vec::from([
            State { pos: self.pos.step(self.dir).unwrap(), dir: self.dir, cost: self.cost + 1 },
            State { pos: self.pos, dir: self.dir.rotate_left(), cost: self.cost + 1000 },
            State { pos: self.pos, dir: self.dir.rotate_right(), cost: self.cost + 1000 },

        ])
    }

}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Grid {
    walls: HashSet<Point>,
    start: Point,
    end: Point,
}

impl Grid {
    pub fn from_input(input: &str) -> Self {
        let mut walls = HashSet::new();
        let mut start = Point { row: 0, col: 0 };
        let mut end = Point { row: 0, col: 0 };

        for (row, line) in input.lines().enumerate() {
            for (col, ch) in line.trim().chars().enumerate() {
                let point = Point { row, col };
                match ch {
                    '#' => { walls.insert(point); },
                    'S' => { start = point; },
                    'E' => { end = point; },
                    '.' => { },
                    _ =>  { unreachable!(); }
                }
            }
        }

        Self { walls, start, end }
    }

    fn new_states(&self, current: &State) -> Vec<State> {
        current.new_states().iter().filter_map(|&state| {
            match self.walls.contains(&state.pos) {
                true => None,
                false => Some(state),
            }
    }).collect()
    }

    fn n_valid_neighbors(&self, pos: &Point) -> usize {
        [Direction::N, Direction::E, Direction::S, Direction::W].iter().filter(|&dir| {
            let new_pos = pos.step(*dir);
            match new_pos {
                Some(new_pos) => !self.walls.contains(&new_pos),
                None => false,
            }
        }).count()
    }

    pub fn shortest_path(&self) -> Option<usize> {
        let mut visited = HashSet::new();
        let mut heap = BinaryHeap::from([State { pos: self.start, dir: Direction::E, cost: 0}]);

        while let Some(current) = heap.pop() {
            if current.pos == self.end {
                return Some(current.cost);
            }

            if !visited.insert((current.pos, current.dir)) {
                continue;
            }

            for child in self.new_states(&current) {
                heap.push( child );
            }
        }
        None
    }

    pub fn shortest_path_with_chain(&self, start: State) -> (usize, HashSet<State>) {
        let mut visited = HashSet::new();
        let mut heap = BinaryHeap::from([start]);
        let mut parent = HashMap::new();

        let mut chain = HashSet::new();
        let mut cost = 0;
        while let Some(current) = heap.pop() {
            if current.pos == self.end {
                let mut current = current;
                cost = current.cost;
                while let Some(parent) = parent.get(&current) {
                    chain.insert(current);
                    current = *parent;
                }
                chain.insert(current);
                break
            }

            if !visited.insert((current.pos, current.dir)) {
                continue;
            }

            for child in self.new_states(&current) {
                heap.push( child );
                parent.insert(child, current);
            }
        }
        (cost, chain)
    }

    pub fn count_all_shortest_paths(&self) -> usize {
        let start_state = State { pos: self.start, dir: Direction::E, cost: 0};
        let (cost, mut chain) = self.shortest_path_with_chain(start_state);
        println!("Cost: {}", cost);
        let mut visited = chain.clone();
        while chain.len() != 0{
            let current = chain.iter().next().cloned().unwrap();
            chain.remove(&current);

            if self.n_valid_neighbors(&current.pos) <= 2 {
                continue;
            }

            for new_state in self.new_states(&current) {
                if visited.contains(&new_state) {
                    continue;
                }
                let (new_cost, new_chain) = self.shortest_path_with_chain(new_state);
                if new_cost == cost {
                    chain = chain.union(&new_chain).cloned().collect();
                    visited = visited.union(&new_chain).cloned().collect();
                }
            }
        }
        let visited_points: HashSet<Point> = HashSet::from_iter(visited.iter().map(|state| state.pos));
        self.print_maze(&visited_points);
        visited_points.len()

    }

    fn print_maze(&self, visited: &HashSet<Point>) {
        let (n_rows, n_cols) = self.walls.iter().fold((0, 0), |(max_row, max_col), point| {
            (max_row.max(point.row+1), max_col.max(point.col+1))
        });

        for row in 0..n_rows {
            for col in 0..n_cols {
                let point = Point { row, col };
                if self.walls.contains(&point) {
                    print!("#");
                } else if point == self.start {
                    print!("S");
                } else if point == self.end {
                    print!("E");
                } else if visited.contains(&point) {
                    print!("O");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }
}

pub fn task01(input: &str) -> String {
    let grid = Grid::from_input(input);
    grid.shortest_path().unwrap().to_string()
}

pub fn task02(input: &str) -> String {
    let grid = Grid::from_input(input);
    // grid.count_tiles_on_shortest_paths_dfs().unwrap().to_string()
    println!("Caution super inefficient. Takes up to 3 min on my machine.");
    grid.count_all_shortest_paths().to_string()
}
