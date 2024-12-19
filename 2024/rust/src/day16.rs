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

    pub fn mirror(&self) -> Self {
        match self {
            Direction::N => Direction::S,
            Direction::E => Direction::W,
            Direction::S => Direction::N,
            Direction::W => Direction::E,
        }
    }

    pub fn all() -> Vec<Self> {
        vec![Direction::N, Direction::E, Direction::S, Direction::W]
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

    pub fn shortest_distances(&self, start_point: Point, start_direction: Direction) -> HashMap<(Point, Direction), usize> {
        let mut distance= HashMap::new();
        let mut heap = BinaryHeap::from([State { pos: start_point, dir: start_direction, cost: 0}]);

        while let Some(current) = heap.pop() {
            if distance.contains_key(&(current.pos, current.dir)) {
                continue;
            }

            distance.insert((current.pos, current.dir), current.cost);

            for child in self.new_states(&current) {
                heap.push( child );
            }
        }
        distance
    }

    pub fn shortest_path(&self) -> Option<usize> {
        let distances = self.shortest_distances(self.start, Direction::E);

        Direction::all().iter().filter_map(|&dir| {
            match distances.get(&(self.end, dir)) {
                Some(&distance) => Some(distance),
                None => None,
            }
        }).min()
    }

    // Based on idea by @jenuk
    pub fn points_on_shortest_path(&self) -> usize {
        let forward_distances = self.shortest_distances(self.start, Direction::E);

        let end_direction = Direction::all().iter().filter_map(|&dir| {
            match forward_distances.get(&(self.end, dir)) {
                Some(&distance) => Some((dir, distance)),
                None => None,
            }
        }).min_by_key(|&(_, distance)| distance).unwrap().0;

        let backward_distances = self.shortest_distances(self.end, end_direction.mirror());

        HashSet::<Point>::from_iter(forward_distances.iter().filter_map(|((point, direction), &distance)| {
            match backward_distances.get(&(*point, direction.mirror())) {
                Some(&backward_distance) => { if distance + backward_distance == forward_distances[&(self.end, end_direction)] {
                        Some(*point)
                    } else {
                        None
                    }
                },
                None => None,
            }
        })).len()
    }

    #[allow(dead_code)]
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
    grid.points_on_shortest_path().to_string()
}
