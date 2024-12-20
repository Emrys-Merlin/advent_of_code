use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

impl Point {
    pub fn neighbors(&self) -> Vec<Self> {
        let mut neighbors = Vec::new();
        if self.row > 0 {
            neighbors.push(Point { row: self.row - 1, col: self.col });
        }
        neighbors.push(Point { row: self.row, col: self.col + 1 });
        neighbors.push(Point { row: self.row + 1, col: self.col });
        if self.col > 0 {
            neighbors.push(Point { row: self.row, col: self.col - 1 });
        }
        neighbors
    }

    pub fn distance(&self, other: &Point) -> usize {
        (self.row as isize - other.row as isize).abs() as usize
            + (self.col as isize - other.col as isize).abs() as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Grid {
    walls: HashSet<Point>,
    start: Point,
    end: Point,
}

impl Grid {
    pub fn parse_input(input: &str) -> Self {
        let mut walls = HashSet::new();
        let mut start = Point { row: 0, col: 0 };
        let mut end = Point { row: 0, col: 0 };
        for (row, line) in input.lines().enumerate() {
            for (col, ch) in line.chars().enumerate() {
                let point = Point { row, col };
                match ch {
                    '#' => { walls.insert(point); },
                    'S' => { start = point; },
                    'E' => { end = point; },
                    '.' => {},
                    _ => unreachable!(),
                }
            }
        }
        Grid { walls, start, end }
    }

    fn neighbors(&self, point: Point) -> Vec<Point> {
        point.neighbors().iter().filter_map(|&point| {
            match self.walls.contains(&point) {
                true => None,
                false => Some(point),
            }
        }).collect()
    }

    pub fn follow_unique_path(&self) -> Option<Vec<Point>> {
        let mut path = vec![self.start];
        let mut previous = self.start;
        let candidates = self.neighbors(self.start);
        if candidates.len() != 1 {
            return None;
        }
        let mut current = candidates[0];
        path.push(current);

        while current != self.end {
            let neighbors = self.neighbors(current).iter().filter_map(|&p| if p != previous { Some(p) } else {None}).collect::<Vec<_>>();
            if neighbors.len() != 1 {
                return None;
            }
            previous = current;
            current = neighbors[0].clone();
            path.push(current);
        }
        Some(path)
    }

    pub fn count_cheats(&self, cheat_length: usize) -> HashMap<usize, usize> {
        let mut cheats = HashMap::new();
        let track = self.follow_unique_path().unwrap();

        for (i, p) in track.iter().enumerate() {
            for (j, q) in track.iter().enumerate().skip(i + 1) {
                let dist = p.distance(q);
                if dist > cheat_length {
                    continue;
                }

                let delta = j - i;
                let save = delta - dist;
                let current = *cheats.get(&save).unwrap_or(&0);
                cheats.insert(save, current + 1);
            }
        }
        cheats
    }
}

fn task(input: &str, cheat_length: usize) -> String {
    let grid = Grid::parse_input(input);
    let cheats = grid.count_cheats(cheat_length);
    cheats.iter().fold(0, |acc, (&k, &v)| acc + if k >= 100 { v } else { 0 }).to_string()

}

pub fn task01(input: &str) -> String {
    task(input, 2)
}

pub fn task02(input: &str) -> String {
    task(input, 20)
}
