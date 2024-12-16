use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

impl Point {
    pub fn move_dir(&self, dir: char) -> Option<Point> {
        match dir {
            '^' => Some(Point { row: self.row.checked_sub(1)?, col: self.col }),
            'v' => Some(Point { row: self.row + 1, col: self.col }),
            '>' => Some(Point { row: self.row, col: self.col + 1 }),
            '<' => Some(Point { row: self.row, col: self.col.checked_sub(1)? }),
            _ => None,
        }
    }

    pub fn gps(&self) -> usize {
        100 * self.row + self.col
    }
}

struct Grid {
    walls: HashSet<Point>,
    crates: HashSet<Point>,
    robot: Point,
    instructions: Vec<char>,
}

impl Grid {
    pub fn from_input(input: &str, col_factor: usize) -> Self {
        let mut walls = HashSet::new();
        let mut crates = HashSet::new();
        let mut robot = Point { row: 0, col: 0 };
        let mut instructions = Vec::new();
        let mut switch = false;

        for (row, line) in input.lines().enumerate() {

            if line.is_empty() {
                switch = true;
                continue;
            }

            if !switch {
                for (col, ch) in line.trim().chars().enumerate() {
                    let point = Point { row, col: col * col_factor };
                    match ch {
                        '#' => {
                            let mut point = point;
                            for _ in 0..col_factor {
                                walls.insert(point);
                                point = point.move_dir('>').unwrap();
                            }
                        },
                        '@' => { robot = point; },
                        'O' => { crates.insert(point); },
                        '.' => { },
                        _ =>  { unreachable!(); }
                    }
                }
            } else {
                instructions.extend(line.chars());
            }
        }

        Self { walls, crates, robot, instructions }
    }

    pub fn gps(&self) -> usize {
        self.crates.iter().map(|point| point.gps()).sum()
    }

    pub fn run(&mut self) {
        for &instruction in self.instructions.iter() {
            let mut new_robot = self.robot.move_dir(instruction).unwrap();
            if self.walls.contains(&new_robot) {
                continue;
            }

            if self.crates.contains(&new_robot) {
                let mut new_crate = new_robot.move_dir(instruction).unwrap();

                while self.walls.contains(&new_crate) || self.crates.contains(&new_crate) {
                    if self.walls.contains(&new_crate) {
                        new_crate = new_robot.clone();
                        new_robot = self.robot.clone();
                        break;
                    }
                    new_crate = new_crate.move_dir(instruction).unwrap();
                    continue;
                }

                self.crates.remove(&new_robot);
                self.crates.insert(new_crate);
            }

            self.robot = new_robot;
        }
    }

    // TODO Refactor: Part as return type; part as reference
    fn move_wide_crate(&self, crate_point: &Point, direction: char, remove: &mut HashSet<Point>, insert: &mut HashSet<Point>) -> bool {
        if !self.crates.contains(crate_point) {
            return true;
        }

        let new_crate = crate_point.move_dir(direction).unwrap();
        let new_crate_right = new_crate.move_dir('>').unwrap();
        let new_crate_left = new_crate.move_dir('<').unwrap();
        if self.walls.contains(&new_crate) || self.walls.contains(&new_crate_right) {
            return false;
        }

        let move_possible = match direction {
            '^' | 'v' => {
                [new_crate, new_crate_right, new_crate_left].iter().fold(true, |move_possible, new_crate| {
                    move_possible && (!self.crates.contains(new_crate) || self.move_wide_crate(new_crate, direction, remove, insert))
                })
            },
            '>' => {
                !self.crates.contains(&new_crate_right) || self.move_wide_crate(&new_crate_right, direction, remove, insert)
            },
            '<' => {
                !self.crates.contains(&new_crate_left) || self.move_wide_crate(&new_crate_left, direction, remove, insert)
            },
            _ => { unreachable!(); },
        };

        if move_possible {
            remove.insert(crate_point.clone());
            insert.insert(new_crate);
        }
        move_possible
    }

    pub fn run_wide(&mut self) {
        for &instruction in self.instructions.clone().iter() {
            let new_robot = self.robot.move_dir(instruction).unwrap();

            if self.walls.contains(&new_robot) {
                continue;

            }
            let left = new_robot.move_dir('<').unwrap();
            let mut remove = HashSet::new();
            let mut insert = HashSet::new();

            let move_possible = match instruction {
                '^' | 'v' => {
                    (!self.crates.contains(&left) || self.move_wide_crate(&left, instruction, &mut remove, &mut insert)) &&
                    (!self.crates.contains(&new_robot) || self.move_wide_crate(&new_robot, instruction, &mut remove, &mut insert))
                },
                '>' => {
                    !self.crates.contains(&new_robot) || self.move_wide_crate(&new_robot, instruction, &mut remove, &mut insert)
                },
                '<' => {
                    !self.crates.contains(&left) || self.move_wide_crate(&left, instruction, &mut remove, &mut insert)
                },
                _ => { unreachable!(); },

            };

            if move_possible {
                self.robot = new_robot;
                // TODO beautify
                for point in remove.iter() {
                    self.crates.remove(point);
                }
                for point in insert.iter() {
                    self.crates.insert(point.clone());
                }
            }
        }
    }
}

pub fn task01(input: &str) -> String {
    let mut grid = Grid::from_input(input, 1);
    grid.run();
    grid.gps().to_string()
}

pub fn task02(input: &str) -> String {
    let mut grid = Grid::from_input(input, 2);
    grid.run_wide();
    grid.gps().to_string()
}
