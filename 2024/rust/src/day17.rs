use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Register {
    a: usize,
    b: usize,
    c: usize,
}

impl Register {
    pub fn combo(&self, operand: usize) -> usize {
        match operand {
            0 => 0,
            1 => 1,
            2 => 2,
            3 => 3,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            _ => {
                unreachable!()
            }
        }
    }

    pub fn insert(&mut self, register: char, value: usize) {
        match register {
            'a' => {
                self.a = value;
            }
            'b' => {
                self.b = value;
            }
            'c' => {
                self.c = value;
            }
            _ => {
                unreachable!()
            }
        }
    }

    pub fn get(&self, register: &char) -> Option<&usize> {
        match register {
            'a' => Some(&self.a),
            'b' => Some(&self.b),
            'c' => Some(&self.c),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Computer {
    registers: Register,
    instructions: Vec<usize>,
}

impl Computer {
    pub fn parse_input(input: &str) -> Self {
        let mut registers = Register { a: 0, b: 0, c: 0 };
        let mut instructions = Vec::new();

        for (i, line) in input.lines().enumerate() {
            match i {
                0 => {
                    registers.insert('a', line.split_once(": ").unwrap().1.parse().unwrap());
                }
                1 => {
                    registers.insert('b', line.split_once(": ").unwrap().1.parse().unwrap());
                }
                2 => {
                    registers.insert('c', line.split_once(": ").unwrap().1.parse().unwrap());
                }
                4 => {
                    instructions = line
                        .split_once(": ")
                        .unwrap()
                        .1
                        .trim()
                        .split(",")
                        .map(|x| x.parse().unwrap())
                        .collect();
                }
                _ => {}
            }
            if i == 0 {}
        }
        Computer {
            registers,
            instructions,
        }
    }

    fn dv(&mut self, pointer: usize, operand: usize, register: char) -> (usize, Option<usize>) {
        let numerator = self.registers.a;
        self.registers
            .insert(register, numerator >> self.registers.combo(operand));
        (pointer + 2, None)
    }

    fn bx(
        &mut self,
        pointer: usize,
        operand: usize,
        register: Option<char>,
    ) -> (usize, Option<usize>) {
        let left = self.registers.b;
        let right = match register {
            None => operand,
            Some(register) => *self.registers.get(&register).unwrap(),
        };
        self.registers.insert('b', left ^ right);
        (pointer + 2, None)
    }

    fn bst(&mut self, pointer: usize, operand: usize) -> (usize, Option<usize>) {
        self.registers
            .insert('b', self.registers.combo(operand) % 8);
        (pointer + 2, None)
    }

    fn jnz(&self, pointer: usize, operand: usize) -> (usize, Option<usize>) {
        if *self.registers.get(&'a').unwrap() != 0 {
            (operand, None)
        } else {
            (pointer + 2, None)
        }
    }

    fn out(&self, pointer: usize, operand: usize) -> (usize, Option<usize>) {
        (pointer + 2, Some(self.registers.combo(operand) % 8))
    }

    pub fn run(&mut self, once: bool) -> String {
        let mut pointer = 0;
        let mut output = Vec::new();

        while pointer < self.instructions.len() {
            let opcode = self.instructions[pointer];
            let operand = self.instructions[pointer + 1];

            let (new_pointer, result) = match opcode {
                0 => self.dv(pointer, operand, 'a'),
                1 => self.bx(pointer, operand, None),
                2 => self.bst(pointer, operand),
                3 => {
                    if once {
                        (pointer + 2, None)
                    } else {
                        self.jnz(pointer, operand)
                    }
                }
                4 => self.bx(pointer, operand, Some('c')),
                5 => self.out(pointer, operand),
                6 => self.dv(pointer, operand, 'b'),
                7 => self.dv(pointer, operand, 'c'),
                _ => {
                    unreachable!()
                }
            };

            match result {
                None => {}
                Some(result) => {
                    output.push(result);
                }
            }

            pointer = new_pointer;
        }

        output
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(",")
    }

    fn check_task02_assumptions(&self) -> bool {
        let mut result = true;

        // Check that the instructions are even
        result &= self.instructions.len() % 2 == 0;
        // At least 2 instuctions
        result &= self.instructions.len() >= 4;
        // Last instruction is jump to 0;
        result &= self.instructions[self.instructions.len() - 2] == 3;
        result &= self.instructions[self.instructions.len() - 1] == 0;
        // Second to last instruction is a print of a register
        result &= self.instructions[self.instructions.len() - 4] == 5;
        result &= self.instructions[self.instructions.len() - 3] >= 4;

        result
    }

    pub fn find_input(&mut self, mut max_iter: usize) -> usize {
        max_iter = if max_iter == 0 {
            self.instructions.len()
        } else {
            max_iter
        };

        let mut candidates =
            HashSet::from_iter((0..(1 << 7)).map(|i| (i, Register { a: i, b: 0, c: 0 })));

        for (i, &output) in self.instructions.clone().iter().enumerate() {
            if i >= max_iter {
                break;
            }

            let mut new_candidates = HashSet::new();
            for (old_a, reg) in candidates {
                for a in 0..(1 << 3) {
                    let new_a = old_a + (a << (7 + 3 * i));
                    self.registers = reg.clone();
                    self.registers.a += a << 7;
                    let res = self.run(true);
                    let a = self.registers.a;
                    let end_reached = i == self.instructions.len() - 1;
                    if res.parse::<usize>().unwrap() == output
                        && (!end_reached || a == 0)
                        && (end_reached || a != 0)
                    {
                        new_candidates.insert((new_a, self.registers.clone()));
                    }
                }
            }

            candidates = new_candidates;
        }

        *candidates.iter().map(|(a, _)| a).min().unwrap()
    }
}

pub fn task01(input: &str) -> String {
    let mut computer = Computer::parse_input(input);
    let res = computer.run(false);
    println!("{:?}", computer);
    res
}

pub fn task02(input: &str) -> String {
    let mut computer = Computer::parse_input(input);
    if !computer.check_task02_assumptions() {
        return "Assumptions not met".to_string();
    }
    let res = computer.find_input(0);
    computer.registers = Register { a: res, b: 0, c: 0 };
    let forward = computer.run(false);
    println!("{:?}", forward);

    res.to_string()
}

#[cfg(test)]
mod tests {
    use super::super::fs_utils::{read_example, read_input};
    use super::*;

    #[test]
    fn test_task01() {
        let input = read_example(17, 1);
        assert_eq!(task01(&input), "4,6,3,5,6,3,5,2,1,0");
    }

    #[test]
    fn run_task01() {
        let input = read_input(17);
        assert_eq!(task01(&input), "5,1,3,4,3,7,2,1,7");
    }

    #[test]
    fn test_task02() {
        let input = read_example(17, 2);
        assert_eq!(task02(&input), "117440");
    }

    #[test]
    fn run_task02() {
        let input = read_input(17);
        assert_eq!(task02(&input), "216584205979245");
    }
}
