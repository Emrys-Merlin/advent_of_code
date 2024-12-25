use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Wire {
    True,
    False,
    Inactive,
}

impl Wire {
    pub fn from_str(wire: &str) -> Self {
        match wire {
            "1" => Wire::True,
            "0" => Wire::False,
            _ => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operator {
    And,
    Or,
    Xor,
}

impl Operator {
    pub fn from_str(operator: &str) -> Self {
        match operator {
            "AND" => Operator::And,
            "OR" => Operator::Or,
            "XOR" => Operator::Xor,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Gate {
    input1: String,
    input2: String,
    output: String,
    operator: Operator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Device {
    wires: HashMap<String, Wire>,
    gates: Vec<Gate>,
}

impl Device {
    pub fn parse_input(input: &str) -> Self {
        let mut first_part = true;
        let mut wires = HashMap::new();
        let mut gates = Vec::new();

        for line in input.lines() {
            if line.is_empty() {
                first_part = false;
                continue;
            }

            if first_part {
                let (left, right) = line.trim().split_once(": ").unwrap();
                wires.insert(left.to_string(), Wire::from_str(right));
            } else {
                let mut input1 = "".to_string();
                let mut input2 = "".to_string();
                let mut output = "".to_string();
                let mut operator = "";
                for (i, part) in line.split(" ").into_iter().enumerate() {
                    match i {
                        0 => {
                            input1 = part.to_string();
                        }
                        1 => {
                            operator = part;
                        }
                        2 => {
                            input2 = part.to_string();
                        }
                        3 => {}
                        4 => {
                            output = part.to_string();
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                }

                if !wires.contains_key(&output) {
                    wires.insert(output.clone(), Wire::Inactive);
                }
                gates.push(Gate {
                    input1,
                    input2,
                    output,
                    operator: Operator::from_str(operator),
                });
            }
        }

        Device { wires, gates }
    }

    fn run_gate(&self, gate: &Gate) -> Wire {
        let &input1 = self.wires.get(&gate.input1).unwrap();
        let &input2 = self.wires.get(&gate.input2).unwrap();

        if input1 == Wire::Inactive || input2 == Wire::Inactive {
            return Wire::Inactive;
        }

        let test = match gate.operator {
            Operator::And => input1 == Wire::True && input2 == Wire::True,
            Operator::Or => input1 == Wire::True || input2 == Wire::True,
            Operator::Xor => (input1 == Wire::True) ^ (input2 == Wire::True),
        };

        match test {
            true => Wire::True,
            false => Wire::False,
        }
    }

    fn read_output(&self) -> Option<usize> {
        let mut res = 0;

        for (name, wire) in self.wires.iter() {
            if !name.starts_with("z") || *wire == Wire::False {
                continue;
            }

            if *wire == Wire::Inactive {
                return None;
            }

            let shift = name.replace("z", "").parse::<usize>().unwrap();
            res += 1 << shift;
        }

        Some(res)
    }

    pub fn run(&mut self) -> usize {
        while !self.gates.is_empty() {
            let mut new_gates = Vec::new();
            for gate in self.gates.iter() {
                let output = self.run_gate(&gate);

                if output != Wire::Inactive {
                    self.wires.insert(gate.output.clone(), output);
                } else {
                    new_gates.push(gate.clone())
                }
            }

            self.gates = new_gates;
        }

        self.read_output().unwrap()
    }
}

pub fn task01(input: &str) -> String {
    let mut device = Device::parse_input(input);
    device.run().to_string()
}

pub fn task02(_input: &str) -> String {
    "Not implemented. Look at python graphviz approach".to_string()
}
