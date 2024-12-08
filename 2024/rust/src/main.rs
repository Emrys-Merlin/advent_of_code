use clap::Parser;
use std::env;
use std::path::Path;
use std::collections::HashMap;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;

pub fn get_task_map() -> HashMap<(i32, i32), fn(&str) -> String> {
    let mut task_map: HashMap<(i32, i32), fn(&str) -> String> = HashMap::new();
    task_map.insert((1, 1), day01::task01);
    task_map.insert((1, 2), day01::task02);
    task_map.insert((2, 1), day02::task01);
    task_map.insert((2, 2), day02::task02);
    task_map.insert((3, 1), day03::task01);
    task_map.insert((3, 2), day03::task02);
    task_map.insert((4, 1), day04::task01);
    task_map.insert((4, 2), day04::task02);
    task_map.insert((5, 1), day05::task01);
    task_map.insert((5, 2), day05::task02);
    task_map.insert((6, 1), day06::task01);
    task_map.insert((6, 2), day06::task02);
    task_map.insert((7, 1), day07::task01);
    task_map.insert((7, 2), day07::task02);
    task_map.insert((8, 1), day08::task01);
    task_map.insert((8, 2), day08::task02);
    task_map
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    // Day to run
    day: i32,

    // Task to run
    #[arg(default_value_t = 1)]
    task: i32,

    // Run example
    #[arg(short, long, default_value_t = 0)]
    example: i32,
}

fn main() {
    let task_map = get_task_map();

    let args = Cli::parse();

    let input_file = if args.example > 0 {
        let example_dir_env = env::var("EXAMPLE_DIR").unwrap_or("../examples".to_string());
        let example_dir = Path::new(&example_dir_env);
        example_dir.join(format!("day{:02}_{:02}.txt", args.day, args.example))
    } else {
        let input_dir_env = env::var("INPUT_DIR").unwrap_or("../inputs".to_string());
        let input_dir = Path::new(&input_dir_env);
        input_dir.join(format!("day{:02}.txt", args.day))
    };

    println!("Input file: {}", input_file.display());

    let input = std::fs::read_to_string(input_file).expect("Failed to read input file");

    let task = task_map.get(&(args.day, args.task)).expect("Task not found");
    let result = task(&input);
    println!("Result: {}", result);
}
