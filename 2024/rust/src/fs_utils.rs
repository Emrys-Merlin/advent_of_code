use std::env;
use std::path::Path;

pub fn get_example_path(day: usize, example: usize) -> String {
    let example_dir_env = env::var("EXAMPLE_DIR").unwrap_or("../examples".to_string());
    let example_dir = Path::new(&example_dir_env);
    example_dir
        .join(format!("day{:02}_{:02}.txt", day, example))
        .to_str()
        .unwrap()
        .to_string()
}

pub fn read_example(day: usize, example: usize) -> String {
    let example_path = get_example_path(day, example);
    std::fs::read_to_string(example_path).expect("Failed to read example file")
}

pub fn get_input_path(day: usize) -> String {
    let input_dir_env = env::var("INPUT_DIR").unwrap_or("../inputs".to_string());
    let input_dir = Path::new(&input_dir_env);
    input_dir
        .join(format!("day{:02}.txt", day))
        .to_str()
        .unwrap()
        .to_string()
}

pub fn read_input(day: usize) -> String {
    let input_path = get_input_path(day);
    std::fs::read_to_string(input_path).expect("Failed to read input file")
}
