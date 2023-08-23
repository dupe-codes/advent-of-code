use std::fs;

pub fn dive(input_file: &str) {
    if let Ok(commands) = fs::read_to_string(input_file) {
        let (horizontal_pos, depth) =
            commands
                .split("\n")
                .fold((0, 0), |(horizontal_pos, depth), command| {
                    if command.is_empty() {
                        return (horizontal_pos, depth);
                    }
                    let (cmd, amt) = command.split_once(' ').unwrap();
                    let amt = amt.parse::<i32>().unwrap();
                    match cmd {
                        "forward" => (horizontal_pos + amt, depth),
                        "up" => (horizontal_pos, depth - amt),
                        "down" => (horizontal_pos, depth + amt),
                        _ => panic!("Unknown command"),
                    }
                });
        println!("Multiplied result: {}", horizontal_pos * depth)
    } else {
        panic!("Failed to open file");
    }
}
