use std::fs;

pub fn binary(input_file: &str) {
    let commands = fs::read_to_string(input_file).unwrap();
    let mut element_counts = [0; 12];
    for line in commands.split("\n") {
        if line != "" {
            for (i, ch) in line.chars().enumerate() {
                element_counts[i] += if ch == '0' { -1 } else { 1 }
            }
        }
    }

    println!("{:?}", element_counts);
    let mut gamma = 0;
    let mut epsilon = 0;
    for i in (0..12).rev() {
        println!("{}", 1 << (11 - i));
        if element_counts[i] > 0 {
            gamma += 1 << (11 - i);
        } else {
            epsilon += 1 << (11 - i);
        }
    }
    println!("gamma: {}, epsilon: {}", gamma, epsilon);
    println!("result: {}", gamma * epsilon);
}
