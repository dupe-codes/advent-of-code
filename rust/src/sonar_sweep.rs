use std::fs;

const INPUT_FILE: &str = "inputs/sonar_sweep.txt";

pub fn sonar_sweep() {
    if let Ok(depths) = fs::read_to_string(INPUT_FILE) {
        let mut window: Vec<i32> = Vec::new();
        let (_, total_depth_changes) =
            depths
                .split("\n")
                .fold((&mut window, 0), |(window, total), new_depth| {
                    if new_depth.is_empty() {
                        return (window, total);
                    }
                    let new_depth: i32 = new_depth.parse().unwrap();
                    if window.len() < 3 {
                        window.push(new_depth);
                        return (window, total);
                    } else {
                        let prev_sum: i32 = window.iter().sum();
                        window.remove(0);
                        window.push(new_depth);
                        let new_sum: i32 = window.iter().sum();
                        let new_total = if new_sum > prev_sum { total + 1 } else { total };
                        (window, new_total)
                    }
                });
        println!("Total depth changes! : {}", total_depth_changes);
    } else {
        panic!("Failed to open file");
    }
}
