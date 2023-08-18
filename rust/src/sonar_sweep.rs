use std::fs;

const INPUT_FILE: &str = "inputs/sonar_sweep.txt";

pub fn sonar_sweep() {
    if let Ok(depths) = fs::read_to_string(INPUT_FILE) {
        let (_, total_depth_changes) =
            // Start with -1 since the "increase" from 0 depth to
            // first measurement is not a depth change
            depths.split("\n").fold((0, -1), |(prev, total), new_depth| {
                if new_depth.is_empty() {
                    return (prev, total);
                }
                let new_depth: i32 = new_depth.parse().unwrap();
                let new_total = if new_depth > prev { total + 1 } else { total };
                (new_depth, new_total)
            });
        println!("Total depth changes: {}", total_depth_changes);
    } else {
        panic!("Failed to open file");
    }
}
