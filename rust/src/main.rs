mod dive;
mod sonar_sweep;

const DIVE_INPUT_FILE: &str = "inputs/dive.txt";

fn main() {
    //sonar_sweep::sonar_sweep();
    dive::dive(DIVE_INPUT_FILE)
}
