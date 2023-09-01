mod binary;
mod dive;
mod sonar_sweep;

const DIVE_INPUT_FILE: &str = "inputs/dive.txt";
const BINARY_INPUT: &str = "inputs/binary.txt";

fn main() {
    //sonar_sweep::sonar_sweep();
    //dive::dive(DIVE_INPUT_FILE)
    binary::binary(BINARY_INPUT);
}
