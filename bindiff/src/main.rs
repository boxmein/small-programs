use std::io::{BufReader, Read};

fn main() {
    let mut args = std::env::args();

    let left = args.next().expect("expected 1 cli argument");
    let right = args.next().expect("expected 2 cli arguments");

    print_compare(&left, &right);
}

fn print_compare(left: &str, right: &str) {
    let left = std::fs::File::open(left).expect("failed to open first file");
    let right = std::fs::File::open(right).expect("failed to open 2nd file");

    let mut left_buf = BufReader::new(left);
    let mut right_buf = BufReader::new(right);

    let mut left_bytes = [0; 512];
    let mut left_cur = 0;

    let mut right_bytes = [0; 512];
    let mut right_cur = 0;

    loop {
        match (left_buf.read(&mut left_bytes), right_buf.read(&mut right_bytes)) {
            (Ok(left_n), Ok(right_n)) => {
                left_cur += left_n;
                right_cur += right_n;
                let count = std::cmp::min(left_n, right_n);
                for i in 0..left_n {
                    if left_bytes[i] != right_bytes[i] {
                        println!("{:08x} is different (left {:02x} right {:02x})", left_cur + i, left_bytes[i], right_bytes[i]);
                    }
                }
            },
            _ => break,
        }
    }
}
