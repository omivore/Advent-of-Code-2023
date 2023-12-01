use clap::Parser;
use std::{error::Error, fs::read_to_string};

fn extract_digits(calibration: String) -> Vec<i32>
{
    calibration.chars()
        .filter(|c| c.is_ascii_digit())
        .map(|c| c.to_digit(10))
        .flatten()
        .map(|u| u as i32)
        .collect()
}

fn answer(input: String) -> i32 {
    let digits = extract_digits(input);
    match (digits.first(), digits.last()) {
        (Some(x), Some(y)) => x * 10 + y,
        _ => panic!("Could not retrieve desired digits.")
    }
}

fn aggregate<I>(contents: I) -> i32
where
    I: Iterator<Item = String>
{
    contents.map(|calibration| answer(calibration)).sum()
}

#[derive(Debug, Parser)]
struct Cli {
    file: std::path::PathBuf,
    #[arg(default_value = None)]
    length: Option<usize>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let content = read_to_string(&args.file).unwrap();
    let calibrations: Vec<String> = content.lines().map(|line| line.to_string()).collect();
    let result = aggregate(calibrations.into_iter());
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn digits() {
        assert_eq!(extract_digits("1abc2".to_string()), vec![1, 2]);
        assert_eq!(extract_digits("pqr3stu8vwx".to_string()), vec![3, 8]);
        assert_eq!(extract_digits("a1b2c3d4e5f".to_string()), vec![1, 2, 3, 4, 5]);
        assert_eq!(extract_digits("treb7uchet".to_string()), vec![7]);
    }

    #[test]
    fn sample() {
        assert_eq!(answer("1abc2".to_string()), 12);
        assert_eq!(answer("pqr3stu8vwx".to_string()), 38);
        assert_eq!(answer("a1b2c3d4e5f".to_string()), 15);
        assert_eq!(answer("treb7uchet".to_string()), 77);
    }

    #[test]
    fn total() {
        let input = vec![
            "1abc2".to_string(),
            "pqr3stu8vwx".to_string(),
            "a1b2c3d4e5f".to_string(),
            "treb7uchet".to_string()
        ];
        assert_eq!(aggregate(input.into_iter()), 142);
    }
}
