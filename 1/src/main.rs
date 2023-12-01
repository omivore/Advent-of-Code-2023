use clap::Parser;
use std::{error::Error, fs::read_to_string};

fn extract_digits(calibration: String, read_letters: bool) -> Vec<i32> {
    let mut tokens: Vec<i32> = Vec::new();

    let mut chars = calibration.as_str();
    while !chars.is_empty() {
        let next_token: Option<i32> = match chars {
            num if num.starts_with(|c: char| c.is_ascii_digit()) => {
                let digit = chars[0..1].parse::<i32>().ok();
                chars = &chars[1..];
                digit
            }
            num if read_letters && num.starts_with("one")   => {
                chars = &chars[3..];
                Some(1)
            }
            num if read_letters && num.starts_with("two")   => {
                chars = &chars[3..];
                Some(2)
            }
            num if read_letters && num.starts_with("three") => {
                chars = &chars[5..];
                Some(3)
            }
            num if read_letters && num.starts_with("four")  => {
                chars = &chars[4..];
                Some(4)
            }
            num if read_letters && num.starts_with("five")  => {
                chars = &chars[4..];
                Some(5)
            }
            num if read_letters && num.starts_with("six")   => {
                chars = &chars[3..];
                Some(6)
            }
            num if read_letters && num.starts_with("seven") => {
                chars = &chars[5..];
                Some(7)
            }
            num if read_letters && num.starts_with("eight") => {
                chars = &chars[5..];
                Some(8)
            }
            num if read_letters && num.starts_with("nine")  => {
                chars = &chars[4..];
                Some(9)
            }
            _   => {
                chars = &chars[1..];
                None
            }
        };
        match next_token {
            Some(token) => tokens.push(token),
            None        => (),
        };
    }

    tokens
}

fn answer(input: String, read_letters: bool) -> i32 {
    let digits = extract_digits(input, read_letters);
    match (digits.first(), digits.last()) {
        (Some(x), Some(y)) => {println!("{}", x * 10 + y); x * 10 + y},
        _ => panic!("Could not retrieve desired digits.")
    }
}

fn aggregate<I>(contents: I, read_letters: bool) -> i32
where
    I: Iterator<Item = String>
{
    contents.map(|calibration| answer(calibration, read_letters)).sum()
}

#[derive(Debug, Parser)]
struct Cli {
    file: std::path::PathBuf,
    #[arg(short, long)]
    letters: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let content = read_to_string(&args.file).unwrap();
    let calibrations: Vec<String> = content.lines().map(|line| line.to_string()).collect();
    let result = aggregate(calibrations.into_iter(), args.letters);
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn digits() {
        assert_eq!(extract_digits("1abc2".to_string(), false), vec![1, 2]);
        assert_eq!(extract_digits("pqr3stu8vwx".to_string(), false), vec![3, 8]);
        assert_eq!(extract_digits("a1b2c3d4e5f".to_string(), false), vec![1, 2, 3, 4, 5]);
        assert_eq!(extract_digits("treb7uchet".to_string(), false), vec![7]);

        assert_eq!(extract_digits("two1nine".to_string(), true), vec![2, 1, 9]);
        assert_eq!(extract_digits("eighttwothree".to_string(), true), vec![8, 2, 3]);
        assert_eq!(extract_digits("abcone2threexyz".to_string(), true), vec![1, 2, 3]);
        assert_eq!(extract_digits("xtwone3four".to_string(), true), vec![2, 3, 4]);
        assert_eq!(extract_digits("4nineeightseven2".to_string(), true), vec![4, 9, 8, 7, 2]);
        assert_eq!(extract_digits("zoneight234".to_string(), true), vec![1, 2, 3, 4]);
        assert_eq!(extract_digits("7pqrstsixteen".to_string(), true), vec![7, 6]);
    }

    #[test]
    fn sample() {
        assert_eq!(answer("1abc2".to_string(), false), 12);
        assert_eq!(answer("pqr3stu8vwx".to_string(), false), 38);
        assert_eq!(answer("a1b2c3d4e5f".to_string(), false), 15);
        assert_eq!(answer("treb7uchet".to_string(), false), 77);

        assert_eq!(answer("two1nine".to_string(), true), 29);
        assert_eq!(answer("eighttwothree".to_string(), true), 83);
        assert_eq!(answer("abcone2threexyz".to_string(), true), 13);
        assert_eq!(answer("xtwone3four".to_string(), true), 24);
        assert_eq!(answer("4nineeightseven2".to_string(), true), 42);
        assert_eq!(answer("zoneight234".to_string(), true), 14);
        assert_eq!(answer("7pqrstsixteen".to_string(), true), 76);
    }

    #[test]
    fn total() {
        let input = vec![
            "1abc2".to_string(),
            "pqr3stu8vwx".to_string(),
            "a1b2c3d4e5f".to_string(),
            "treb7uchet".to_string()
        ];
        assert_eq!(aggregate(input.into_iter(), false), 142);

        let input = vec![
            "two1nine".to_string(),
            "eighttwothree".to_string(),
            "abcone2threexyz".to_string(),
            "xtwone3four".to_string(),
            "4nineeightseven2".to_string(),
            "zoneight234".to_string(),
            "7pqrstsixteen".to_string(),
        ];
        assert_eq!(aggregate(input.into_iter(), true), 281);
    }
}
