use clap::Parser;
use std::{error::Error, fs::read_to_string, iter::successors, ops::Not};

fn extract_digits(calibration: &str, read_letters: bool) -> Vec<i32> {
    let mut tokens: Vec<i32> = Vec::new();

    let iterator = successors(
        calibration.is_empty().not().then_some(calibration),
        |chars| chars.get(1..)
    );
    for chars in iterator {
        match chars {
            num if num.starts_with(|c: char| c.is_ascii_digit()) => {
                if let Ok(digit) = chars[..1].parse::<i32>() { tokens.push(digit); }
            }
            num if read_letters && num.starts_with("one")   => tokens.push(1),
            num if read_letters && num.starts_with("two")   => tokens.push(2),
            num if read_letters && num.starts_with("three") => tokens.push(3),
            num if read_letters && num.starts_with("four")  => tokens.push(4),
            num if read_letters && num.starts_with("five")  => tokens.push(5),
            num if read_letters && num.starts_with("six")   => tokens.push(6),
            num if read_letters && num.starts_with("seven") => tokens.push(7),
            num if read_letters && num.starts_with("eight") => tokens.push(8),
            num if read_letters && num.starts_with("nine")  => tokens.push(9),
            _   => ()
        };
    }

    tokens
}

fn answer(input: &str, read_letters: bool) -> Option<i32> {
    let digits = extract_digits(input, read_letters);
    match (digits.first(), digits.last()) {
        (Some(x), Some(y)) => Some(x * 10 + y),
        _ => None
    }
}

fn aggregate<'a, I>(contents: I, read_letters: bool) -> i32
where
    I: Iterator<Item = &'a str>
{
    contents.map(|calibration| answer(calibration, read_letters).unwrap_or(0)).sum()
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
    let result = aggregate(content.lines(), args.letters);
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn digits() {
        assert_eq!(extract_digits("1abc2", false), vec![1, 2]);
        assert_eq!(extract_digits("pqr3stu8vwx", false), vec![3, 8]);
        assert_eq!(extract_digits("a1b2c3d4e5f", false), vec![1, 2, 3, 4, 5]);
        assert_eq!(extract_digits("treb7uchet", false), vec![7]);

        assert_eq!(extract_digits("two1nine", true), vec![2, 1, 9]);
        assert_eq!(extract_digits("eighttwothree", true), vec![8, 2, 3]);
        assert_eq!(extract_digits("abcone2threexyz", true), vec![1, 2, 3]);
        assert_eq!(extract_digits("xtwone3four", true), vec![2, 1, 3, 4]);
        assert_eq!(extract_digits("4nineeightseven2", true), vec![4, 9, 8, 7, 2]);
        assert_eq!(extract_digits("zoneight234", true), vec![1, 8, 2, 3, 4]);
        assert_eq!(extract_digits("7pqrstsixteen", true), vec![7, 6]);
    }

    #[test]
    fn sample() {
        assert_eq!(answer("1abc2", false), Some(12));
        assert_eq!(answer("pqr3stu8vwx", false), Some(38));
        assert_eq!(answer("a1b2c3d4e5f", false), Some(15));
        assert_eq!(answer("treb7uchet", false), Some(77));

        assert_eq!(answer("two1nine", true), Some(29));
        assert_eq!(answer("eighttwothree", true), Some(83));
        assert_eq!(answer("abcone2threexyz", true), Some(13));
        assert_eq!(answer("xtwone3four", true), Some(24));
        assert_eq!(answer("4nineeightseven2", true), Some(42));
        assert_eq!(answer("zoneight234", true), Some(14));
        assert_eq!(answer("7pqrstsixteen", true), Some(76));
    }

    #[test]
    fn total() {
        let input = vec![
            "1abc2",
            "pqr3stu8vwx",
            "a1b2c3d4e5f",
            "treb7uchet"
        ];
        assert_eq!(aggregate(input.into_iter(), false), 142);

        let input = vec![
            "two1nine",
            "eighttwothree",
            "abcone2threexyz",
            "xtwone3four",
            "4nineeightseven2",
            "zoneight234",
            "7pqrstsixteen",
        ];
        assert_eq!(aggregate(input.into_iter(), true), 281);
    }
}
