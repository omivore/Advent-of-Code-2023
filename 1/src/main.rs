use clap::Parser;
use std::{error::Error, fs::read_to_string};

fn get_first(calibration: &String, read_letters: bool) -> Option<i32> {
    let mut chars = calibration.as_str();
    while !chars.is_empty() {
        match chars {
            num if num.starts_with(|c: char| c.is_ascii_digit()) => {
                return chars[0..1].parse::<i32>().ok()
            }
            num if read_letters && num.starts_with("one")   => return Some(1),
            num if read_letters && num.starts_with("two")   => return Some(2),
            num if read_letters && num.starts_with("three") => return Some(3),
            num if read_letters && num.starts_with("four")  => return Some(4),
            num if read_letters && num.starts_with("five")  => return Some(5),
            num if read_letters && num.starts_with("six")   => return Some(6),
            num if read_letters && num.starts_with("seven") => return Some(7),
            num if read_letters && num.starts_with("eight") => return Some(8),
            num if read_letters && num.starts_with("nine")  => return Some(9),
            _ => {
                chars = &chars[1..];
                continue
            }
        };
    }
    None
}

fn get_last(calibration: &String, read_letters: bool) -> Option<i32> {
    let mut chars = calibration.as_str();
    while !chars.is_empty() {
        let i = chars.len();
        match chars {
            num if num.ends_with(|c: char| c.is_ascii_digit()) => {
                return chars[i-1..].parse::<i32>().ok()
            }
            num if read_letters && num.ends_with("one")   => return Some(1),
            num if read_letters && num.ends_with("two")   => return Some(2),
            num if read_letters && num.ends_with("three") => return Some(3),
            num if read_letters && num.ends_with("four")  => return Some(4),
            num if read_letters && num.ends_with("five")  => return Some(5),
            num if read_letters && num.ends_with("six")   => return Some(6),
            num if read_letters && num.ends_with("seven") => return Some(7),
            num if read_letters && num.ends_with("eight") => return Some(8),
            num if read_letters && num.ends_with("nine")  => return Some(9),
            _ => {
                chars = &chars[..i-1];
                continue
            }
        };
    }
    None
}

fn answer(input: String, read_letters: bool) -> i32 {
    let first = get_first(&input, read_letters);
    let last = get_last(&input, read_letters);
    match (first, last) {
        (Some(x), Some(y)) => x * 10 + y,
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
        assert_eq!(get_first(&"1abc2".to_string(), false), Some(1));
        assert_eq!(get_first(&"pqr3stu8vwx".to_string(), false), Some(3));
        assert_eq!(get_first(&"a1b2c3d4e5f".to_string(), false), Some(1));
        assert_eq!(get_first(&"treb7uchet".to_string(), false), Some(7));

        assert_eq!(get_last(&"1abc2".to_string(), false), Some(2));
        assert_eq!(get_last(&"pqr3stu8vwx".to_string(), false), Some(8));
        assert_eq!(get_last(&"a1b2c3d4e5f".to_string(), false), Some(5));
        assert_eq!(get_last(&"treb7uchet".to_string(), false), Some(7));


        assert_eq!(get_first(&"two1nine".to_string(), true), Some(2));
        assert_eq!(get_first(&"eighttwothree".to_string(), true), Some(8));
        assert_eq!(get_first(&"abcone2threexyz".to_string(), true), Some(1));
        assert_eq!(get_first(&"xtwone3four".to_string(), true), Some(2));
        assert_eq!(get_first(&"4nineeightseven2".to_string(), true), Some(4));
        assert_eq!(get_first(&"zoneight234".to_string(), true), Some(1));
        assert_eq!(get_first(&"7pqrstsixteen".to_string(), true), Some(7));

        assert_eq!(get_last(&"two1nine".to_string(), true), Some(9));
        assert_eq!(get_last(&"eighttwothree".to_string(), true), Some(3));
        assert_eq!(get_last(&"abcone2threexyz".to_string(), true), Some(3));
        assert_eq!(get_last(&"xtwone3four".to_string(), true), Some(4));
        assert_eq!(get_last(&"4nineeightseven2".to_string(), true), Some(2));
        assert_eq!(get_last(&"zoneight234".to_string(), true), Some(4));
        assert_eq!(get_last(&"7pqrstsixteen".to_string(), true), Some(6));
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
