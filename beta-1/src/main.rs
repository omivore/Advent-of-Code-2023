use clap::Parser;
use std::{error::Error, fs::read_to_string};

fn parse_backpacks(input: String) -> Vec<Vec<i32>> {
    let mut backpacks: Vec<Vec<i32>> = vec![];
    let mut current_backpack: Vec<i32> = vec![];

    for line in input.lines() {
        match line {
            calories if line.parse::<i32>().is_ok() => {
                current_backpack.push(calories.parse::<i32>().unwrap());
            },
            "" => {
                backpacks.push(current_backpack);
                current_backpack = vec![];
            },
            _ => panic!("Unrecognized input: \"{}\"", line),
        }
    }
    backpacks.push(current_backpack);
    backpacks
}

fn sort_and_sum<I>(backpack: I, top: Option<usize>) -> i32
where
    I: Iterator<Item = i32>
{
    let mut vec = backpack.collect::<Vec<_>>();
    vec.sort_unstable_by_key(|x| -x);
    if let Some(n) = top {
        vec.truncate(n);
    }
    vec.iter().sum()
}

fn answer(input: String, top: Option<usize>) -> i32 {
    let backpacks = parse_backpacks(input);
    let mut sorted_backpacks = backpacks.into_iter().map(|backpack| sort_and_sum(backpack.into_iter(), None));
    sort_and_sum(&mut sorted_backpacks as &mut dyn Iterator<Item = i32>, top)
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
    let result = answer(content, args.length);
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sums() {
        let input = vec![1, 2, 3, 4, 5];
        assert_eq!(sort_and_sum(input.into_iter(), None), 15);
    }

    #[test]
    fn sample() {
        let input = "1000\n\
                     2000\n\
                     3000\n\
                     \n\
                     4000\n\
                     \n\
                     5000\n\
                     6000\n\
                     \n\
                     7000\n\
                     8000\n\
                     9000\n\
                     \n\
                     10000".to_string();
        assert_eq!(parse_backpacks(input),
            vec![
                vec![1000, 2000, 3000],
                vec![4000],
                vec![5000, 6000],
                vec![7000, 8000, 9000],
                vec![10000],
            ]
        );
    }
}
