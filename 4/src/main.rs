use clap::Parser;
use regex::Regex;
use std::{error::Error, fs::read_to_string};

#[derive(Clone, Debug, PartialEq)]
struct Card {
    id: usize,
    winners: Vec<u32>,
    revealed: Vec<u32>,
}

fn parse_row(row_data: &str) -> Card {
    const ERR_MSG: &str = "Card data did not match expected pattern";
    let re = Regex::new(r"^Card +(\d+): ([\d ]+) \| ([\d ]+)$").expect("Card Regex could not compile");
    let caps = re.captures(row_data).expect(ERR_MSG);

    let id = caps.get(1).expect(ERR_MSG).as_str().parse::<usize>().expect("Could not parse card number");
    let winners = caps.get(2).expect(ERR_MSG)
        .as_str()
        .split_ascii_whitespace()
        .map(|num| num.parse::<u32>().expect("Could not parse number"))
        .collect();
    let revealed = caps.get(3).expect(ERR_MSG)
        .as_str()
        .split_ascii_whitespace()
        .map(|num| num.parse::<u32>().expect("Could not parse number"))
        .collect();

    Card {
        id,
        winners,
        revealed,
    }
}

fn get_overlap<'a>(this: &'a Vec<u32>, that: &'a Vec<u32>) -> Vec<&'a u32> {
    this.iter().filter(|num| that.contains(num)).collect()
}

fn calculate_points(winners: &Vec<&u32>) -> u32 {
    let mut points = 0;

    for _ in winners {
        points = match points {
            0 => 1,
            _ => points * 2,
        }
    }
    points
}

fn answer(input: &str) -> u32 {
    let card = parse_row(input);
    calculate_points(&get_overlap(&card.winners, &card.revealed))
}

fn aggregate<'a, I>(contents: I) -> u32
where
    I: Iterator<Item = &'a str>
{
    contents.map(|line| answer(line)).sum()
}

#[derive(Debug, Parser)]
struct Cli {
    file: std::path::PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let content = read_to_string(args.file).unwrap();
    let result = aggregate(content.lines());
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_row() {
        assert_eq!(
            parse_row("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"),
            Card {
                id: 1,
                winners: vec![41, 48, 83, 86, 17],
                revealed: vec![83, 86, 6, 31, 17, 9, 48, 53],
            }
        );
        assert_eq!(
            parse_row("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"),
            Card {
                id: 2,
                winners: vec![13, 32, 20, 16, 61],
                revealed: vec![61, 30, 68, 82, 17, 32, 24, 19],
            }
        );
        assert_eq!(
            parse_row("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"),
            Card {
                id: 3,
                winners: vec![1, 21, 53, 59, 44],
                revealed: vec![69, 82, 63, 72, 16, 21, 14, 1],
            }
        );
        assert_eq!(
            parse_row("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"),
            Card {
                id: 4,
                winners: vec![41, 92, 73, 84, 69],
                revealed: vec![59, 84, 76, 51, 58, 5, 54, 83],
            }
        );
        assert_eq!(
            parse_row("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"),
            Card {
                id: 5,
                winners: vec![87, 83, 26, 28, 32],
                revealed: vec![88, 30, 70, 12, 93, 22, 82, 36],
            }
        );
        assert_eq!(
            parse_row("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"),
            Card {
                id: 6,
                winners: vec![31, 18, 13, 56, 72],
                revealed: vec![74, 77, 10, 23, 35, 67, 36, 11],
            }
        );
    }

    #[test]
    fn test_get_overlap() {
        assert_eq!(
            get_overlap(
                &vec![41, 48, 83, 86, 17],
                &vec![83, 86, 6, 31, 17, 9, 48, 53],
            ),
            vec![&48, &83, &86, &17]
        );
        assert_eq!(
            get_overlap(
                &vec![13, 32, 20, 16, 61],
                &vec![61, 30, 68, 82, 17, 32, 24, 19],
            ),
            vec![&32, &61]
        );
        assert_eq!(
            get_overlap(
                &vec![1, 21, 53, 59, 44],
                &vec![69, 82, 63, 72, 16, 21, 14, 1],
            ),
            vec![&1, &21]
        );
        assert_eq!(
            get_overlap(
                &vec![41, 92, 73, 84, 69],
                &vec![59, 84, 76, 51, 58, 5, 54, 83],
            ),
            vec![&84]
        );
        assert_eq!(
            get_overlap(
                &vec![87, 83, 26, 28, 32],
                &vec![88, 30, 70, 12, 93, 22, 82, 36],
            ),
            Vec::<&u32>::new()
        );
        assert_eq!(
            get_overlap(
                &vec![31, 18, 13, 56, 72],
                &vec![74, 77, 10, 23, 35, 67, 36, 11],
            ),
            Vec::<&u32>::new()
        );
    }

    #[test]
    fn test_calculate_points() {
        assert_eq!(calculate_points(&vec![&48, &83, &86, &17]), 8);
        assert_eq!(calculate_points(&vec![&32, &61]), 2);
        assert_eq!(calculate_points(&vec![&1, &21]), 2);
        assert_eq!(calculate_points(&vec![&84]), 1);
        assert_eq!(calculate_points(&vec![]), 0);
    }

    #[test]
    fn test_answer() {
        assert_eq!(answer("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"), 8);
        assert_eq!(answer("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"), 2);
        assert_eq!(answer("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"), 2);
        assert_eq!(answer("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"), 1);
        assert_eq!(answer("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"), 0);
        assert_eq!(answer("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"), 0);
    }

    #[test]
    fn test_aggregate() {
        let input = vec![
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
        ];
        assert_eq!(aggregate(input.into_iter()), 13);
    }
}
