use clap::Parser;
use std::{error::Error, fs::read_to_string, iter::from_fn, iter::once};

#[derive(Clone, Debug, PartialEq)]
enum Content {
    Symbol(char),
    Number(i32),
}
#[derive(Clone, Debug, PartialEq)]
struct Position {
    row: i32,
    columns: Vec<i32>,
}
#[derive(Clone, Debug, PartialEq)]
struct Artifact {
    content: Content,
    position: Position,
}

fn parse_row(row_number: i32, row_data: &str) -> Vec<Artifact> {
    let mut iter = row_data.chars().enumerate().peekable();
    let mut artifacts = Vec::<Artifact>::new();
    while let Some((i, ch)) = iter.next() {
        let col = i.try_into().unwrap();
        match ch {
            '.' => continue,
            '0'..='9' => {
                let digit_text = once(ch)
                    .chain(from_fn(|| iter.by_ref().next_if(|(_, s)| s.is_ascii_digit()).map(|(_, s)| s)))
                    .collect::<String>();
                let n = digit_text
                    .parse()
                    .unwrap();
                artifacts.push(
                    Artifact {
                        content: Content::Number(n),
                        position: Position {
                            row: row_number,
                            columns: (col..col + digit_text.chars().count() as i32).collect(),
                        }
                    }
                );
            },
            _ => {
                artifacts.push(
                    Artifact {
                        content: Content::Symbol(ch),
                        position: Position {
                            row: row_number,
                            columns: vec![col],
                        }
                    }
                );
            }
        }
    }

    artifacts
}

fn get_symbols(all: &Vec<Artifact>) -> Vec<&Artifact> {
    all.into_iter().filter(|art| matches!(art.content, Content::Symbol(_))).collect()
}

fn get_parts_adjacent_to<'a>(all: &'a Vec<Artifact>, pos: &'a Position) -> &'a Vec<Artifact> {
    all
}

fn get_parts_adjacent_to<'a>(all: &'a Vec<Artifact>, pos: &'a Position) -> Vec<&'a Artifact> {
    let adjacent_left: i32 = pos.columns.iter().min().unwrap() - 1;
    let adjacent_right: i32 = pos.columns.iter().max().unwrap() + 1;
    all.into_iter()
        .filter(|art| matches!(art.content, Content::Number(_)))
        .filter(|art| (art.position.row - pos.row).abs() <= 1)
        .filter(|art| art.position.columns.iter().min().unwrap() <= &adjacent_right &&
                      art.position.columns.iter().max().unwrap() >= &adjacent_left
        )
        .collect()
}

fn aggregate<'a, I>(contents: I) -> u32
where
    I: Iterator<Item = &'a str>
{
    let all = contents.enumerate().map(|(i, line)| parse_row(i.try_into().unwrap(), line)).flatten().collect();
    let mut parts: Vec<&Artifact> = vec![];
    for symbol in get_symbols(&all) {
        parts.extend_from_slice(&get_parts_adjacent_to(&all, &symbol.position));
    }

    let mut sum = 0;
    for part in parts {
        if let Content::Number(part_number) = part.content {
            sum += part_number
        }
    }
    sum
}

#[derive(Debug, Parser)]
struct Cli {
    file: std::path::PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let content = read_to_string(&args.file).unwrap();
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
            parse_row(0, "467..114.."),
            vec![
                Artifact {
                    content: Content::Number(467),
                    position: Position { row: 0, columns: vec![0, 1, 2] },
                },
                Artifact {
                    content: Content::Number(114),
                    position: Position { row: 0, columns: vec![5, 6, 7] },
                },
            ]
        );
        assert_eq!(
            parse_row(1, "...*......"),
            vec![
                Artifact {
                    content: Content::Symbol('*'),
                    position: Position { row: 1, columns: vec![3] },
                },
            ]
        );
        assert_eq!(
            parse_row(2, "..35..633."),
            vec![
                Artifact {
                    content: Content::Number(35),
                    position: Position { row: 2, columns: vec![2, 3] },
                },
                Artifact {
                    content: Content::Number(633),
                    position: Position { row: 2, columns: vec![6, 7, 8] },
                },
            ]
        );
        assert_eq!(
            parse_row(3, "......#..."),
            vec![
                Artifact {
                    content: Content::Symbol('#'),
                    position: Position { row: 3, columns: vec![6] },
                },
            ]
        );
        assert_eq!(
            parse_row(4, "617*......"),
            vec![
                Artifact {
                    content: Content::Number(617),
                    position: Position { row: 4, columns: vec![0, 1, 2] },
                },
                Artifact {
                    content: Content::Symbol('*'),
                    position: Position { row: 4, columns: vec![3] },
                },
            ]
        );
        assert_eq!(
            parse_row(5, ".....+.58."),
            vec![
                Artifact {
                    content: Content::Symbol('+'),
                    position: Position { row: 5, columns: vec![5] },
                },
                Artifact {
                    content: Content::Number(58),
                    position: Position { row: 5, columns: vec![7, 8] },
                },
            ]
        );
        assert_eq!(
            parse_row(6, "..592....."),
            vec![
                Artifact {
                    content: Content::Number(592),
                    position: Position { row: 6, columns: vec![2, 3, 4] },
                },
            ]
        );
        assert_eq!(
            parse_row(7, "...$.*...."),
            vec![
                Artifact {
                    content: Content::Symbol('$'),
                    position: Position { row: 7, columns: vec![3] },
                },
                Artifact {
                    content: Content::Symbol('*'),
                    position: Position { row: 7, columns: vec![5] },
                },
            ]
        );
        assert_eq!(
            parse_row(8, ".664.598.."),
            vec![
                Artifact {
                    content: Content::Number(664),
                    position: Position { row: 8, columns: vec![1, 2, 3] },
                },
                Artifact {
                    content: Content::Number(598),
                    position: Position { row: 8, columns: vec![5, 6, 7] },
                },
            ]
        );
    }

    #[test]
    fn test_get_symbols() {
        let all = vec![
            Artifact {
                content: Content::Number(467),
                position: Position { row: 0, columns: vec![0, 1, 2] },
            },
            Artifact {
                content: Content::Number(114),
                position: Position { row: 0, columns: vec![5, 6, 7] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 1, columns: vec![3] },
            },
            Artifact {
                content: Content::Number(35),
                position: Position { row: 2, columns: vec![2, 3] },
            },
            Artifact {
                content: Content::Number(633),
                position: Position { row: 2, columns: vec![6, 7, 8] },
            },
            Artifact {
                content: Content::Symbol('#'),
                position: Position { row: 3, columns: vec![6] },
            },
            Artifact {
                content: Content::Number(617),
                position: Position { row: 4, columns: vec![0, 1, 2] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 4, columns: vec![3] },
            },
            Artifact {
                content: Content::Symbol('+'),
                position: Position { row: 5, columns: vec![5] },
            },
            Artifact {
                content: Content::Number(58),
                position: Position { row: 5, columns: vec![7, 8] },
            },
            Artifact {
                content: Content::Number(592),
                position: Position { row: 6, columns: vec![2, 3, 4] },
            },
            Artifact {
                content: Content::Symbol('$'),
                position: Position { row: 7, columns: vec![3] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 7, columns: vec![5] },
            },
            Artifact {
                content: Content::Number(664),
                position: Position { row: 8, columns: vec![1, 2, 3] },
            },
            Artifact {
                content: Content::Number(598),
                position: Position { row: 8, columns: vec![5, 6, 7] },
            },
        ];
        let symbols = vec![
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 1, columns: vec![3] },
            },
            Artifact {
                content: Content::Symbol('#'),
                position: Position { row: 3, columns: vec![6] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 4, columns: vec![3] },
            },
            Artifact {
                content: Content::Symbol('+'),
                position: Position { row: 5, columns: vec![5] },
            },
            Artifact {
                content: Content::Symbol('$'),
                position: Position { row: 7, columns: vec![3] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 7, columns: vec![5] },
            },
        ];
        assert_eq!(get_symbols(&all), &symbols);
    }

    #[test]
    fn test_get_parts_adjacent_to() {
        let all = vec![
            Artifact {
                content: Content::Number(467),
                position: Position { row: 0, columns: vec![0, 1, 2] },
            },
            Artifact {
                content: Content::Number(114),
                position: Position { row: 0, columns: vec![5, 6, 7] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 1, columns: vec![3] },
            },
            Artifact {
                content: Content::Number(35),
                position: Position { row: 2, columns: vec![2, 3] },
            },
            Artifact {
                content: Content::Number(633),
                position: Position { row: 2, columns: vec![6, 7, 8] },
            },
            Artifact {
                content: Content::Symbol('#'),
                position: Position { row: 3, columns: vec![6] },
            },
            Artifact {
                content: Content::Number(617),
                position: Position { row: 4, columns: vec![0, 1, 2] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 4, columns: vec![3] },
            },
            Artifact {
                content: Content::Symbol('+'),
                position: Position { row: 5, columns: vec![5] },
            },
            Artifact {
                content: Content::Number(58),
                position: Position { row: 5, columns: vec![7, 8] },
            },
            Artifact {
                content: Content::Number(592),
                position: Position { row: 6, columns: vec![2, 3, 4] },
            },
            Artifact {
                content: Content::Symbol('$'),
                position: Position { row: 7, columns: vec![3] },
            },
            Artifact {
                content: Content::Symbol('*'),
                position: Position { row: 7, columns: vec![5] },
            },
            Artifact {
                content: Content::Number(664),
                position: Position { row: 8, columns: vec![1, 2, 3] },
            },
            Artifact {
                content: Content::Number(598),
                position: Position { row: 8, columns: vec![5, 6, 7] },
            },
        ];
        assert_eq!(
            get_parts_adjacent_to(&all, &Position { row: 1, columns: vec![3] }),
            vec![
                Artifact {
                    content: Content::Number(467),
                    position: Position { row: 0, columns: vec![0, 1, 2] },
                },
                Artifact {
                    content: Content::Number(35),
                    position: Position { row: 2, columns: vec![2, 3] },
                },
            ].iter().collect::<Vec<_>>()
        );
        assert_eq!(
            get_parts_adjacent_to(&all, &Position { row: 3, columns: vec![6] }),
            vec![
                Artifact {
                    content: Content::Number(633),
                    position: Position { row: 2, columns: vec![6, 7, 8] },
                },
            ].iter().collect::<Vec<_>>()
        );
        assert_eq!(
            get_parts_adjacent_to(&all, &Position { row: 4, columns: vec![3] }),
            vec![
                Artifact {
                    content: Content::Number(617),
                    position: Position { row: 4, columns: vec![0, 1, 2] },
                },
            ].iter().collect::<Vec<_>>()
        );
        assert_eq!(
            get_parts_adjacent_to(&all, &Position { row: 5, columns: vec![5] }),
            vec![
                Artifact {
                    content: Content::Number(592),
                    position: Position { row: 6, columns: vec![2, 3, 4] },
                },
            ].iter().collect::<Vec<_>>()
        );
        assert_eq!(
            get_parts_adjacent_to(&all, &Position { row: 7, columns: vec![3] }),
            vec![
                Artifact {
                    content: Content::Number(592),
                    position: Position { row: 6, columns: vec![2, 3, 4] },
                },
                Artifact {
                    content: Content::Number(664),
                    position: Position { row: 8, columns: vec![1, 2, 3] },
                },
            ].iter().collect::<Vec<_>>()
        );
        assert_eq!(
            get_parts_adjacent_to(&all, &Position { row: 7, columns: vec![5] }),
            vec![
                Artifact {
                    content: Content::Number(592),
                    position: Position { row: 6, columns: vec![2, 3, 4] },
                },
                Artifact {
                    content: Content::Number(598),
                    position: Position { row: 8, columns: vec![5, 6, 7] },
                },
            ].iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_aggregate() {
        let input = vec![
            "467..114..",
            "...*......",
            "..35..633.",
            "......#...",
            "617*......",
            ".....+.58.",
            "..592.....",
            "......755.",
            "...$.*....",
            ".664.598..",
        ];
        assert_eq!(aggregate(input.into_iter()), 4361);
    }
}
