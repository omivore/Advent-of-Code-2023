use clap::Parser;
use std::{collections::HashMap, error::Error, fs::read_to_string};

type Cubes = HashMap<String, u8>;
type GameId = u8;
#[derive(Debug, PartialEq)]
struct Game {
    id: GameId,
    reveals: Vec<Cubes>,
}

fn parse_game(game_data: &str) -> Game {
    Game {
        id: 1,
        reveals: vec![Cubes::from([("red".to_string(), 5)])],
    }
}

impl Game {
    fn is_possible_with(&self, bag: &Cubes) -> bool {
        for reveal in &self.reveals {
            for (color, count) in reveal.iter() {
                if !bag.get(color).is_some_and(|held| held >= count) {
                    return false
                }
            }
        }
        true
    }
}

fn answer(input: &str, bag: &Cubes) -> Option<GameId> {
    let game = parse_game(input);
    game.is_possible_with(bag).then(|| game.id)
}

fn aggregate<'a, I>(contents: I, bag: &Cubes) -> u8
where
    I: Iterator<Item = &'a str>
{
    contents.map(|line| answer(line, bag).unwrap_or(0)).sum()
}

#[derive(Debug, Parser)]
struct Cli {
    file: std::path::PathBuf,
    #[arg(short, long)]
    red: u8,
    #[arg(short, long)]
    green: u8,
    #[arg(short, long)]
    blue: u8,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let content = read_to_string(&args.file).unwrap();
    let bag = Cubes::from([
        ("red".to_string(), args.red),
        ("green".to_string(), args.green),
        ("blue".to_string(), args.blue),
    ]);
    let result = aggregate(content.lines(), &bag);
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        assert_eq!(
            parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"),
            Game {
                id: 1,
                reveals: vec![
                    Cubes::from([("red".to_string(), 4), ("blue".to_string(), 3)]),
                    Cubes::from([("red".to_string(), 1), ("green".to_string(), 2), ("blue".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 2)]),
                ],
            }
        );
        assert_eq!(
            parse_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"),
            Game {
                id: 2,
                reveals: vec![
                    Cubes::from([("blue".to_string(), 1), ("green".to_string(), 2)]),
                    Cubes::from([("green".to_string(), 3), ("blue".to_string(), 4), ("red".to_string(), 1)]),
                    Cubes::from([("green".to_string(), 1), ("blue".to_string(), 1)]),
                ],
            }
        );
        assert_eq!(
            parse_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"),
            Game {
                id: 3,
                reveals: vec![
                    Cubes::from([("green".to_string(), 8), ("blue".to_string(), 6), ("red".to_string(), 20)]),
                    Cubes::from([("blue".to_string(), 5), ("red".to_string(), 4), ("green".to_string(), 13)]),
                    Cubes::from([("green".to_string(), 5), ("red".to_string(), 1)]),
                ],
            }
        );
        assert_eq!(
            parse_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"),
            Game {
                id: 4,
                reveals: vec![
                    Cubes::from([("green".to_string(), 1), ("red".to_string(), 3), ("blue".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 3), ("red".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 3), ("blue".to_string(), 15), ("red".to_string(), 14)]),
                ],
            }
        );
        assert_eq!(
            parse_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"),
            Game {
                id: 5,
                reveals: vec![
                    Cubes::from([("red".to_string(), 6), ("blue".to_string(), 1), ("green".to_string(), 3)]),
                    Cubes::from([("blue".to_string(), 2), ("red".to_string(), 1), ("green".to_string(), 2)]),
                ],
            }
        );
    }

    #[test]
    fn test_game_is_possible_with() {
        let bag = Cubes::from([("red".to_string(), 12), ("green".to_string(), 13), ("blue".to_string(), 14)]);
        assert_eq!(
            Game {
                id: 1,
                reveals: vec![
                    Cubes::from([("red".to_string(), 4), ("blue".to_string(), 3)]),
                    Cubes::from([("red".to_string(), 1), ("green".to_string(), 2), ("blue".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 2)]),
                ],
            }.is_possible_with(&bag),
            true
        );
        assert_eq!(
            Game {
                id: 2,
                reveals: vec![
                    Cubes::from([("blue".to_string(), 1), ("green".to_string(), 2)]),
                    Cubes::from([("green".to_string(), 3), ("blue".to_string(), 4), ("red".to_string(), 1)]),
                    Cubes::from([("green".to_string(), 1), ("blue".to_string(), 1)]),
                ],
            }.is_possible_with(&bag),
            true
        );
        assert_eq!(
            Game {
                id: 3,
                reveals: vec![
                    Cubes::from([("green".to_string(), 8), ("blue".to_string(), 6), ("red".to_string(), 20)]),
                    Cubes::from([("blue".to_string(), 5), ("red".to_string(), 4), ("green".to_string(), 13)]),
                    Cubes::from([("green".to_string(), 5), ("red".to_string(), 1)]),
                ],
            }.is_possible_with(&bag),
            false
        );
        assert_eq!(
            Game {
                id: 4,
                reveals: vec![
                    Cubes::from([("green".to_string(), 1), ("red".to_string(), 3), ("blue".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 3), ("red".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 3), ("blue".to_string(), 15), ("red".to_string(), 14)]),
                ],
            }.is_possible_with(&bag),
            false
        );
        assert_eq!(
            Game {
                id: 5,
                reveals: vec![
                    Cubes::from([("red".to_string(), 6), ("blue".to_string(), 1), ("green".to_string(), 3)]),
                    Cubes::from([("blue".to_string(), 2), ("red".to_string(), 1), ("green".to_string(), 2)]),
                ],
            }.is_possible_with(&bag),
            true
        );
    }

    #[test]
    fn complete() {
        let bag = Cubes::from([("red".to_string(), 12), ("green".to_string(), 13), ("blue".to_string(), 14)]);
        assert_eq!(answer("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", &bag), Some(1));
        assert_eq!(answer("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", &bag), Some(2));
        assert_eq!(answer("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", &bag), None);
        assert_eq!(answer("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", &bag), None);
        assert_eq!(answer("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", &bag), Some(5));
    }

    #[test]
    fn total() {
        let bag = Cubes::from([("red".to_string(), 12), ("green".to_string(), 13), ("blue".to_string(), 14)]);
        let input = vec![
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
        ];
        assert_eq!(aggregate(input.into_iter(), &bag), 8);
    }
}
