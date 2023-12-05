use clap::Parser;
use regex::Regex;
use std::{cmp::max, collections::HashMap, error::Error, fs::read_to_string};

type Cubes = HashMap<String, u32>;
type GameId = u32;
#[derive(Debug, PartialEq)]
struct Game {
    id: GameId,
    reveals: Vec<Cubes>,
}

fn parse_game(game_data: &str) -> Game {
    const ERR_MSG: &str = "Game Data did not match expected pattern";
    let game_re = Regex::new(r"^Game (\d+): (.+)$").expect("Game Regex could not compile");
    let cube_re = Regex::new(r"(\d+) ([a-zA-Z]+)").expect("Cube Regex could not compile");

    let game_caps = game_re.captures(game_data).expect(ERR_MSG);

    let id = game_caps.get(1).expect(ERR_MSG).as_str().parse::<GameId>().expect("Could not parse game ID");
    let mut reveals = vec![];

    for reveal_text in game_caps.get(2).expect(ERR_MSG).as_str().split(';') {
        let mut reveal = Cubes::new();
        for cube_text in reveal_text.split(',') {
            let cube_caps = cube_re.captures(cube_text).expect(ERR_MSG);
            let count = cube_caps.get(1).expect(ERR_MSG).as_str().parse::<u32>().expect("Could not parse number of cubes");
            let color = cube_caps.get(2).expect(ERR_MSG).as_str();
            reveal.insert(color.to_string(), count);
        }
        reveals.push(reveal);
    }
    Game {
        id,
        reveals,
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

    fn get_minimum_set(&self) -> Cubes {
        let mut min_set = Cubes::new();
        for reveal in &self.reveals {
            for (color, count) in reveal.iter() {
                min_set.insert(
                    color.to_string(),
                    min_set.get(color).map(|current_min| max(*current_min, *count)).unwrap_or(*count)
                );
            }
        }
        min_set
    }
}

fn power_of(bag: &Cubes) -> u32 {
    bag.values().product()
}

fn answer1(input: &str, bag: &Cubes) -> Option<GameId> {
    let game = parse_game(input);
    game.is_possible_with(bag).then_some(game.id)
}

fn aggregate1<'a, I>(contents: I, bag: &Cubes) -> GameId
where
    I: Iterator<Item = &'a str>
{
    contents.map(|line| answer1(line, bag).unwrap_or(0)).sum()
}

fn answer2(input: &str) -> u32 {
    let game = parse_game(input);
    power_of(&game.get_minimum_set())
}

fn aggregate2<'a, I>(contents: I) -> GameId
where
    I: Iterator<Item = &'a str>
{
    contents.map(answer2).sum()
}

#[derive(Debug, Parser)]
struct Cli {
    file: std::path::PathBuf,
    #[arg(short, long)]
    red: u32,
    #[arg(short, long)]
    green: u32,
    #[arg(short, long)]
    blue: u32,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let content = read_to_string(&args.file).unwrap();
    let bag = Cubes::from([
        ("red".to_string(), args.red),
        ("green".to_string(), args.green),
        ("blue".to_string(), args.blue),
    ]);
    let result1 = aggregate1(content.lines(), &bag);
    println!("{:?}", result1);
    let result2 = aggregate2(content.lines());
    println!("{:?}", result2);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_game() {
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
    fn test_answer1() {
        let bag = Cubes::from([("red".to_string(), 12), ("green".to_string(), 13), ("blue".to_string(), 14)]);
        assert_eq!(answer1("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", &bag), Some(1));
        assert_eq!(answer1("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", &bag), Some(2));
        assert_eq!(answer1("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", &bag), None);
        assert_eq!(answer1("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", &bag), None);
        assert_eq!(answer1("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", &bag), Some(5));
    }

    #[test]
    fn test_aggregate1() {
        let bag = Cubes::from([("red".to_string(), 12), ("green".to_string(), 13), ("blue".to_string(), 14)]);
        let input = vec![
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
        ];
        assert_eq!(aggregate1(input.into_iter(), &bag), 8);
    }

    #[test]
    fn test_get_minimum_set() {
        assert_eq!(
            Game {
                id: 1,
                reveals: vec![
                    Cubes::from([("red".to_string(), 4), ("blue".to_string(), 3)]),
                    Cubes::from([("red".to_string(), 1), ("green".to_string(), 2), ("blue".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 2)]),
                ],
            }.get_minimum_set(),
            Cubes::from([("red".to_string(), 4), ("green".to_string(), 2), ("blue".to_string(), 6)])
        );
        assert_eq!(
            Game {
                id: 2,
                reveals: vec![
                    Cubes::from([("blue".to_string(), 1), ("green".to_string(), 2)]),
                    Cubes::from([("green".to_string(), 3), ("blue".to_string(), 4), ("red".to_string(), 1)]),
                    Cubes::from([("green".to_string(), 1), ("blue".to_string(), 1)]),
                ],
            }.get_minimum_set(),
            Cubes::from([("red".to_string(), 1), ("green".to_string(), 3), ("blue".to_string(), 4)])
        );
        assert_eq!(
            Game {
                id: 3,
                reveals: vec![
                    Cubes::from([("green".to_string(), 8), ("blue".to_string(), 6), ("red".to_string(), 20)]),
                    Cubes::from([("blue".to_string(), 5), ("red".to_string(), 4), ("green".to_string(), 13)]),
                    Cubes::from([("green".to_string(), 5), ("red".to_string(), 1)]),
                ],
            }.get_minimum_set(),
            Cubes::from([("red".to_string(), 20), ("green".to_string(), 13), ("blue".to_string(), 6)])
        );
        assert_eq!(
            Game {
                id: 4,
                reveals: vec![
                    Cubes::from([("green".to_string(), 1), ("red".to_string(), 3), ("blue".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 3), ("red".to_string(), 6)]),
                    Cubes::from([("green".to_string(), 3), ("blue".to_string(), 15), ("red".to_string(), 14)]),
                ],
            }.get_minimum_set(),
            Cubes::from([("red".to_string(), 14), ("green".to_string(), 3), ("blue".to_string(), 15)])
        );
        assert_eq!(
            Game {
                id: 5,
                reveals: vec![
                    Cubes::from([("red".to_string(), 6), ("blue".to_string(), 1), ("green".to_string(), 3)]),
                    Cubes::from([("blue".to_string(), 2), ("red".to_string(), 1), ("green".to_string(), 2)]),
                ],
            }.get_minimum_set(),
            Cubes::from([("red".to_string(), 6), ("green".to_string(), 3), ("blue".to_string(), 2)])
        );
    }

    #[test]
    fn test_power_of() {
        assert_eq!(power_of(&Cubes::from([("red".to_string(), 4), ("green".to_string(), 2), ("blue".to_string(), 6)])), 48);
        assert_eq!(power_of(&Cubes::from([("red".to_string(), 1), ("green".to_string(), 3), ("blue".to_string(), 4)])), 12);
        assert_eq!(power_of(&Cubes::from([("red".to_string(), 20), ("green".to_string(), 13), ("blue".to_string(), 6)])), 1560);
        assert_eq!(power_of(&Cubes::from([("red".to_string(), 14), ("green".to_string(), 3), ("blue".to_string(), 15)])), 630);
        assert_eq!(power_of(&Cubes::from([("red".to_string(), 6), ("green".to_string(), 3), ("blue".to_string(), 2)])), 36);
    }

    #[test]
    fn test_answer2() {
        assert_eq!(answer2("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"), 48);
        assert_eq!(answer2("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"), 12);
        assert_eq!(answer2("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"), 1560);
        assert_eq!(answer2("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"), 630);
        assert_eq!(answer2("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"), 36);
    }

    #[test]
    fn test_aggregate2() {
        let input = vec![
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
        ];
        assert_eq!(aggregate2(input.into_iter()), 2286);
    }
}
