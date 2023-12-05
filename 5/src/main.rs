use clap::Parser;
use regex::Regex;
use std::{collections::HashMap, collections::VecDeque, error::Error, fs::read_to_string};

#[derive(Clone, Debug, PartialEq)]
struct Mapping {
    src: usize,
    dst: usize,
    len: usize,
}

#[derive(Clone, Debug, PartialEq)]
struct Map {
    from: String,
    to: String,
    mappings: Vec<Mapping>,
}

fn parse_data(all_data: &str) -> (Vec<usize>, Vec<Map>) {
    (
        vec![1],
        vec![Map {
            from: String::from("this"),
            to: String::from("that"),
            mappings: vec![Mapping { src: 1, dst: 10, len: 2 }],
        }]
    )
}

impl Map {
    fn convert(&self, key: usize) -> usize {
        key
    }
}

fn get_location(maps: &Vec<Map>, seed: usize) -> usize {
    1
}

fn answer(contents: &str) -> usize {
    let (seeds, maps) = parse_data(contents);
    1
}

#[derive(Debug, Parser)]
struct Cli {
    file: std::path::PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let content = read_to_string(args.file).unwrap();
    let result = answer(&content);
    println!("{:?}", result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_data() {
        let input = "seeds: 79 14 55 13\n\
                     \n\
                     seed-to-soil map:\n\
                     50 98 2\n\
                     52 50 48\n\
                     \n\
                     soil-to-fertilizer map:\n\
                     0 15 37\n\
                     37 52 2\n\
                     39 0 15\n\
                     \n\
                     fertilizer-to-water map:\n\
                     49 53 8\n\
                     0 11 42\n\
                     42 0 7\n\
                     57 7 4\n\
                     \n\
                     water-to-light map:\n\
                     88 18 7\n\
                     18 25 70\n\
                     \n\
                     light-to-temperature map:\n\
                     45 77 23\n\
                     81 45 19\n\
                     68 64 13\n\
                     \n\
                     temperature-to-humidity map:\n\
                     0 69 1\n\
                     1 0 69\n\
                     \n\
                     humidity-to-location map:\n\
                     60 56 37\n\
                     56 93 4";
        assert_eq!(
            parse_data(input),
            (
                vec![79, 14, 55, 13],
                vec![
                    Map {
                        from: String::from("seed"),
                        to:   String::from("soil"),
                        mappings: vec![
                            Mapping { src: 50, dst: 98, len: 2 },
                            Mapping { src: 52, dst: 50, len: 48 },
                        ],
                    },
                    Map {
                        from: String::from("soil"),
                        to:   String::from("fertilizer"),
                        mappings: vec![
                            Mapping { src: 0, dst: 15, len: 37 },
                            Mapping { src: 37, dst: 52, len: 2 },
                            Mapping { src: 39, dst: 0, len: 15 },
                        ],
                    },
                    Map {
                        from: String::from("fertilizer"),
                        to:   String::from("water"),
                        mappings: vec![
                            Mapping { src: 49, dst: 53, len: 8 },
                            Mapping { src: 0, dst: 11, len: 42 },
                            Mapping { src: 42, dst: 0, len: 7 },
                            Mapping { src: 57, dst: 7, len: 4 },
                        ],
                    },
                    Map {
                        from: String::from("water"),
                        to:   String::from("light"),
                        mappings: vec![
                            Mapping { src: 88, dst: 18, len: 7 },
                            Mapping { src: 18, dst: 25, len: 70 },
                        ],
                    },
                    Map {
                        from: String::from("light"),
                        to:   String::from("temperature"),
                        mappings: vec![
                            Mapping { src: 45, dst: 77, len: 23 },
                            Mapping { src: 81, dst: 45, len: 19 },
                            Mapping { src: 68, dst: 64, len: 13 },
                        ],
                    },
                    Map {
                        from: String::from("temperature"),
                        to:   String::from("humidity"),
                        mappings: vec![
                            Mapping { src: 0, dst: 69, len: 1 },
                            Mapping { src: 1, dst: 0, len: 69 },
                        ],
                    },
                    Map {
                        from: String::from("humidity"),
                        to:   String::from("location"),
                        mappings: vec![
                            Mapping { src: 60, dst: 56, len: 37 },
                            Mapping { src: 56, dst: 93, len: 4 },
                        ],
                    },
                ],
            )
        );
    }

    #[test]
    fn test_convert() {
        let soil = Map {
            from: String::from("seed"),
            to:   String::from("soil"),
            mappings: vec![
                Mapping { src: 50, dst: 98, len: 2 },
                Mapping { src: 52, dst: 50, len: 48 },
            ],
        };
        let fertilizer = Map {
            from: String::from("soil"),
            to:   String::from("fertilizer"),
            mappings: vec![
                Mapping { src: 0, dst: 15, len: 37 },
                Mapping { src: 37, dst: 52, len: 2 },
                Mapping { src: 39, dst: 0, len: 15 },
            ],
        };
        let water = Map {
            from: String::from("fertilizer"),
            to:   String::from("water"),
            mappings: vec![
                Mapping { src: 49, dst: 53, len: 8 },
                Mapping { src: 0, dst: 11, len: 42 },
                Mapping { src: 42, dst: 0, len: 7 },
                Mapping { src: 57, dst: 7, len: 4 },
            ],
        };
        let light = Map {
            from: String::from("water"),
            to:   String::from("light"),
            mappings: vec![
                Mapping { src: 88, dst: 18, len: 7 },
                Mapping { src: 18, dst: 25, len: 70 },
            ],
        };
        let temperature = Map {
            from: String::from("light"),
            to:   String::from("temperature"),
            mappings: vec![
                Mapping { src: 45, dst: 77, len: 23 },
                Mapping { src: 81, dst: 45, len: 19 },
                Mapping { src: 68, dst: 64, len: 13 },
            ],
        };
        let humidity = Map {
            from: String::from("temperature"),
            to:   String::from("humidity"),
            mappings: vec![
                Mapping { src: 0, dst: 69, len: 1 },
                Mapping { src: 1, dst: 0, len: 69 },
            ],
        };
        let location = Map {
            from: String::from("humidity"),
            to:   String::from("location"),
            mappings: vec![
                Mapping { src: 60, dst: 56, len: 37 },
                Mapping { src: 56, dst: 93, len: 4 },
            ],
        };

        assert_eq!(soil.convert(79), 81);
        assert_eq!(fertilizer.convert(81), 81);
        assert_eq!(water.convert(81), 81);
        assert_eq!(light.convert(81), 74);
        assert_eq!(temperature.convert(74), 78);
        assert_eq!(humidity.convert(78), 78);
        assert_eq!(location.convert(78), 82);

        assert_eq!(soil.convert(14), 14);
        assert_eq!(fertilizer.convert(14), 53);
        assert_eq!(water.convert(53), 49);
        assert_eq!(light.convert(49), 42);
        assert_eq!(temperature.convert(42), 42);
        assert_eq!(humidity.convert(42), 43);
        assert_eq!(location.convert(43), 43);

        assert_eq!(soil.convert(55), 57);
        assert_eq!(fertilizer.convert(57), 57);
        assert_eq!(water.convert(57), 53);
        assert_eq!(light.convert(81), 46);
        assert_eq!(temperature.convert(46), 82);
        assert_eq!(humidity.convert(82), 82);
        assert_eq!(location.convert(82), 86);

        assert_eq!(soil.convert(13), 13);
        assert_eq!(fertilizer.convert(13), 52);
        assert_eq!(water.convert(52), 41);
        assert_eq!(light.convert(41), 34);
        assert_eq!(temperature.convert(34), 34);
        assert_eq!(humidity.convert(34), 35);
        assert_eq!(location.convert(35), 35);
    }

    #[test]
    fn test_get_location() {
        let maps = vec![
            Map {
                from: String::from("seed"),
                to:   String::from("soil"),
                mappings: vec![
                    Mapping { src: 50, dst: 98, len: 2 },
                    Mapping { src: 52, dst: 50, len: 48 },
                ],
            },
            Map {
                from: String::from("soil"),
                to:   String::from("fertilizer"),
                mappings: vec![
                    Mapping { src: 0, dst: 15, len: 37 },
                    Mapping { src: 37, dst: 52, len: 2 },
                    Mapping { src: 39, dst: 0, len: 15 },
                ],
            },
            Map {
                from: String::from("fertilizer"),
                to:   String::from("water"),
                mappings: vec![
                    Mapping { src: 49, dst: 53, len: 8 },
                    Mapping { src: 0, dst: 11, len: 42 },
                    Mapping { src: 42, dst: 0, len: 7 },
                    Mapping { src: 57, dst: 7, len: 4 },
                ],
            },
            Map {
                from: String::from("water"),
                to:   String::from("light"),
                mappings: vec![
                    Mapping { src: 88, dst: 18, len: 7 },
                    Mapping { src: 18, dst: 25, len: 70 },
                ],
            },
            Map {
                from: String::from("light"),
                to:   String::from("temperature"),
                mappings: vec![
                    Mapping { src: 45, dst: 77, len: 23 },
                    Mapping { src: 81, dst: 45, len: 19 },
                    Mapping { src: 68, dst: 64, len: 13 },
                ],
            },
            Map {
                from: String::from("temperature"),
                to:   String::from("humidity"),
                mappings: vec![
                    Mapping { src: 0, dst: 69, len: 1 },
                    Mapping { src: 1, dst: 0, len: 69 },
                ],
            },
            Map {
                from: String::from("humidity"),
                to:   String::from("location"),
                mappings: vec![
                    Mapping { src: 60, dst: 56, len: 37 },
                    Mapping { src: 56, dst: 93, len: 4 },
                ],
            },
        ];

        assert_eq!(get_location(&maps, 79), 82);
        assert_eq!(get_location(&maps, 14), 43);
        assert_eq!(get_location(&maps, 55), 86);
        assert_eq!(get_location(&maps, 13), 35);
    }

    #[test]
    fn test_answer() {
        let input = "seeds: 79 14 55 13\n\
                     \n\
                     seed-to-soil map:\n\
                     50 98 2\n\
                     52 50 48\n\
                     \n\
                     soil-to-fertilizer map:\n\
                     0 15 37\n\
                     37 52 2\n\
                     39 0 15\n\
                     \n\
                     fertilizer-to-water map:\n\
                     49 53 8\n\
                     0 11 42\n\
                     42 0 7\n\
                     57 7 4\n\
                     \n\
                     water-to-light map:\n\
                     88 18 7\n\
                     18 25 70\n\
                     \n\
                     light-to-temperature map:\n\
                     45 77 23\n\
                     81 45 19\n\
                     68 64 13\n\
                     \n\
                     temperature-to-humidity map:\n\
                     0 69 1\n\
                     1 0 69\n\
                     \n\
                     humidity-to-location map:\n\
                     60 56 37\n\
                     56 93 4";
        assert_eq!(answer(input), 35);
    }
}
