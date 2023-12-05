use clap::Parser;
use regex::Regex;
use std::{error::Error, fs::read_to_string};

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
    const ERR_MSG: &str = "Data did not match expected pattern";
    let seeds_re = Regex::new(r"^seeds: ([\d ]+)$").expect("Seed Regex could not compile");
    let map_re = Regex::new(r"^(\w+)-to-(\w+) map:$").expect("Map Regex could not compile");
    let mapping_re = Regex::new(r"^(\d+) (\d+) (\d+)$").expect("Mapping Regex could not compile");

    let mut iter = all_data.split("\n\n");
    let seeds_data = iter.next().expect("Did not find seed line");
    let seeds_caps = seeds_re.captures(seeds_data).expect(ERR_MSG);
    let seeds = seeds_caps
        .get(1)
        .expect(ERR_MSG)
        .as_str()
        .split(' ')
        .map(|num| num.parse::<usize>().expect("Could not parse seed number"))
        .collect();

    let mut maps = Vec::<Map>::new();
    for data in iter {
        let mut map_iter = data.lines();
        let map_data = map_iter.next().expect("Did not find map line");
        let map_caps = map_re.captures(map_data).expect(ERR_MSG);

        let from = map_caps
            .get(1)
            .expect(ERR_MSG)
            .as_str()
            .to_string();
        let to = map_caps
            .get(2)
            .expect(ERR_MSG)
            .as_str()
            .to_string();

        let mut mappings = Vec::<Mapping>::new();
        for mapping_data in map_iter {
            let mapping_caps = mapping_re.captures(mapping_data).expect(ERR_MSG);
            let dst = mapping_caps
                .get(1)
                .expect(ERR_MSG)
                .as_str()
                .parse::<usize>()
                .expect("Could not parse mapping number");
            let src = mapping_caps
                .get(2)
                .expect(ERR_MSG)
                .as_str()
                .parse::<usize>()
                .expect("Could not parse mapping number");
            let len = mapping_caps
                .get(3)
                .expect(ERR_MSG)
                .as_str()
                .parse::<usize>()
                .expect("Could not parse mapping number");
            mappings.push(Mapping { dst, src, len });
        }
        maps.push(Map { from, to, mappings });
    }
    (seeds, maps)
}

impl Map {
    fn convert(&self, key: usize) -> usize {
        for mapping in &self.mappings {
            if (mapping.src..mapping.src + mapping.len).contains(&key) {
                return mapping.dst + key - mapping.src
            }
        }
        key
    }
}

fn get_location(maps: &Vec<Map>, seed: usize) -> usize {
    let mut value = seed;
    for map in maps {
        value = map.convert(value);
    }
    value
}

fn answer(contents: &str) -> usize {
    let (seeds, maps) = parse_data(contents);
    seeds
        .into_iter()
        .map(|seed| get_location(&maps, seed))
        .min()
        .expect("Could not find a minimum location")
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
                            Mapping { dst: 50, src: 98, len: 2 },
                            Mapping { dst: 52, src: 50, len: 48 },
                        ],
                    },
                    Map {
                        from: String::from("soil"),
                        to:   String::from("fertilizer"),
                        mappings: vec![
                            Mapping { dst: 0, src: 15, len: 37 },
                            Mapping { dst: 37, src: 52, len: 2 },
                            Mapping { dst: 39, src: 0, len: 15 },
                        ],
                    },
                    Map {
                        from: String::from("fertilizer"),
                        to:   String::from("water"),
                        mappings: vec![
                            Mapping { dst: 49, src: 53, len: 8 },
                            Mapping { dst: 0, src: 11, len: 42 },
                            Mapping { dst: 42, src: 0, len: 7 },
                            Mapping { dst: 57, src: 7, len: 4 },
                        ],
                    },
                    Map {
                        from: String::from("water"),
                        to:   String::from("light"),
                        mappings: vec![
                            Mapping { dst: 88, src: 18, len: 7 },
                            Mapping { dst: 18, src: 25, len: 70 },
                        ],
                    },
                    Map {
                        from: String::from("light"),
                        to:   String::from("temperature"),
                        mappings: vec![
                            Mapping { dst: 45, src: 77, len: 23 },
                            Mapping { dst: 81, src: 45, len: 19 },
                            Mapping { dst: 68, src: 64, len: 13 },
                        ],
                    },
                    Map {
                        from: String::from("temperature"),
                        to:   String::from("humidity"),
                        mappings: vec![
                            Mapping { dst: 0, src: 69, len: 1 },
                            Mapping { dst: 1, src: 0, len: 69 },
                        ],
                    },
                    Map {
                        from: String::from("humidity"),
                        to:   String::from("location"),
                        mappings: vec![
                            Mapping { dst: 60, src: 56, len: 37 },
                            Mapping { dst: 56, src: 93, len: 4 },
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
                Mapping { dst: 50, src: 98, len: 2 },
                Mapping { dst: 52, src: 50, len: 48 },
            ],
        };
        let fertilizer = Map {
            from: String::from("soil"),
            to:   String::from("fertilizer"),
            mappings: vec![
                Mapping { dst: 0, src: 15, len: 37 },
                Mapping { dst: 37, src: 52, len: 2 },
                Mapping { dst: 39, src: 0, len: 15 },
            ],
        };
        let water = Map {
            from: String::from("fertilizer"),
            to:   String::from("water"),
            mappings: vec![
                Mapping { dst: 49, src: 53, len: 8 },
                Mapping { dst: 0, src: 11, len: 42 },
                Mapping { dst: 42, src: 0, len: 7 },
                Mapping { dst: 57, src: 7, len: 4 },
            ],
        };
        let light = Map {
            from: String::from("water"),
            to:   String::from("light"),
            mappings: vec![
                Mapping { dst: 88, src: 18, len: 7 },
                Mapping { dst: 18, src: 25, len: 70 },
            ],
        };
        let temperature = Map {
            from: String::from("light"),
            to:   String::from("temperature"),
            mappings: vec![
                Mapping { dst: 45, src: 77, len: 23 },
                Mapping { dst: 81, src: 45, len: 19 },
                Mapping { dst: 68, src: 64, len: 13 },
            ],
        };
        let humidity = Map {
            from: String::from("temperature"),
            to:   String::from("humidity"),
            mappings: vec![
                Mapping { dst: 0, src: 69, len: 1 },
                Mapping { dst: 1, src: 0, len: 69 },
            ],
        };
        let location = Map {
            from: String::from("humidity"),
            to:   String::from("location"),
            mappings: vec![
                Mapping { dst: 60, src: 56, len: 37 },
                Mapping { dst: 56, src: 93, len: 4 },
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
        assert_eq!(light.convert(53), 46);
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
                    Mapping { dst: 50, src: 98, len: 2 },
                    Mapping { dst: 52, src: 50, len: 48 },
                ],
            },
            Map {
                from: String::from("soil"),
                to:   String::from("fertilizer"),
                mappings: vec![
                    Mapping { dst: 0, src: 15, len: 37 },
                    Mapping { dst: 37, src: 52, len: 2 },
                    Mapping { dst: 39, src: 0, len: 15 },
                ],
            },
            Map {
                from: String::from("fertilizer"),
                to:   String::from("water"),
                mappings: vec![
                    Mapping { dst: 49, src: 53, len: 8 },
                    Mapping { dst: 0, src: 11, len: 42 },
                    Mapping { dst: 42, src: 0, len: 7 },
                    Mapping { dst: 57, src: 7, len: 4 },
                ],
            },
            Map {
                from: String::from("water"),
                to:   String::from("light"),
                mappings: vec![
                    Mapping { dst: 88, src: 18, len: 7 },
                    Mapping { dst: 18, src: 25, len: 70 },
                ],
            },
            Map {
                from: String::from("light"),
                to:   String::from("temperature"),
                mappings: vec![
                    Mapping { dst: 45, src: 77, len: 23 },
                    Mapping { dst: 81, src: 45, len: 19 },
                    Mapping { dst: 68, src: 64, len: 13 },
                ],
            },
            Map {
                from: String::from("temperature"),
                to:   String::from("humidity"),
                mappings: vec![
                    Mapping { dst: 0, src: 69, len: 1 },
                    Mapping { dst: 1, src: 0, len: 69 },
                ],
            },
            Map {
                from: String::from("humidity"),
                to:   String::from("location"),
                mappings: vec![
                    Mapping { dst: 60, src: 56, len: 37 },
                    Mapping { dst: 56, src: 93, len: 4 },
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
