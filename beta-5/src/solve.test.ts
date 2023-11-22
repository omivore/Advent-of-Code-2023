import {
  Crate,
  Stack,
  Step,
  executeStep1,
  executeStep2,
  readTops,
  evaluateProcedure1,
  evaluateProcedure2,
  parseInitialState,
  parseSteps,
} from "./solve";

test.each([
  [
    [["Z", "N"], ["M", "C", "D"], ["P"]],
    { src: 2, dst: 1, num: 1 },
    [["Z", "N", "D"], ["M", "C"], ["P"]],
  ],
  [
    [["Z", "N", "D"], ["M", "C"], ["P"]],
    { src: 1, dst: 3, num: 3 },
    [[], ["M", "C"], ["P", "Z", "N", "D"]],
  ],
  [
    [[], ["M", "C"], ["P", "Z", "N", "D"]],
    { src: 2, dst: 1, num: 2 },
    [["M", "C"], [], ["P", "Z", "N", "D"]],
  ],
  [
    [["M", "C"], [], ["P", "Z", "N", "D"]],
    { src: 1, dst: 2, num: 1 },
    [["M"], ["C"], ["P", "Z", "N", "D"]],
  ],
])("steps execute via 9001", (stacks: Stack[], step: Step, expected: Stack[]) => {
  executeStep2(stacks, step);
  expect(stacks).toEqual(expected);
});

test("procedure executes via 9001", () => {
  let stacks = [["Z", "N"], ["M", "C", "D"], ["P"]];
  let steps = [
    { src: 2, dst: 1, num: 1 },
    { src: 1, dst: 3, num: 3 },
    { src: 2, dst: 1, num: 2 },
    { src: 1, dst: 2, num: 1 },
  ];
  let expected = [["M"], ["C"], ["P", "Z", "N", "D"]];
  evaluateProcedure2(stacks, steps);
  expect(stacks).toEqual(expected);
});

test.each([
  [
    [["Z", "N"], ["M", "C", "D"], ["P"]],
    { src: 2, dst: 1, num: 1 },
    [["Z", "N", "D"], ["M", "C"], ["P"]],
  ],
  [
    [["Z", "N", "D"], ["M", "C"], ["P"]],
    { src: 1, dst: 3, num: 3 },
    [[], ["M", "C"], ["P", "D", "N", "Z"]],
  ],
  [
    [[], ["M", "C"], ["P", "D", "N", "Z"]],
    { src: 2, dst: 1, num: 2 },
    [["C", "M"], [], ["P", "D", "N", "Z"]],
  ],
  [
    [["C", "M"], [], ["P", "D", "N", "Z"]],
    { src: 1, dst: 2, num: 1 },
    [["C"], ["M"], ["P", "D", "N", "Z"]],
  ],
])("steps execute via 9000", (stacks: Stack[], step: Step, expected: Stack[]) => {
  executeStep1(stacks, step);
  expect(stacks).toEqual(expected);
});

test.each([
  [
    [["Z", "N"], ["M", "C", "D"], ["P"]],
    ["N", "D", "P"],
  ],
  [
    [["Z", "N", "D"], ["M", "C"], ["P"]],
    ["D", "C", "P"],
  ],
  [
    [[], ["M", "C"], ["P", "D", "N", "Z"]],
    ["C", "Z"],
  ],
  [
    [["C", "M"], [], ["P", "D", "N", "Z"]],
    ["M", "Z"],
  ],
  [
    [["C"], ["M"], ["P", "D", "N", "Z"]],
    ["C", "M", "Z"],
  ],
])("reads top crate", (stacks: Stack[], expected: Crate[]) => {
  expect(readTops(stacks)).toEqual(expected);
});

test("procedure executes via 9000", () => {
  let stacks = [["Z", "N"], ["M", "C", "D"], ["P"]];
  let steps = [
    { src: 2, dst: 1, num: 1 },
    { src: 1, dst: 3, num: 3 },
    { src: 2, dst: 1, num: 2 },
    { src: 1, dst: 2, num: 1 },
  ];
  let expected = [["C"], ["M"], ["P", "D", "N", "Z"]];
  evaluateProcedure1(stacks, steps);
  expect(stacks).toEqual(expected);
});

test("state parses", () => {
  let input = `    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 `;
  let expected = [["Z", "N"], ["M", "C", "D"], ["P"]];
  expect(parseInitialState(input)).toEqual(expected);
});

test("steps parse", () => {
  let input = `move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2`;
  let expected = [
    { src: 2, dst: 1, num: 1 },
    { src: 1, dst: 3, num: 3 },
    { src: 2, dst: 1, num: 2 },
    { src: 1, dst: 2, num: 1 },
  ];
  expect(parseSteps(input)).toEqual(expected);
});
