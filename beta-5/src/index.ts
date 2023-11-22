import {
  parseInitialState,
  parseSteps,
  evaluateProcedure1,
  evaluateProcedure2,
  readTops,
} from "./solve";
import * as fs from "fs";
import * as path from "path";
import Minimist from "minimist";

const argv = Minimist(process.argv.slice(2));
if (argv["_"].length < 1) {
  throw Error("Path to input file required.");
}

let full_input = fs.readFileSync(
  path.join(process.cwd(), argv["_"][0]),
  "utf8",
);
let split_input = full_input.split("\n\n");

let stacks, steps, tops
// Part 1
stacks = parseInitialState(split_input[0]);
steps = parseSteps(split_input[1]);

evaluateProcedure1(stacks, steps);
tops = readTops(stacks);
console.log(tops.join(""));

// Part 2
stacks = parseInitialState(split_input[0]);
steps = parseSteps(split_input[1]);

evaluateProcedure2(stacks, steps);
tops = readTops(stacks);
console.log(tops.join(""));
