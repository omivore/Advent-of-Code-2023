import {
  parseInitialState,
  parseSteps,
  evaluateProcedure,
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

let stacks = parseInitialState(split_input[0]);
let steps = parseSteps(split_input[1]);

evaluateProcedure(stacks, steps);
let tops = readTops(stacks);
console.log(tops.join(""));
