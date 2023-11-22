export type Crate = string;
export type Stack = Crate[];
export type Step = {
  src: number;
  dst: number;
  num: number;
};

export function executeStep1(stacks: Stack[], step: Step) {
  for (let held, i = 0; i < step.num; i++) {
    if (
      step.src < 1 ||
      step.dst < 1 ||
      step.src > stacks.length ||
      step.dst > stacks.length
    ) {
      throw Error("Step source or destination index is out of bounds.");
    }
    held = stacks[step.src - 1].pop();
    if (held === undefined) {
      throw Error(
        "Number of crates to move greater than number of crates in stack.",
      );
    }
    stacks[step.dst - 1].push(held);
  }
}

export function executeStep2(stacks: Stack[], step: Step) {
  if (
    step.src < 1 ||
    step.dst < 1 ||
    step.src > stacks.length ||
    step.dst > stacks.length
  ) {
    throw Error("Step source or destination index is out of bounds.");
  }
  if (step.num > stacks[step.src - 1].length) {
    throw Error(
      "Number of crates to move greater than number of crates in stack.",
    );
  }
  let held = stacks[step.src - 1].splice(-step.num);
  stacks[step.dst - 1].splice(stacks[step.dst - 1].length, 0, ...held);
}

export function readTops(stacks: Stack[]): Crate[] {
  return stacks
    .filter((stack) => stack.length >= 1)
    .map((stack) => stack.at(-1)!);
}

export function evaluateProcedure1(stacks: Stack[], steps: Step[]) {
  steps.map((step) => executeStep1(stacks, step));
}

export function evaluateProcedure2(stacks: Stack[], steps: Step[]) {
  steps.map((step) => executeStep2(stacks, step));
}

export function parseInitialState(input: string): Stack[] {
  let lines = input.split("\n");
  let stack_count = Math.ceil(lines[0].length / 4);
  let stacks: Stack[] = new Array(stack_count);
  for (let i = 0; i < stack_count; i++) {
    stacks[i] = new Array();
  }

  const crateContent = /\[(.+)\]/;
  for (let line of lines) {
    for (let i = 0; i < stack_count; i++) {
      let chunk = line.slice(i * 4, i * 4 + 3);
      let matches = crateContent.exec(chunk);
      if (matches) {
        stacks[i].unshift(matches[1]);
      }
    }
  }

  return stacks;
}

export function parseSteps(input: string): Step[] {
  let lines = input.trim().split("\n");
  const stepPattern = /^move (\d+) from (\d+) to (\d+)$/;

  let steps = new Array();
  for (let line of lines) {
    let matches = stepPattern.exec(line);
    if (matches === null) {
      throw Error(`Step format \"${line}\" does not match expected pattern.`);
    }
    steps.push({
      src: parseInt(matches[2]),
      dst: parseInt(matches[3]),
      num: parseInt(matches[1]),
    });
  }

  return steps;
}
