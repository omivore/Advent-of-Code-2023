export type Crate = string;
export type Stack = Crate[];
export type Step = {
  src: number;
  dst: number;
  num: number;
};

export function executeStep(stacks: Stack[], step: Step) {
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

export function readTops(stacks: Stack[]): Crate[] {
  return stacks
    .filter((stack) => stack.length >= 1)
    .map((stack) => stack.at(-1)!);
}

export function evaluateProcedure(stacks: Stack[], steps: Step[]) {
  steps.map((step) => executeStep(stacks, step));
}

export function parseInitialState(input: string): Stack[] {
  console.log(input);
  return [[]];
}

export function parseSteps(input: string): Step[] {
  console.log(input);
  return [];
}
