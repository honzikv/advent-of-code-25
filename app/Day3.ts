import heap from "npm:heap@0.2.7";

type Bank = string;
type Pair<T1, T2> = [T1, T2];

// This assumes the string's length >= 2
const findMaxJoltage = (bank: Bank, k_digits: number): number => {
  const digitArray = Array.from(bank, (char) => parseInt(char, 10));

  const result = [];
  let sliceStartIdx = 0;
  for (let i = 0; i < k_digits; i += 1) {
    // Each position needs to keep at least one digit to the subsequent number
    // So if we have two digits, we need to process from 0 (inc) .. n-1 (exc)
    // index range for the first (MSB) number
    const sliceEndIdx = digitArray.length - k_digits + i + 1
    const sliced = digitArray.slice(sliceStartIdx, sliceEndIdx);
    // console.log("i=", i, "sliced=", sliced, "from", sliceStartIdx, "sliceEndIdx", sliceEndIdx, "digitArray=", digitArray)
    const [maxNumber, maxNumberIdx] = findMaxWithIndex(sliced);
    result.push(maxNumber);
    sliceStartIdx += maxNumberIdx + 1;
  }

  const x = parseInt(result.join(""));
  // console.log(x)
  return x
};

const findMaxWithIndex = (array: number[]): Pair<number, number> => {
  let max = Number.NEGATIVE_INFINITY;
  let idx = -1;

  array.forEach((currVal, currIdx) => {
    if (currVal > max) {
      max = currVal;
      idx = currIdx;
    }
  });

  return [max, idx];
};

const main = async () => {
  console.log("Day 3");

  console.log(
    (await Deno.readTextFile(Deno.args[0]))
      .split("\n")
      .filter(ln => !!ln)
      .map((row) => findMaxJoltage(row, 12))
      .reduce((a, b) => a + b)
  );
};

await main();
