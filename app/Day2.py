from dataclasses import dataclass
import itertools
from pathlib import Path
import sys


@dataclass(frozen=True)
class NumberRange:
    start: int
    end: int

    def __post_init__(self) -> None:
        if self.start > self.end:
            raise ValueError(f"Start: {self.start} cannot be > end: {self.end}")

    @classmethod
    def create(cls, range_str: str) -> "NumberRange":
        start, end = range_str.split("-")
        return cls(start=int(start), end=int(end))

    def get_repeating_elements(self) -> list[int]:
        return [
            number
            for number in range(self.start, self.end + 1)
            if self.is_repeating_element(str(number))
        ]

    def is_repeating_element(self, number_str: str) -> bool:
        # 1111 - 4 repeating 1-tuples
        # 1212 - 2 repeating 2-tuples
        # 123123 - 2 repeating 3-tuples
        # 1234123 -> there can be max len // 2 tuples
        if not number_str or len(number_str) < 2:
            return False

        max_n_tuple = len(number_str) // 2
        for n_tuple in range(1, max_n_tuple + 1):
            if len(number_str) % n_tuple != 0:
                continue

            sliced = [
                number_str[i : i + n_tuple] for i in range(0, len(number_str), n_tuple)
            ]
            if len({n_tuple_slice for n_tuple_slice in sliced}) == 1:
                return True

        return False


def process_ranges(ranges: list[str]) -> None:
    number_ranges = [NumberRange.create(range_str) for range_str in ranges]
    repeating_numbers = list(
        itertools.chain(
            *[number_range.get_repeating_elements() for number_range in number_ranges]
        )
    )
    print(repeating_numbers)
    print(sum(repeating_numbers))


def main() -> None:
    path = Path(sys.argv[1])

    contents = path.read_text(encoding="utf-8")
    process_ranges(contents.split(","))


if __name__ == "__main__":
    main()
