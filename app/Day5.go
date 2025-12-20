package main

/*
#cgo CXXFLAGS: -std=c++17
*/
import (
	"C"
	"fmt"
	"os"
)
import (
	"bufio"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

func main() {
	fmt.Println("Day 5")

	if len(os.Args) < 2 {
		panic("Invalid number of arguments, expecting file path")
	}

	filePath := os.Args[1]
	file, err := os.Open(filePath)
	if err != nil {
		panic("Error opening up file")
	}

	defer file.Close()
	scanner := bufio.NewScanner(file)
	var lines []string

	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		panic("Error while reading file contents")
	}

	ranges, ingredients := parseRangesWithIngredients(lines)
	for _, r := range ranges {
		fmt.Println("<", r.start, ",", r.end, ">")
	}

	slices.SortFunc(ranges, func(a Range, b Range) int {
		return a.start - b.start
	})
	ranges = mergeOverlappingRanges(ranges)

	fmt.Println("Ranges: ", len(ranges))
	fmt.Println("Ingredients: ", len(ingredients))
	fmt.Println("Solution:\n-----------------------------")
	fmt.Println(getValidIngredientCount(ranges, ingredients))
}

type Range struct {
	start int
	end   int
}

func parseRangesWithIngredients(lines []string) ([]Range, []int) {
	var ranges []Range
	var ingredients []int
	for _, line := range lines {
		if line == "\n" || line == "" {
			continue
		}

		if rangeMatch, _ := regexp.MatchString("\\d+-\\d+", line); rangeMatch {
			items := strings.Split(line, "-")
			if len(items) != 2 {
				panic("Invalid range definition: '" + line + "'")
			}

			start, err := strconv.Atoi(items[0])
			if err != nil {
				panic("Invalid range start value: " + items[0])
			}

			end, err := strconv.Atoi(items[1])
			if err != nil {
				panic("Invalid range end value: " + items[1])
			}

			ranges = append(ranges, Range{start: start, end: end})
			continue
		}

		if ingredientMatch, _ := regexp.MatchString("\\d+", line); ingredientMatch {
			val, err := strconv.Atoi(line)
			if err != nil {
				panic("Invalid value, unable to parse int for ingredient: " + line)
			}

			ingredients = append(ingredients, val)
		}

	}

	return ranges, ingredients
}

func mergeOverlappingRanges(sortedRanges []Range) []Range {
	var merged []Range
	currentRange := sortedRanges[0]
	for _, r := range sortedRanges {
		if overlaps(currentRange, r) {
			currentRange = merge(currentRange, r)
			continue
		}

		merged = append(merged, currentRange)
		currentRange = r
	}

	if merged[len(merged)-1] != currentRange {
		merged = append(merged, currentRange)
	}

	return merged
}

func overlaps(a Range, b Range) bool {
	return a.start <= b.end && b.start <= a.end
}

func merge(a Range, b Range) Range {
	fmt.Println("Merging ", "<", a.start, ",", a.end, ">", "with <", b.start, ",", b.end, ">")
	return Range{start: min(a.start, b.start), end: max(a.end, b.end)}
}

func getValidIngredientCount(sortedRanges []Range, ingredients []int) int {
	total := 0
	fmt.Println("Solving for ingredients", ingredients)
	for _, ingredient := range ingredients {
		if isIngredientInRange(sortedRanges, ingredient) {
			total += 1
		}
	}

	return total
}

func isIngredientInRange(sortedRanges []Range, ingredient int) bool {
	_, found := slices.BinarySearchFunc(sortedRanges, ingredient, func(r Range, target int) int {
		if target < r.start {
			return 1
		}

		if target > r.end {
			return -1
		}

		fmt.Println("ingredient:", ingredient, "is in <", r.start, ",", r.end, ">")
		return 0
	})

	return found
}
