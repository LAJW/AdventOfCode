package day2

import (
    "aoc/utils"
    "fmt"
    "log"
    "slices"
    "strconv"
    "strings"
)

func lineToNumbers(line string) ([]int, error) {
    numbers := make([]int, 0)
    for _, word := range strings.Split(line, " ") {
        number, err := strconv.Atoi(word)
        if err != nil {
            return nil, err
        }
        numbers = append(numbers, number)
    }
    return numbers, nil

}

func pairwiseCheck(numbers []int, pred func(x int, y int) bool) bool {
    for i := 0; i < len(numbers)-1; i++ {
        if !pred(numbers[i], numbers[i+1]) {
            return false
        }
    }
    return true
}

func isAscending(numbers []int) bool {
    return pairwiseCheck(numbers, func(x int, y int) bool { return x < y })
}

func isDescending(numbers []int) bool {
    return pairwiseCheck(numbers, func(x int, y int) bool { return x > y })
}

func isGentle(numbers []int) bool {
    return pairwiseCheck(numbers, func(x int, y int) bool { return utils.Abs(x-y) <= 3 })
}

func isSafe(numbers []int) bool {
    return (isAscending(numbers) || isDescending(numbers)) && isGentle(numbers)
}

func checkDampened(list []int) bool {
    if isSafe(list) {
        return true
    }
    for i := range list {
        if isSafe(slices.Concat(list[0:i], list[i+1:])) {
            return true
        }
    }
    return false
}

func readAllNumbers(filename string) ([][]int, error) {
    lines, err := utils.ReadAllLines(filename)
    if err != nil {
        return nil, err
    }

    result := [][]int{}

    for _, line := range lines {
        numbers, err := lineToNumbers(line)
        if err != nil {
            return nil, err
        }
        result = append(result, numbers)
    }

    return result, nil
}

func CountIf[T any](arr []T, pred func(x T) bool) int {
    count := 0
    for _, line := range arr {
        if pred(line) {
            count++
        }
    }
    return count
}

func Run1() {
    numbers, err := readAllNumbers("../data2.txt")
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(CountIf(numbers, isSafe))
}

func Run2() {
    numbers, err := readAllNumbers("../data2.txt")
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(CountIf(numbers, checkDampened))
}
