package day1

import (
    "aoc/utils"
    "bufio"
    "fmt"
    "log"
    "os"
    "sort"
    "strconv"
    "strings"
)

type Result[T any] struct {
    value T
    error error
}

func processLine(line string) (int, int, error) {
    words := strings.Split(line, " ")
    first, err := strconv.Atoi(words[0])
    if err != nil {
        return 0, 0, err
    }
    last, err := strconv.Atoi(words[len(words)-1])
    if err != nil {
        return 0, 0, err
    }
    return first, last, nil
}

func firstsAndSeconds(lines []string) ([]int, []int, error) {
    var firsts []int
    var seconds []int
    for _, line := range lines {
        first, second, err := processLine(line)
        if err != nil {
            return nil, nil, err
        }
        firsts = append(firsts, first)
        seconds = append(seconds, second)
    }
    return firsts, seconds, nil
}

func solve1(lines []string) (int, error) {
    firsts, seconds, err := firstsAndSeconds(lines)
    if err != nil {
        return 0, err
    }
    sort.Ints(firsts)
    sort.Ints(seconds)
    var result = 0
    for i, first := range firsts {
        second := seconds[i]
        result += utils.Abs(second - first)
    }
    return result, nil
}

func solve2(lines []string) (int, error) {
    firsts, seconds, err := firstsAndSeconds(lines)
    if err != nil {
        return 0, err
    }
    counts := make(map[int]int)
    for _, second := range seconds {
        counts[second]++
    }
    result := 0
    for _, first := range firsts {
        result += first * counts[first]
    }
    return result, nil
}

func readAllLines(scanner *bufio.Scanner) []string {
    lines := make([]string, 0)
    for scanner.Scan() {
        line := scanner.Text()
        lines = append(lines, line)
    }
    return lines
}

func Go[T any](fn func() T) chan T {
    result := make(chan T)
    go func() {
        result <- fn()
    }()
    return result
}

func Run() {
    file, err := os.Open("../data1.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer func(file *os.File) {
        err := file.Close()
        if err != nil {
            log.Fatal(err)
        }
    }(file)

    scanner := bufio.NewScanner(file)
    lines := readAllLines(scanner)

    chan1 := Go(func() Result[int] {
        result, err := solve1(lines)
        return Result[int]{result, err}
    })

    chan2 := Go(func() Result[int] {
        result, err := solve2(lines)
        return Result[int]{result, err}
    })

    result1 := <-chan1
    if result1.error != nil {
        log.Fatal(err)
    }

    result2 := <-chan2
    if result2.error != nil {
        log.Fatal(err)
    }

    fmt.Printf("results: %d %d", result1.value, result2.value)
}
