package utils

import (
    "bufio"
    "os"
)

func scanAllLines(scanner *bufio.Scanner) []string {
    lines := make([]string, 0)
    for scanner.Scan() {
        line := scanner.Text()
        lines = append(lines, line)
    }
    return lines
}

func ReadAllLines(filename string) ([]string, error) {
    file, err := os.Open(filename)
    if err != nil {
        return nil, err
    }
    scanner := bufio.NewScanner(file)
    lines := scanAllLines(scanner)
    err = file.Close()
    if err != nil {
        return nil, err
    }
    return lines, nil
}

func Abs(x int) int {
    if x < 0 {
        return -x
    } else {
        return x
    }
}
