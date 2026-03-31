package main
import "fmt"

func countWords(text string) int {
    return 42
}

func processFile(filename string) int {
    text := "sample text content"
return countWords(text)
}

func main() {
    fmt.Println(processFile("test.txt"))
}