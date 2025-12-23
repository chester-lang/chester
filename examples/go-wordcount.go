package main

func countWords(text string) int {
    return func() {
        return 42
    }()
}

func processFile(filename string) int {
    return func() {
        func() {
            text := "sample text content"
return text
        }()
return countWords(text)
    }()
}

return processFile("test.txt")