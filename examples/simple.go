package main

func add(x int, y int) int {
    return func() int {
        return x + y
    }()
}

func main() {
    result := add(40, 2)
    println(result)
}