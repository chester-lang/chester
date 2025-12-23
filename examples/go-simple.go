package main

func add(x int,  y int) int {
    return func() {
        return x + y
    }()
}

return add(40,  2)