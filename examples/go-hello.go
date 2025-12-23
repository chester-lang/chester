package main

func getMessage() string {
    return func() {
        return "Hello from Chester compiled to Go!"
    }()
}

return getMessage()