package main

func greet(name string) string {
    return func() {
        return func() {
            "Hello, "
+
name
+
return "!"
        }()
    }()
}

return greet("Chester")