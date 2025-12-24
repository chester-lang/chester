package main
import "fmt"

type GoImport_fmt struct {
    Sprintln func(args any) string
Fprint func(args any) string
Sscanf func(args any) string
Scanln func(args any) string
Sscanln func(args any) string
Println func(args any) string
Printf func(args any) string
Sscan func(args any) string
Sprint func(args any) string
Fscanf func(args any) string
Sprintf func(args any) string
Append func(args any) string
Fscanln func(args any) string
Errorf func(args any) string
Fprintf func(args any) string
Print func(args any) string
Fprintln func(args any) string
Scanf func(args any) string
FormatString func(args any) string
Fscan func(args any) string
Scan func(args any) string
Appendf func(args any) string
String func(args any) string
Appendln func(args any) string
}

type GoImport_go struct {
    fmt GoImport_fmt
}

func greet(name string) []any {
    return func() {
        go.fmt.Printf(name)
return []any{}
    }()
}

func main() {
    fmt.Println(greet("Chester from Go FFI"))
}