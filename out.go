package main
import "fmt"

type GoImport_fmt struct {
Printf func(format string, args []any) string
  Println func(args []any) struct {}
}

type GoImport_go struct {
fmt GoImport_fmt
}

func main() {
fmt.Printf("Hello from Chester via Go FFI! The answer is %d\n", 42)
}