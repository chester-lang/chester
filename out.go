package main
import "fmt"

type GoImport_fmt struct {
Printf func(format string, args []any) string
  Sprintf func(format string, args []any) string
  Println func(args []any) struct {}
}

type GoImport_go struct {
fmt GoImport_fmt
}

func print_str(msg string) struct {} {
fmt.Println(msg)
  return struct {}{}
}

func print_int(val int) struct {} {
fmt.Println(val)
  return struct {}{}
}

func concat_str(a string, b string) string {
return fmt.Sprintf("%s%s", a, b)
}

func int_to_str(val int) string {
return fmt.Sprintf("%d", val)
}

func main() {
print_str("Starting target-agnostic standard library test...")
  s := concat_str("The answer is: ", int_to_str(42))
  print_str(s)
}