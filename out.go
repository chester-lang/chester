package main
import "fmt"

func list_append(list any, elem any) any {
len := __chester_list_len(list)
  return __chester_list_make(__chester_int_add(len, 1), func(i any) any {
  return func() any {
    if __chester_as_bool(__chester_int_eq(i, len)) {
      return elem
      }
      return __chester_list_get(list, i)
    }()
  })
}

func list_map(list any, f func(x any) any) any {
len := __chester_list_len(list)
  return __chester_list_make(len, func(i any) any {
  return f(__chester_list_get(list, i))
  })
}

func find_matching_index(list any, predicate func(x any) any, target_match any, current_idx any, matches_found any) any {
return func() any {
  if __chester_as_bool(__chester_int_eq(current_idx, __chester_list_len(list))) {
    return __chester_int_sub(0, 1)
    }
    return func() any {
    if __chester_as_bool(predicate(__chester_list_get(list, current_idx))) {
      return func() any {
        if __chester_as_bool(__chester_int_eq(matches_found, target_match)) {
          return current_idx
          }
          return find_matching_index(list, predicate, target_match, __chester_int_add(current_idx, 1), __chester_int_add(matches_found, 1))
        }()
      }
      return find_matching_index(list, predicate, target_match, __chester_int_add(current_idx, 1), matches_found)
    }()
  }()
}

func count_matches(list any, predicate func(x any) any, current_idx any, acc any) any {
return func() any {
  if __chester_as_bool(__chester_int_eq(current_idx, __chester_list_len(list))) {
    return acc
    }
    return func() any {
    if __chester_as_bool(predicate(__chester_list_get(list, current_idx))) {
      return count_matches(list, predicate, __chester_int_add(current_idx, 1), __chester_int_add(acc, 1))
      }
      return count_matches(list, predicate, __chester_int_add(current_idx, 1), acc)
    }()
  }()
}

func list_filter(list any, predicate func(x any) any) any {
cnt := count_matches(list, predicate, 0, 0)
  return __chester_list_make(cnt, func(j any) any {
  return func() any {
    orig_idx := find_matching_index(list, predicate, j, 0, 0)
      return __chester_list_get(list, orig_idx)
    }()
  })
}

func list_foldl_helper(list any, init any, f func(acc any, elem any) any, idx any) any {
return func() any {
  if __chester_as_bool(__chester_int_eq(idx, __chester_list_len(list))) {
    return init
    }
    return list_foldl_helper(list, f(init, __chester_list_get(list, idx)), f, __chester_int_add(idx, 1))
  }()
}

func list_foldl(list any, init any, f func(acc any, elem any) any) any {
return list_foldl_helper(list, init, f, 0)
}

func print_str(msg any) struct {} {
fmt.Println(msg)
  return struct {}{}
}

func print_int(val any) struct {} {
fmt.Println(val)
  return struct {}{}
}

func concat_str(a any, b any) any {
return fmt.Sprintf("%s%s", a, b)
}

func int_to_str(val any) any {
return fmt.Sprintf("%d", val)
}

func to_upper_str(s any) any {
return s
}

func main() {
s := "hello"
  fmt.Println(to_upper_str(s))
  fmt.Println(to_upper_str(s))
}

func __chester_as_bool(v any) bool { b, _ := v.(bool); return b }
func __chester_int_add(a, b any) any { return a.(int) + b.(int) }
func __chester_int_sub(a, b any) any { return a.(int) - b.(int) }
func __chester_int_mul(a, b any) any { return a.(int) * b.(int) }
func __chester_int_eq(a, b any) bool { return a.(int) == b.(int) }
func __chester_int_lt(a, b any) bool { return a.(int) < b.(int) }

func __chester_list_len(list any) any {
	return len(list.([]any))
}

func __chester_list_get(list any, idx any) any {
	return list.([]any)[idx.(int)]
}

func __chester_list_make(size any, generator func(any) any) []any {
	n := size.(int)
	res := make([]any, n)
	for i := 0; i < n; i++ {
		res[i] = generator(i)
	}
	return res
}

var __chester_effect_handlers = map[string][]func(...any)any{}

func __chester_push_handler(op string, handler func(...any)any) {
	__chester_effect_handlers[op] = append(__chester_effect_handlers[op], handler)
}

func __chester_pop_handler(op string) {
	handlers := __chester_effect_handlers[op]
	__chester_effect_handlers[op] = handlers[:len(handlers)-1]
}

func __chester_do(op string, args ...any) any {
	handlers := __chester_effect_handlers[op]
	return handlers[len(handlers)-1](args...)
}
