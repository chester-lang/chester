package main

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"golang.org/x/tools/go/packages"
	"go/types"
)

type Param struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type Function struct {
	Params  []Param  `json:"params"`
	Returns string   `json:"returns"`
	Effects []string `json:"effects"`
}

type Package struct {
	Functions map[string]Function `json:"functions"`
}

type Root struct {
	Packages map[string]Package `json:"packages"`
}

func formatType(t types.Type, isVariadic bool) string {
	s := t.String()
	s = strings.ReplaceAll(s, "interface{}", "any")
	if isVariadic && strings.HasPrefix(s, "[]") {
		s = "..." + s[2:]
	}
	return s
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: go2chester <pkg1> <pkg2> ...\n")
		os.Exit(1)
	}

	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedTypes | packages.NeedTypesInfo,
	}

	pkgs, err := packages.Load(cfg, os.Args[1:]...)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to load packages: %v\n", err)
		os.Exit(1)
	}

	if packages.PrintErrors(pkgs) > 0 {
		os.Exit(1)
	}

	root := Root{
		Packages: make(map[string]Package),
	}

	for _, pkg := range pkgs {
		pkgName := pkg.Name
		if pkgName == "" {
			continue
		}

		funcs := make(map[string]Function)
		scope := pkg.Types.Scope()
		for _, name := range scope.Names() {
			obj := scope.Lookup(name)
			if !obj.Exported() {
				continue
			}

			fn, ok := obj.(*types.Func)
			if !ok {
				continue
			}

			sig, ok := fn.Type().(*types.Signature)
			if !ok {
				continue
			}

			if sig.Recv() != nil {
				continue
			}

			var params []Param
			paramsStruct := sig.Params()
			isVariadic := sig.Variadic()

			for i := 0; i < paramsStruct.Len(); i++ {
				p := paramsStruct.At(i)
				pType := formatType(p.Type(), isVariadic && i == paramsStruct.Len()-1)
				
				pName := p.Name()
				if pName == "" {
					pName = fmt.Sprintf("arg%d", i)
				}
				params = append(params, Param{
					Name: pName,
					Type: pType,
				})
			}

			if params == nil {
				params = []Param{}
			}

			resultsStruct := sig.Results()
			var returns string
			if resultsStruct.Len() == 0 {
				returns = "Unit"
			} else if resultsStruct.Len() == 1 {
				returns = formatType(resultsStruct.At(0).Type(), false)
			} else {
				var retTypes []string
				for i := 0; i < resultsStruct.Len(); i++ {
					retTypes = append(retTypes, formatType(resultsStruct.At(i).Type(), false))
				}
				returns = "(" + strings.Join(retTypes, ", ") + ")"
			}

			funcs[name] = Function{
				Params:  params,
				Returns: returns,
				Effects: []string{"io"},
			}
		}

		root.Packages[pkgName] = Package{
			Functions: funcs,
		}
	}

	out, err := json.MarshalIndent(root, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to encode JSON: %v\n", err)
		os.Exit(1)
	}

	fmt.Println(string(out))
}
