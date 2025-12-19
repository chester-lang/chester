package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"os"

	"golang.org/x/tools/go/packages"
)

type TypeInfo struct {
	Package   string         `json:"package"`
	Interfaces []InterfaceInfo `json:"interfaces"`
	Structs   []StructInfo    `json:"structs"`
	Functions []FunctionInfo  `json:"functions"`
}

type InterfaceInfo struct {
	Name    string       `json:"name"`
	Methods []MethodInfo `json:"methods"`
}

type MethodInfo struct {
	Name    string      `json:"name"`
	Params  []ParamInfo `json:"params"`
	Results []ParamInfo `json:"results"`
}

type StructInfo struct {
	Name   string      `json:"name"`
	Fields []FieldInfo `json:"fields"`
}

type FieldInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type FunctionInfo struct {
	Name    string      `json:"name"`
	Params  []ParamInfo `json:"params"`
	Results []ParamInfo `json:"results"`
}

type ParamInfo struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type"`
}

func main() {
	flag.Parse()
	args := flag.Args()

	if len(args) == 0 {
		fmt.Fprintf(os.Stderr, "Usage: go-type-extractor <package-path>\n")
		os.Exit(1)
	}

	packagePath := args[0]
	typeInfo, err := extractTypes(packagePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	output, err := json.MarshalIndent(typeInfo, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error marshaling JSON: %v\n", err)
		os.Exit(1)
	}

	fmt.Println(string(output))
}

func extractTypes(packagePath string) (*TypeInfo, error) {
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedSyntax | packages.NeedTypes | packages.NeedTypesInfo,
	}

	pkgs, err := packages.Load(cfg, packagePath)
	if err != nil {
		return nil, fmt.Errorf("failed to load package: %w", err)
	}

	if len(pkgs) == 0 {
		return nil, fmt.Errorf("no packages found for %s", packagePath)
	}

	pkg := pkgs[0]
	if len(pkg.Errors) > 0 {
		return nil, fmt.Errorf("package has errors: %v", pkg.Errors)
	}

	info := &TypeInfo{
		Package:    pkg.Name,
		Interfaces: []InterfaceInfo{},
		Structs:    []StructInfo{},
		Functions:  []FunctionInfo{},
	}

	// Extract exported types and functions
	scope := pkg.Types.Scope()
	for _, name := range scope.Names() {
		if !ast.IsExported(name) {
			continue
		}

		obj := scope.Lookup(name)
		if obj == nil {
			continue
		}

		switch t := obj.Type().Underlying().(type) {
		case *types.Interface:
			info.Interfaces = append(info.Interfaces, extractInterface(name, t))
		case *types.Struct:
			info.Structs = append(info.Structs, extractStruct(name, t))
		}

		// Check if it's a function
		if fn, ok := obj.(*types.Func); ok {
			sig := fn.Type().(*types.Signature)
			info.Functions = append(info.Functions, extractFunction(name, sig))
		}
	}

	return info, nil
}

func extractInterface(name string, iface *types.Interface) InterfaceInfo {
	methods := []MethodInfo{}

	for i := 0; i < iface.NumMethods(); i++ {
		method := iface.Method(i)
		if !ast.IsExported(method.Name()) {
			continue
		}

		sig := method.Type().(*types.Signature)
		methods = append(methods, extractMethod(method.Name(), sig))
	}

	return InterfaceInfo{
		Name:    name,
		Methods: methods,
	}
}

func extractMethod(name string, sig *types.Signature) MethodInfo {
	return MethodInfo{
		Name:    name,
		Params:  extractParams(sig.Params()),
		Results: extractParams(sig.Results()),
	}
}

func extractStruct(name string, st *types.Struct) StructInfo {
	fields := []FieldInfo{}

	for i := 0; i < st.NumFields(); i++ {
		field := st.Field(i)
		if !ast.IsExported(field.Name()) {
			continue
		}

		fields = append(fields, FieldInfo{
			Name: field.Name(),
			Type: field.Type().String(),
		})
	}

	return StructInfo{
		Name:   name,
		Fields: fields,
	}
}

func extractFunction(name string, sig *types.Signature) FunctionInfo {
	return FunctionInfo{
		Name:    name,
		Params:  extractParams(sig.Params()),
		Results: extractParams(sig.Results()),
	}
}

func extractParams(tuple *types.Tuple) []ParamInfo {
	if tuple == nil {
		return []ParamInfo{}
	}

	params := []ParamInfo{}
	for i := 0; i < tuple.Len(); i++ {
		param := tuple.At(i)
		params = append(params, ParamInfo{
			Name: param.Name(),
			Type: param.Type().String(),
		})
	}

	return params
}
