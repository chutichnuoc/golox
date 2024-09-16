package main

import (
	"fmt"
	"os"

	lox "golox/interpreter"
)

func main() {
	vm := lox.InitVM()
	runFile(vm, "C:\\Users\\hungle\\code\\test.lox")
	//if len(os.Args) == 1 {
	//	repl(vm)
	//} else if len(os.Args) == 2 {
	//	runFile(vm, os.Args[1])
	//} else {
	//	fmt.Println("Usage: lox [path]")
	//	os.Exit(64)
	//}

	//vm.Interpret()
	vm.Free()
}

func repl(vm *lox.VM) {
	var line [1024]byte
	for {
		fmt.Print("> ")

		n, err := os.Stdin.Read(line[:])
		if err != nil {
			fmt.Println()
			break
		}

		vm.Interpret(string(line[:n]))
	}
}

func runFile(vm *lox.VM, path string) {
	source, err := os.ReadFile(path)
	if err != nil {
		fmt.Printf("Could not open file %s: %s\n", path, err)
		os.Exit(74)
	}
	result := vm.Interpret(string(source))
	if result == lox.InterpretCompileError {
		os.Exit(65)
	}
	if result == lox.InterpretRuntimeError {
		os.Exit(70)
	}
}
