package main

import (
	lox "golox/interpreter"
)

func main() {
	//vm := lox.NewVM()
	chunk := lox.NewChunk()

	constant := chunk.AddConstant(1.2)
	chunk.Write(lox.OpConstant, 123)
	chunk.Write(constant, 123)

	constant = chunk.AddConstant(3.4)
	chunk.Write(lox.OpConstant, 123)
	chunk.Write(constant, 123)

	chunk.Write(lox.OpAdd, 123)

	constant = chunk.AddConstant(5.6)
	chunk.Write(lox.OpConstant, 123)
	chunk.Write(constant, 123)

	chunk.Write(lox.OpDivide, 123)

	chunk.Write(lox.OpNegate, 123)
	chunk.Write(lox.OpReturn, 123)

	lox.DisassembleChunk(chunk, "test chunk")

	//vm.Interpret()
	//vm.Free()
	chunk.Free()
}
