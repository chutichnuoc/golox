package main

import (
	"golox/vm"
)

func main() {
	chunk := vm.NewChunk()
	constant := chunk.AddConstant(1.2)
	chunk.Write(vm.OP_CONSTANT, 123)
	chunk.Write(constant, 123)
	chunk.Write(vm.OP_RETURN, 123)
	vm.DisassembleChunk(chunk, "test chunk")
	chunk.Free()
}
