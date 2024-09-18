package interpreter

import "fmt"

type VM struct {
	chunk    Chunk
	ip       uint8
	stack    [256]Value
	stackTop int
}

func InitVM() *VM {
	vm := &VM{}
	vm.stackTop = 0
	return vm
}

func (vm *VM) Free() {

}

func (vm *VM) push(value Value) {
	vm.stack[vm.stackTop] = value
	vm.stackTop++
}

func (vm *VM) pop() Value {
	vm.stackTop--
	return vm.stack[vm.stackTop]
}

func (vm *VM) run() InterpretResult {
	for {
		if DebugTraceExecution {
			fmt.Print("          ")
			for slot := 0; slot < vm.stackTop; slot++ {
				fmt.Print("[ ")
				printValue(vm.stack[slot])
				fmt.Print(" ]")
			}
			fmt.Println()
			disassembleInstruction(&vm.chunk, int(vm.ip))
		}
		instruction := vm.chunk.code[vm.ip]
		vm.ip++
		switch instruction {
		case OpConstant:
			constantIndex := vm.chunk.code[vm.ip]
			constant := vm.chunk.constants.values[constantIndex]
			vm.ip++
			vm.push(constant)
			break
		case OpAdd:
			b := vm.pop()
			a := vm.pop()
			vm.push(a + b)
			break
		case OpSubtract:
			b := vm.pop()
			a := vm.pop()
			vm.push(a - b)
			break
		case OpMultiply:
			b := vm.pop()
			a := vm.pop()
			vm.push(a * b)
			break
		case OpDivide:
			b := vm.pop()
			a := vm.pop()
			vm.push(a / b)
			break
		case OpNegate:
			vm.push(-vm.pop())
			break
		case OpReturn:
			printValue(vm.pop())
			fmt.Println()
			return InterpretOk
		}
	}
}

func (vm *VM) Interpret(source string) InterpretResult {
	chunk := NewChunk()
	if !compile(source, chunk) {
		chunk.Free()
		return InterpretCompileError
	}

	vm.chunk = *chunk
	vm.ip = 0

	result := vm.run()

	chunk.Free()
	return result
}
