package interpreter

import (
	"fmt"
)

var global = make(map[string]Value)

type VM struct {
	chunk    Chunk
	ip       uint8
	stack    [256]Value
	stackTop int
}

func InitVM() *VM {
	vm := &VM{}
	vm.resetStack()
	return vm
}

func (vm *VM) resetStack() {
	vm.stackTop = 0
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

func (vm *VM) peek(distance int) Value {
	return vm.stack[vm.stackTop-distance-1]
}

func (vm *VM) readByte() uint8 {
	ip := vm.ip
	vm.ip++
	return ip
}

func (vm *VM) readConstant() Value {
	constantIndex := vm.chunk.code[vm.readByte()]
	constant := vm.chunk.constants.values[constantIndex]
	return constant
}

func (vm *VM) readString() string {
	return asString(vm.readConstant())
}

func isFalsey(value Value) bool {
	return isNil(value) || (isBool(value) && !asBool(value))
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
		instruction := vm.chunk.code[vm.readByte()]
		switch instruction {
		case OpConstant:
			constant := vm.readConstant()
			vm.push(constant)
			break
		case OpNil:
			vm.push(nilVal())
			break
		case OpTrue:
			vm.push(boolVal(true))
			break
		case OpFalse:
			vm.push(boolVal(false))
			break
		case OpPop:
			vm.pop()
			break
		case OpGetLocal:
			slot := vm.readByte()
			vm.push(vm.stack[slot])
			break
		case OpSetLocal:
			slot := vm.readByte()
			vm.stack[slot] = vm.peek(0)
			break
		case OpGetGlobal:
			name := vm.readString()
			value, ok := global[name]
			if !ok {
				vm.runtimeError("Undefined variable '%s'.", name)
				return InterpretRuntimeError
			}
			vm.push(value)
			break
		case OpDefineGlobal:
			name := vm.readString()
			global[name] = vm.peek(0)
			vm.pop()
			break
		case OpSetGlobal:
			name := vm.readString()
			_, ok := global[name]
			if !ok {
				vm.runtimeError("Undefined variable '%s'.", name)
				return InterpretRuntimeError
			}
			global[name] = vm.peek(0)
			break
		case OpEqual:
			b := vm.pop()
			a := vm.pop()
			vm.push(boolVal(valuesEqual(a, b)))
			break
		case OpGreater:
			if !isNumber(vm.peek(0)) || !isNumber(vm.peek(1)) {
				vm.runtimeError("Operands must be numbers.")
				return InterpretRuntimeError
			}
			b := asNumber(vm.pop())
			a := asNumber(vm.pop())
			vm.push(boolVal(a > b))
			break
		case OpLess:
			if !isNumber(vm.peek(0)) || !isNumber(vm.peek(1)) {
				vm.runtimeError("Operands must be numbers.")
				return InterpretRuntimeError
			}
			b := asNumber(vm.pop())
			a := asNumber(vm.pop())
			vm.push(boolVal(a < b))
			break
		case OpAdd:
			if isString(vm.peek(0)) && isString(vm.peek(1)) {
				b := asString(vm.pop())
				a := asString(vm.pop())
				vm.push(stringVal(a + b))
			} else if isNumber(vm.peek(0)) && isNumber(vm.peek(1)) {
				b := asNumber(vm.pop())
				a := asNumber(vm.pop())
				vm.push(numberVal(a + b))
			} else {
				vm.runtimeError("Operands must be two numbers or two strings.")
				return InterpretRuntimeError
			}
			break
		case OpSubtract:
			if !isNumber(vm.peek(0)) || !isNumber(vm.peek(1)) {
				vm.runtimeError("Operands must be numbers.")
				return InterpretRuntimeError
			}
			b := asNumber(vm.pop())
			a := asNumber(vm.pop())
			vm.push(numberVal(a - b))
			break
		case OpMultiply:
			if !isNumber(vm.peek(0)) || !isNumber(vm.peek(1)) {
				vm.runtimeError("Operands must be numbers.")
				return InterpretRuntimeError
			}
			b := asNumber(vm.pop())
			a := asNumber(vm.pop())
			vm.push(numberVal(a * b))
			break
		case OpDivide:
			if !isNumber(vm.peek(0)) || !isNumber(vm.peek(1)) {
				vm.runtimeError("Operands must be numbers.")
				return InterpretRuntimeError
			}
			b := asNumber(vm.pop())
			a := asNumber(vm.pop())
			vm.push(numberVal(a / b))
			break
		case OpNot:
			vm.push(boolVal(isFalsey(vm.pop())))
			break
		case OpNegate:
			if !isNumber(vm.peek(0)) {
				vm.runtimeError("Operand must be a number.")
				return InterpretRuntimeError
			}
			vm.push(numberVal(-asNumber(vm.pop())))
			break
		case OpPrint:
			printValue(vm.pop())
			fmt.Println()
			break
		case OpReturn:
			// Exit interpreter.
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

func (vm *VM) runtimeError(format string, args ...interface{}) {
	fmt.Printf(format, args...)
	fmt.Println()
	instruction := vm.ip - 1
	line := vm.chunk.lines[instruction]
	fmt.Printf("[line %d] in script\n", line)
}
