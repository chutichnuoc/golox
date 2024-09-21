package interpreter

import (
	"fmt"
	"time"
)

const FramesMax int = 64
const StackMax = FramesMax * 256

var global = make(map[string]Value)

type CallFrame struct {
	function *Function
	ip       uint16
	slots    []Value
}

type VM struct {
	frames     [FramesMax]CallFrame
	frameCount int
	stack      [StackMax]Value
	stackTop   int
}

func InitVM() *VM {
	vm := &VM{}
	vm.resetStack()
	vm.defineNative("clock", clockNative)
	return vm
}

func (vm *VM) resetStack() {
	vm.stackTop = 0
	vm.frameCount = 0
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

func (vm *VM) call(function *Function, argCount int) bool {
	if argCount != function.arity {
		vm.runtimeError("Expected %d arguments but got %d.", function.arity, argCount)
		return false
	}

	if vm.frameCount == FramesMax {
		vm.runtimeError("Stack overflow.")
		return false
	}

	frame := &vm.frames[vm.frameCount]
	vm.frameCount++
	frame.function = function
	frame.ip = 0
	frame.slots = vm.stack[vm.stackTop-argCount-1:]
	return true
}

func (vm *VM) callValue(callee Value, argCount int) bool {
	if isFunction(callee) {
		return vm.call(asFunction(callee), argCount)
	} else if isNativeFn(callee) {
		native := asNativeFn(callee)
		result := native(argCount, vm.stack[vm.stackTop-argCount:])
		vm.stackTop -= argCount + 1
		vm.push(result)
		return true
	}
	vm.runtimeError("Can only call functions and classes.")
	return false
}

func (frame *CallFrame) readByte() uint8 {
	code := frame.function.chunk.code[frame.ip]
	frame.ip++
	return code
}

func (frame *CallFrame) readConstant() Value {
	return frame.function.chunk.constants.values[frame.readByte()]
}

func (frame *CallFrame) readShort() uint16 {
	frame.ip += 2
	return uint16((frame.function.chunk.code[frame.ip-2] << 8) | frame.function.chunk.code[frame.ip-1])
}

func (frame *CallFrame) readString() string {
	return asString(frame.readConstant())
}

func isFalsey(value Value) bool {
	return isNil(value) || (isBool(value) && !asBool(value))
}

func (vm *VM) run() InterpretResult {
	frame := &vm.frames[vm.frameCount-1]
	for {
		if DebugTraceExecution {
			fmt.Print("          ")
			for slot := 0; slot < vm.stackTop; slot++ {
				fmt.Print("[ ")
				printValue(vm.stack[slot])
				fmt.Print(" ]")
			}
			fmt.Println()
			disassembleInstruction(&frame.function.chunk, int(frame.ip))
		}
		instruction := frame.readByte()
		switch instruction {
		case OpConstant:
			constant := frame.readConstant()
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
			slot := frame.readByte()
			vm.push(frame.slots[slot])
			break
		case OpSetLocal:
			slot := frame.readByte()
			frame.slots[slot] = vm.peek(0)
			break
		case OpGetGlobal:
			name := frame.readString()
			value, ok := global[name]
			if !ok {
				vm.runtimeError("Undefined variable '%s'.", name)
				return InterpretRuntimeError
			}
			vm.push(value)
			break
		case OpDefineGlobal:
			name := frame.readString()
			global[name] = vm.peek(0)
			vm.pop()
			break
		case OpSetGlobal:
			name := frame.readString()
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
		case OpJump:
			offset := frame.readShort()
			frame.ip += offset
			break
		case OpJumpIfFalse:
			offset := frame.readShort()
			if isFalsey(vm.peek(0)) {
				frame.ip += offset
				break
			}
		case OpLoop:
			offset := frame.readShort()
			frame.ip -= offset
			break
		case OpCall:
			argCount := int(frame.readByte())
			if !vm.callValue(vm.peek(argCount), argCount) {
				return InterpretRuntimeError
			}
			frame = &vm.frames[vm.frameCount-1]
			break
		case OpReturn:
			result := vm.pop()
			vm.frameCount--
			if vm.frameCount == 0 {
				vm.pop()
				return InterpretOk
			}

			vm.stackTop = len(vm.stack) - len(frame.slots)
			vm.push(result)
			frame = &vm.frames[vm.frameCount-1]
			break
		}
	}
}

func (vm *VM) Interpret(source string) InterpretResult {
	function := compile(source)
	if function == nil {
		return InterpretCompileError
	}

	vm.push(functionVal(function))
	vm.call(function, 0)

	return vm.run()
}

func (vm *VM) runtimeError(format string, args ...interface{}) {
	fmt.Printf(format, args...)
	fmt.Println()

	for i := vm.frameCount - 1; i >= 0; i-- {
		frame := &vm.frames[i]
		function := frame.function
		instruction := frame.ip - 1
		fmt.Printf("[line %d] in ", function.chunk.lines[instruction])
		if function.name == "" {
			fmt.Printf("script\n")
		} else {
			fmt.Printf("%s()\n", function.name)
		}
	}

	vm.resetStack()
}

func (vm *VM) defineNative(name string, function NativeFn) {
	vm.push(stringVal(name))
	vm.push(nativeFnVal(function))
	global[asString(vm.stack[0])] = vm.stack[1]
	vm.pop()
	vm.pop()
}

func clockNative(argCount int, args []Value) Value {
	return numberVal(float64(time.Now().UnixNano() / int64(time.Second)))
}
