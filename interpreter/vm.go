package interpreter

import (
	"fmt"
	"time"
)

const FramesMax int = 64
const StackMax = FramesMax * 256

var global = make(map[string]Value)

type CallFrame struct {
	closure *Closure
	ip      uint16
	slots   []Value
}

type VM struct {
	frames       [FramesMax]CallFrame
	frameCount   int
	stack        [StackMax]Value
	stackTop     int
	openUpvalues *Upvalue
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
	vm.openUpvalues = nil
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

func (vm *VM) call(closure *Closure, argCount int) bool {
	if argCount != closure.function.arity {
		vm.runtimeError("Expected %d arguments but got %d.", closure.function.arity, argCount)
		return false
	}

	if vm.frameCount == FramesMax {
		vm.runtimeError("Stack overflow.")
		return false
	}

	frame := &vm.frames[vm.frameCount]
	vm.frameCount++
	frame.closure = closure
	frame.ip = 0
	frame.slots = vm.stack[vm.stackTop-argCount-1:]
	return true
}

func (vm *VM) callValue(callee Value, argCount int) bool {
	if isBoundMethod(callee) {
		bound := asBoundMethod(callee)
		vm.stack[vm.stackTop-argCount-1] = bound.receiver
		return vm.call(bound.method, argCount)
	} else if isClass(callee) {
		class := asClass(callee)
		vm.stack[vm.stackTop-argCount-1] = instanceVal(newInstance(class))
		initializer, ok := class.methods["init"]
		if ok {
			return vm.call(asClosure(initializer), argCount)
		} else if argCount != 0 {
			vm.runtimeError("Expected 0 arguments but got %d.", argCount)
			return false
		}
		return true
	} else if isClosure(callee) {
		return vm.call(asClosure(callee), argCount)
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

func (vm *VM) invokeFromClass(class *Class, name string, argCount int) bool {
	method, ok := class.methods[name]
	if !ok {
		vm.runtimeError("Undefined property '%s'.", name)
		return false
	}
	return vm.call(asClosure(method), argCount)
}

func (vm *VM) invoke(name string, argCount int) bool {
	receiver := vm.peek(argCount)

	if !isInstance(receiver) {
		vm.runtimeError("Only instances have methods..")
		return false
	}

	instance := asInstance(receiver)

	value, ok := instance.fields[name]
	if ok {
		vm.stack[vm.stackTop-argCount-1] = value
		return vm.callValue(value, argCount)
	}

	return vm.invokeFromClass(instance.class, name, argCount)
}

func (vm *VM) bindMethod(class *Class, name string) bool {
	method, ok := class.methods[name]
	if !ok {
		vm.runtimeError("Undefined property '%s'.", name)
		return false
	}

	bound := newBoundMethod(vm.peek(0), asClosure(method))
	vm.pop()
	vm.push(boundMethodVal(bound))
	return true
}

func (vm *VM) captureUpvalue(local *Value) *Upvalue {
	var prevUpvalue *Upvalue
	upvalue := vm.openUpvalues
	for upvalue != nil && vm.findStackIndex(upvalue.location) > vm.findStackIndex(local) {
		prevUpvalue = upvalue
		upvalue = upvalue.next
	}

	if upvalue != nil && upvalue.location == local {
		return upvalue
	}

	createdUpvalue := newUpvalue(local)
	createdUpvalue.next = upvalue

	if prevUpvalue == nil {
		vm.openUpvalues = createdUpvalue
	} else {
		prevUpvalue.next = createdUpvalue
	}
	return createdUpvalue
}

func (vm *VM) closeUpvalues(last *Value) {
	for vm.openUpvalues != nil && vm.findStackIndex(vm.openUpvalues.location) >= vm.findStackIndex(last) {
		upvalue := vm.openUpvalues
		upvalue.closed = *upvalue.location
		upvalue.location = &upvalue.closed
		vm.openUpvalues = upvalue.next
	}
}

func (vm *VM) findStackIndex(value *Value) int {
	for i := vm.stackTop - 1; i >= 0; i-- {
		if &vm.stack[i] == value {
			return i
		}
	}
	return -1
}

func (vm *VM) defineMethod(name string) {
	method := vm.peek(0)
	class := asClass(vm.peek(1))
	class.methods[name] = method
	vm.pop()
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
			disassembleInstruction(&frame.closure.function.chunk, int(frame.ip))
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
		case OpGetUpvalue:
			slot := frame.readByte()
			vm.push(*frame.closure.upvalues[slot].location)
			break
		case OpSetUpvalue:
			slot := frame.readByte()
			*frame.closure.upvalues[slot].location = vm.peek(0)
			break
		case OpGetProperty:
			if !isInstance(vm.peek(0)) {
				vm.runtimeError("Only instances have properties.")
				return InterpretRuntimeError
			}

			instance := asInstance(vm.peek(0))
			name := frame.readString()

			value, ok := instance.fields[name]
			if ok {
				vm.pop() // Instance.
				vm.push(value)
				break
			}

			if !vm.bindMethod(instance.class, name) {
				return InterpretRuntimeError
			}
			break
		case OpSetProperty:
			if !isInstance(vm.peek(1)) {
				vm.runtimeError("Only instances have fields.")
				return InterpretRuntimeError
			}

			instance := asInstance(vm.peek(1))
			instance.fields[frame.readString()] = vm.peek(0)
			value := vm.pop()
			vm.pop()
			vm.push(value)
			break
		case OpGetSuper:
			name := frame.readString()
			superclass := asClass(vm.pop())

			if !vm.bindMethod(superclass, name) {
				return InterpretRuntimeError
			}
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
		case OpInvoke:
			method := frame.readString()
			argCount := int(frame.readByte())
			if !vm.invoke(method, argCount) {
				return InterpretRuntimeError
			}
			frame = &vm.frames[vm.frameCount-1]
			break
		case OpSuperInvoke:
			superMethod := frame.readString()
			argCount := int(frame.readByte())
			superclass := asClass(vm.pop())
			if !vm.invokeFromClass(superclass, superMethod, argCount) {
				return InterpretRuntimeError
			}
			frame = &vm.frames[vm.frameCount-1]
			break
		case OpClosure:
			function := asFunction(frame.readConstant())
			closure := newClosure(function)
			vm.push(closureVal(closure))
			for i := 0; i < closure.upvalueCount; i++ {
				isLocal := frame.readByte()
				index := frame.readByte()
				if isLocal == 1 {
					closure.upvalues[i] = vm.captureUpvalue(&frame.slots[index])
				} else {
					closure.upvalues[i] = frame.closure.upvalues[index]
				}
			}
			break
		case OpCloseUpvalue:
			vm.closeUpvalues(&vm.stack[vm.stackTop])
			vm.pop()
			break
		case OpReturn:
			result := vm.pop()
			vm.closeUpvalues(&frame.slots[0])
			vm.frameCount--
			if vm.frameCount == 0 {
				vm.pop()
				return InterpretOk
			}

			vm.stackTop = len(vm.stack) - len(frame.slots)
			vm.push(result)
			frame = &vm.frames[vm.frameCount-1]
			break
		case OpClass:
			vm.push(classVal(newClass(frame.readString())))
			break
		case OpInherit:
			superclass := vm.peek(1)
			if !isClass(superclass) {
				vm.runtimeError("Superclass must be a class.")
				return InterpretRuntimeError
			}
			subclass := asClass(vm.peek(0))
			for key, value := range asClass(superclass).methods {
				subclass.methods[key] = value
			}
			vm.pop() // Subclass.
			break
		case OpMethod:
			vm.defineMethod(frame.readString())
			break
		}
	}
}

func (vm *VM) Interpret(source string) InterpretResult {
	function := compile(source)
	if function == nil {
		return InterpretCompileError
	}

	closure := newClosure(function)
	vm.push(closureVal(closure))
	vm.call(closure, 0)

	return vm.run()
}

func (vm *VM) runtimeError(format string, args ...interface{}) {
	fmt.Printf(format, args...)
	fmt.Println()

	for i := vm.frameCount - 1; i >= 0; i-- {
		frame := &vm.frames[i]
		function := frame.closure.function
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

func (frame *CallFrame) readByte() uint8 {
	code := frame.closure.function.chunk.code[frame.ip]
	frame.ip++
	return code
}

func (frame *CallFrame) readConstant() Value {
	return frame.closure.function.chunk.constants.values[frame.readByte()]
}

func (frame *CallFrame) readShort() uint16 {
	frame.ip += 2
	return uint16((frame.closure.function.chunk.code[frame.ip-2] << 8) | frame.closure.function.chunk.code[frame.ip-1])
}

func (frame *CallFrame) readString() string {
	return asString(frame.readConstant())
}

func clockNative(int, []Value) Value {
	return numberVal(float64(time.Now().UnixNano() / int64(time.Second)))
}

func isFalsey(value Value) bool {
	return isNil(value) || (isBool(value) && !asBool(value))
}
