package interpreter

import "fmt"

type NativeFn func(argCount int, args []Value) Value

type Value struct {
	valueType ValueType
	as        union
}

type union struct {
	boolean  bool
	number   float64
	string   string
	closure  *Closure
	function *Function
	nativeFn NativeFn
}

type Closure struct {
	function     *Function
	upvalues     []*ObjUpvalue
	upvalueCount int
}

type Function struct {
	arity        int
	upvalueCount int
	chunk        Chunk
	name         string
}

type ObjUpvalue struct {
	location *Value
	closed   Value
	next     *ObjUpvalue
}

func newClosure(function *Function) *Closure {
	closure := Closure{}
	closure.function = function
	closure.upvalues = make([]*ObjUpvalue, 256)
	closure.upvalueCount = function.upvalueCount
	return &closure
}

func newFunction() *Function {
	function := Function{}
	function.chunk = *NewChunk()
	return &function
}

func newUpvalue(slot *Value) *ObjUpvalue {
	upvalue := &ObjUpvalue{}
	upvalue.location = slot
	upvalue.next = nil
	return upvalue
}

func boolVal(value bool) Value {
	return Value{valueType: ValBool, as: union{boolean: value}}
}

func nilVal() Value {
	return Value{valueType: ValNil, as: union{number: 0}}
}

func numberVal(value float64) Value {
	return Value{valueType: ValNumber, as: union{number: value}}
}

func closureVal(value *Closure) Value {
	return Value{valueType: ValClosure, as: union{closure: value}}
}

func functionVal(value *Function) Value {
	return Value{valueType: ValFunction, as: union{function: value}}
}

func nativeFnVal(value NativeFn) Value {
	return Value{valueType: ValNativeFn, as: union{nativeFn: value}}
}

func stringVal(value string) Value {
	return Value{valueType: ValString, as: union{string: value}}
}

func asBool(value Value) bool {
	return value.as.boolean
}

func asNumber(value Value) float64 {
	return value.as.number
}

func asClosure(value Value) *Closure {
	return value.as.closure
}

func asFunction(value Value) *Function {
	return value.as.function
}

func asNativeFn(value Value) NativeFn {
	return value.as.nativeFn
}

func asString(value Value) string {
	return value.as.string
}

func isBool(value Value) bool {
	return value.valueType == ValBool
}

func isNil(value Value) bool {
	return value.valueType == ValNil
}

func isNumber(value Value) bool {
	return value.valueType == ValNumber
}

func isClosure(value Value) bool {
	return value.valueType == ValClosure
}

func isFunction(value Value) bool {
	return value.valueType == ValFunction
}

func isNativeFn(value Value) bool {
	return value.valueType == ValNativeFn
}

func isString(value Value) bool {
	return value.valueType == ValString
}

type ValueArray struct {
	values []Value
}

func newValueArray() *ValueArray {
	valueArray := &ValueArray{}
	valueArray.values = make([]Value, 0)
	return valueArray
}

func (valueArray *ValueArray) write(value Value) {
	valueArray.values = append(valueArray.values, value)
}

func (valueArray *ValueArray) free() {
	valueArray.values = make([]Value, 0)
}

func printValue(value Value) {
	switch value.valueType {
	case ValBool:
		if asBool(value) {
			fmt.Print("true")
		} else {
			fmt.Print("false")
		}
		break
	case ValNil:
		fmt.Print("nil")
		break
	case ValNumber:
		fmt.Printf("%g", asNumber(value))
	case ValClosure:
		printFunction(asClosure(value).function)
		break
	case ValFunction:
		printFunction(asFunction(value))
		break
	case ValNativeFn:
		fmt.Printf("<native fn>")
		break
	case ValString:
		fmt.Printf("%s", asString(value))
		break
	case ValUpvalue:
		fmt.Printf("upvalue")
	}
}

func printFunction(function *Function) {
	if function.name == "" {
		fmt.Printf("<script>")
		return
	}
	fmt.Printf("<fn %s>", function.name)
}

func valuesEqual(a Value, b Value) bool {
	if a.valueType != b.valueType {
		return false
	}
	switch a.valueType {
	case ValBool:
		return asBool(a) == asBool(b)
	case ValNil:
		return true
	case ValNumber:
		return asNumber(a) == asNumber(b)
	case ValString:
		return asString(a) == asString(b)
	default:
		return false // Unreachable.
	}
}
