package interpreter

import "fmt"

//type Value float64

type Value struct {
	valueType ValueType
	as        union
}

type union struct {
	boolean bool
	number  float64
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

func asBool(value Value) bool {
	return value.as.boolean
}

func asNumber(value Value) float64 {
	return value.as.number
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
	}

}

func valueEqual(a Value, b Value) bool {
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
	default:
		return false // Unreachable.
	}
}
