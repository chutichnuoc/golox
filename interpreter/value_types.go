package interpreter

type ValueType int

const (
	ValBool = iota
	ValNil
	ValNumber
	ValString
	ValClosure
	ValFunction
	ValUpvalue
	ValNativeFn
)
