package interpreter

type ValueType int

const (
	ValBool = iota
	ValNil
	ValNumber
	ValString
	ValBoundMethod
	ValClass
	ValInstance
	ValClosure
	ValFunction
	ValUpvalue
	ValNativeFn
)
