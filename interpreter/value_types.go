package interpreter

type ValueType int

const (
	ValBool ValueType = iota
	ValNil
	ValNumber
	ValString
	ValFunction
	ValClosure
	ValUpvalue
	ValNativeFn
	ValClass
	ValInstance
	ValBoundMethod
)
