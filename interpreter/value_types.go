package interpreter

type ValueType int

const (
	ValBool = iota
	ValNil
	ValNumber
	ValString
)
