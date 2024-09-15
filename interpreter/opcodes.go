package interpreter

const (
	OpConstant = 1 << iota
	OpAdd
	OpSubtract
	OpMultiply
	OpDivide
	OpNegate
	OpReturn
)
