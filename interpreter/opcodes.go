package interpreter

const (
	OpConstant = iota
	OpNil
	OpTrue
	OpFalse
	OpPop
	OpGetLocal
	OpSetLocal
	OpGetGlobal
	OpDefineGlobal
	OpSetGlobal
	OpEqual
	OpGreater
	OpLess
	OpAdd
	OpSubtract
	OpMultiply
	OpDivide
	OpNot
	OpNegate
	OpPrint
	OpJump
	OpJumpIfFalse
	OpLoop
	OpReturn
)
