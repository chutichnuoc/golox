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
	OpGetUpvalue
	OpSetUpvalue
	OpGetProperty
	OpSetProperty
	OpGetSuper
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
	OpCall
	OpInvoke
	OpSuperInvoke
	OpClosure
	OpCloseUpvalue
	OpReturn
	OpClass
	OpInherit
	OpMethod
)
