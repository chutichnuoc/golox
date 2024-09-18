package interpreter

type InterpretResult int

const (
	InterpretOk = iota
	InterpretCompileError
	InterpretRuntimeError
)
