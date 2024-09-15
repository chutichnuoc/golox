package interpreter

const (
	InterpretOk = 1 << iota
	InterpretCompileError
	InterpretRuntimeError
)
