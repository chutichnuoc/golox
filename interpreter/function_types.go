package interpreter

type FunctionType int

const (
	TypeFunction FunctionType = iota
	TypeInitializer
	TypeMethod
	TypeScript
)
