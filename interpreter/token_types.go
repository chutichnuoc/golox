package interpreter

type TokenType float64

const (
	// Single-character tokens.
	TokenLeftParen = iota
	TokenRightParen
	TokenLeftBrace
	TokenRightBrace
	TokenComma
	TokenDot
	TokenMinus
	TokenPlus
	TokenSemicolon
	TokenSlash
	TokenStar
	// One or two character tokens.
	TokenBang
	TokenBangEqual
	TokenEqual
	TokenEqualEqual
	TokenGreater
	TokenGreaterEqual
	TokenLess
	TokenLessEqual
	// Literals.
	TokenIdentifier
	TokenString
	TokenNumber
	// Keywords.
	TokenAnd
	TokenClass
	TokenElse
	TokenFalse
	TokenFor
	TokenFun
	TokenIf
	TokenNil
	TokenOr
	TokenPrint
	TokenReturn
	TokenSuper
	TokenThis
	TokenTrue
	TokenVar
	TokenWhile
	TokenError
	TokenEof
)
