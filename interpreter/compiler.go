package interpreter

import (
	"fmt"
	"strconv"
)

type ParseFn func()

type Parser struct {
	current   Token
	previous  Token
	hadError  bool
	panicMode bool
}

type ParseRule struct {
	prefix     ParseFn
	infix      ParseFn
	precedence Precedence
}

var rules []ParseRule

var scanner *Scanner
var parser = Parser{hadError: false, panicMode: false}
var compilingChunk *Chunk

func currentChunk() *Chunk {
	return compilingChunk
}

func getRule(tokeType TokenType) ParseRule {
	return rules[int(tokeType)]
}

func compile(source string, chunk *Chunk) bool {
	rules = []ParseRule{
		TokenLeftParen:    {grouping, nil, PrecNone},
		TokenRightParen:   {nil, nil, PrecNone},
		TokenLeftBrace:    {nil, nil, PrecNone},
		TokenRightBrace:   {nil, nil, PrecNone},
		TokenComma:        {nil, nil, PrecNone},
		TokenDot:          {nil, nil, PrecNone},
		TokenMinus:        {unary, binary, PrecTerm},
		TokenPlus:         {nil, binary, PrecTerm},
		TokenSemicolon:    {nil, nil, PrecNone},
		TokenSlash:        {nil, binary, PrecFactor},
		TokenStar:         {nil, binary, PrecFactor},
		TokenBang:         {unary, nil, PrecNone},
		TokenBangEqual:    {nil, binary, PrecEquality},
		TokenEqual:        {nil, nil, PrecNone},
		TokenEqualEqual:   {nil, binary, PrecEquality},
		TokenGreater:      {nil, binary, PrecComparison},
		TokenGreaterEqual: {nil, binary, PrecComparison},
		TokenLess:         {nil, binary, PrecComparison},
		TokenLessEqual:    {nil, binary, PrecComparison},
		TokenIdentifier:   {nil, nil, PrecNone},
		TokenString:       {nil, nil, PrecNone},
		TokenNumber:       {number, nil, PrecNone},
		TokenAnd:          {nil, nil, PrecNone},
		TokenClass:        {nil, nil, PrecNone},
		TokenElse:         {nil, nil, PrecNone},
		TokenFalse:        {literal, nil, PrecNone},
		TokenFor:          {nil, nil, PrecNone},
		TokenFun:          {nil, nil, PrecNone},
		TokenIf:           {nil, nil, PrecNone},
		TokenNil:          {literal, nil, PrecNone},
		TokenOr:           {nil, nil, PrecNone},
		TokenPrint:        {nil, nil, PrecNone},
		TokenReturn:       {nil, nil, PrecNone},
		TokenSuper:        {nil, nil, PrecNone},
		TokenThis:         {nil, nil, PrecNone},
		TokenTrue:         {literal, nil, PrecNone},
		TokenVar:          {nil, nil, PrecNone},
		TokenWhile:        {nil, nil, PrecNone},
		TokenError:        {nil, nil, PrecNone},
		TokenEof:          {nil, nil, PrecNone},
	}

	scanner = initScanner(source)
	compilingChunk = chunk

	parser.advance()
	parser.expression()
	parser.consume(TokenEof, "Expect end of expression.")
	parser.endCompiler()
	return !parser.hadError
}

func (parser *Parser) advance() {
	parser.previous = parser.current

	for {
		parser.current = scanner.scanToken()
		if parser.current.tokenType != TokenError {
			break
		}
		errorAtCurrent(parser.current.lexeme)
	}
}

func (parser *Parser) expression() {
	parsePrecedence(PrecAssignment)
}

func (parser *Parser) consume(tokenType TokenType, message string) {
	if parser.current.tokenType == tokenType {
		parser.advance()
		return
	}

	errorAtCurrent(message)
}

func (parser *Parser) endCompiler() {
	parser.emitReturn()
	if DebugPrintCode {
		if !parser.hadError {
			DisassembleChunk(currentChunk(), "code")
		}
	}
}

func parsePrecedence(precedence Precedence) {
	parser.advance()
	prefixRule := getRule(parser.previous.tokenType).prefix
	if prefixRule == nil {
		error("Expect expression.")
		return
	}

	prefixRule()

	for precedence <= getRule(parser.current.tokenType).precedence {
		parser.advance()
		infixRule := getRule(parser.previous.tokenType).infix
		infixRule()
	}
}

func grouping() {
	parser.expression()
	parser.consume(TokenRightParen, "Expect ')' after expression.")
}

func unary() {
	operatorType := parser.previous.tokenType

	// Compile the operand.
	parsePrecedence(PrecUnary)

	// Emit the operator instruction.
	switch operatorType {
	case TokenBang:
		parser.emitByte(OpNot)
		break
	case TokenMinus:
		parser.emitByte(OpNegate)
		break
	default:
		return // Unreachable.
	}
}

func binary() {
	operatorType := parser.previous.tokenType
	rule := getRule(operatorType)
	parsePrecedence(rule.precedence + 1)

	switch operatorType {
	case TokenBangEqual:
		parser.emitBytes(OpEqual, OpNot)
		break
	case TokenEqualEqual:
		parser.emitByte(OpEqual)
		break
	case TokenGreater:
		parser.emitByte(OpGreater)
		break
	case TokenLess:
		parser.emitByte(OpLess)
		break
	case TokenLessEqual:
		parser.emitBytes(OpGreater, OpNot)
		break
	case TokenGreaterEqual:
		parser.emitBytes(OpLess, OpNot)
		break
	case TokenPlus:
		parser.emitByte(OpAdd)
		break
	case TokenMinus:
		parser.emitByte(OpSubtract)
		break
	case TokenStar:
		parser.emitByte(OpMultiply)
		break
	case TokenSlash:
		parser.emitByte(OpDivide)
		break
	default:
		return // Unreachable.
	}
}

func literal() {
	switch parser.previous.tokenType {
	case TokenFalse:
		parser.emitByte(OpFalse)
		break
	case TokenNil:
		parser.emitByte(OpNil)
		break
	case TokenTrue:
		parser.emitByte(OpTrue)
		break
	default:
		return // Unreachable.
	}
}

func number() {
	value, _ := strconv.ParseFloat(parser.previous.lexeme, 64)
	parser.emitConstant(numberVal(value))
}

func makeConstant(value Value) uint8 {
	constant := currentChunk().AddConstant(value)
	if constant > 255 {
		error("Too many constants in one chunk.")
		return 0
	}
	return constant
}

func (parser *Parser) emitConstant(value Value) {
	parser.emitBytes(OpConstant, makeConstant(value))
}

func (parser *Parser) emitReturn() {
	parser.emitByte(OpReturn)
}

func (parser *Parser) emitBytes(byte1 uint8, byte2 uint8) {
	parser.emitByte(byte1)
	parser.emitByte(byte2)
}

func (parser *Parser) emitByte(byte uint8) {
	currentChunk().Write(byte, parser.previous.line)
}

func error(message string) {
	errorAt(parser.previous, message)
}

func errorAtCurrent(message string) {
	errorAt(parser.current, message)
}

func errorAt(token Token, message string) {
	if parser.panicMode {
		return
	}
	parser.panicMode = true
	fmt.Printf("[line %d] Error", token.line)

	if token.tokenType == TokenEof {
		fmt.Print(" at end")
	} else if token.tokenType == TokenError {
		// Nothing.
	} else {
		fmt.Printf(" at '%s'", token.lexeme)
	}

	fmt.Printf(": %s\n", message)
	parser.hadError = true
}
