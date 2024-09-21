package interpreter

import (
	"fmt"
	"strconv"
)

type ParseFn func(canAssign bool)

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

type Local struct {
	name  Token
	depth int
}

type Compiler struct {
	locals     [256]Local
	localCount int
	scopeDepth int
}

var rules []ParseRule

var scanner *Scanner
var parser = Parser{hadError: false, panicMode: false}
var current *Compiler
var compilingChunk *Chunk

func currentChunk() *Chunk {
	return compilingChunk
}

func initCompiler() {
	compiler := Compiler{localCount: 0, scopeDepth: 0}
	current = &compiler
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
		TokenIdentifier:   {variable, nil, PrecNone},
		TokenString:       {_string, nil, PrecNone},
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
	initCompiler()
	compilingChunk = chunk

	parser.advance()

	for !match(TokenEof) {
		declaration()
	}

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

func expression() {
	parsePrecedence(PrecAssignment)
}

func block() {
	for !check(TokenRightBrace) && !check(TokenEof) {
		declaration()
	}

	parser.consume(TokenRightBrace, "Expect '}' after block.")
}

func varDeclaration() {
	global := parseVariable("Expect variable name.")

	if match(TokenEqual) {
		expression()
	} else {
		parser.emitByte(OpNil)
	}
	parser.consume(TokenSemicolon, "Expect ';' after variable declaration.")

	defineVariable(global)
}

func expressionStatement() {
	expression()
	parser.consume(TokenSemicolon, "Expect ';' after expression.")
	parser.emitByte(OpPop)
}

func printStatement() {
	expression()
	parser.consume(TokenSemicolon, "Expect ';' after value.")
	parser.emitByte(OpPrint)
}

func synchronize() {
	parser.panicMode = false
	for parser.current.tokenType != TokenEof {
		if parser.previous.tokenType == TokenSemicolon {
			return
		}
		switch parser.current.tokenType {
		case TokenClass:
		case TokenFun:
		case TokenVar:
		case TokenFor:
		case TokenIf:
		case TokenWhile:
		case TokenPrint:
		case TokenReturn:
			return
		default:
			// Do nothing.
		}
		parser.advance()
	}
}

func declaration() {
	if match(TokenVar) {
		varDeclaration()
	} else {
		statement()
	}

	if parser.panicMode {
		synchronize()
	}
}

func statement() {
	if match(TokenPrint) {
		printStatement()
	} else if match(TokenLeftBrace) {
		beginScope()
		block()
		endScope()
	} else {
		expressionStatement()
	}
}

func (parser *Parser) consume(tokenType TokenType, message string) {
	if parser.current.tokenType == tokenType {
		parser.advance()
		return
	}

	errorAtCurrent(message)
}

func check(tokenType TokenType) bool {
	return parser.current.tokenType == tokenType
}

func match(tokenType TokenType) bool {
	if !check(tokenType) {
		return false
	}
	parser.advance()
	return true
}

func (parser *Parser) endCompiler() {
	parser.emitReturn()
	if DebugPrintCode {
		if !parser.hadError {
			DisassembleChunk(currentChunk(), "code")
		}
	}
}

func beginScope() {
	current.scopeDepth++
}

func endScope() {
	current.scopeDepth--

	for current.localCount > 0 && current.locals[current.localCount-1].depth > current.scopeDepth {
		parser.emitByte(OpPop)
		current.localCount--
	}
}

func parsePrecedence(precedence Precedence) {
	parser.advance()
	prefixRule := getRule(parser.previous.tokenType).prefix
	if prefixRule == nil {
		_error("Expect expression.")
		return
	}

	canAssign := precedence <= PrecAssignment
	prefixRule(canAssign)

	for precedence <= getRule(parser.current.tokenType).precedence {
		parser.advance()
		infixRule := getRule(parser.previous.tokenType).infix
		infixRule(canAssign)
	}

	if canAssign && match(TokenEqual) {
		_error("Invalid assignment target.")
	}
}

func identifierConstant(name Token) uint8 {
	return makeConstant(stringVal(name.lexeme))
}

func identifiersEqual(a Token, b Token) bool {
	return a.lexeme == b.lexeme
}

func resolveLocal(compiler *Compiler, name Token) int {
	for i := compiler.localCount - 1; i >= 0; i-- {
		local := compiler.locals[i]
		if identifiersEqual(name, local.name) {
			if local.depth == -1 {
				_error("Can't read local variable in its own initializer.")
			}
			return i
		}
	}
	return -1
}

func addLocal(name Token) {
	if current.localCount == 256 {
		_error("Too many local variables in function.")
		return
	}

	local := current.locals[current.localCount]
	current.localCount++
	local.name = name
	local.depth = -1
}

func declareVariable() {
	if current.scopeDepth == 0 {
		return
	}
	name := parser.previous
	for i := current.localCount - 1; i >= 0; i-- {
		local := current.locals[i]
		if local.depth != -1 && local.depth < current.scopeDepth {
			break
		}
		if identifiersEqual(name, local.name) {
			_error("Already a variable with this name in this scope.")
		}
	}
	addLocal(name)
}

func parseVariable(errorMessage string) uint8 {
	parser.consume(TokenIdentifier, errorMessage)

	declareVariable()
	if current.scopeDepth > 0 {
		return 0
	}

	return identifierConstant(parser.previous)
}

func markInitialized() {
	current.locals[current.localCount-1].depth = current.scopeDepth
}

func defineVariable(global uint8) {
	if current.scopeDepth > 0 {
		markInitialized()
		return
	}
	parser.emitBytes(OpDefineGlobal, global)
}

func grouping(canAssign bool) {
	expression()
	parser.consume(TokenRightParen, "Expect ')' after expression.")
}

func unary(canAssign bool) {
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

func binary(canAssign bool) {
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

func literal(canAssign bool) {
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

func number(canAssign bool) {
	value, _ := strconv.ParseFloat(parser.previous.lexeme, 64)
	parser.emitConstant(numberVal(value))
}

func _string(canAssign bool) {
	stringValue := Value{valueType: ValString, as: union{string: parser.previous.lexeme}}
	parser.emitConstant(stringValue)
}

func namedVariable(name Token, canAssign bool) {
	var getOp uint8
	var setOp uint8
	arg := resolveLocal(current, name)
	if arg != -1 {
		getOp = OpGetLocal
		setOp = OpSetLocal
	} else {
		arg = int(identifierConstant(name))
		getOp = OpGetGlobal
		setOp = OpSetGlobal
	}

	if canAssign && match(TokenEqual) {
		expression()
		parser.emitBytes(setOp, uint8(arg))
	} else {
		parser.emitBytes(getOp, uint8(arg))
	}
}

func variable(canAssign bool) {
	namedVariable(parser.previous, canAssign)
}

func makeConstant(value Value) uint8 {
	constant := currentChunk().AddConstant(value)
	if constant > 255 {
		_error("Too many constants in one chunk.")
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

func _error(message string) {
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
