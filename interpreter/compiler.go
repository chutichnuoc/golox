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
	name       Token
	depth      int
	isCaptured bool
}

type Upvalue struct {
	index   uint8
	isLocal bool
}

type Compiler struct {
	enclosing    *Compiler
	function     *Function
	functionType FunctionType

	locals     [256]Local
	localCount int
	upvalues   [256]Upvalue
	scopeDepth int
}

type ClassCompiler struct {
	enclosing *ClassCompiler
}

var rules []ParseRule

var scanner *Scanner
var parser = Parser{hadError: false, panicMode: false}
var current *Compiler
var currentClass *ClassCompiler

func currentChunk() *Chunk {
	return &current.function.chunk
}

func initCompiler(compiler *Compiler, functionType FunctionType) {
	compiler.enclosing = current
	compiler.functionType = functionType
	compiler.localCount = 0
	compiler.scopeDepth = 0
	compiler.function = newFunction()
	current = compiler
	if functionType != TypeScript {
		current.function.name = parser.previous.lexeme
	}

	local := &current.locals[current.localCount]
	current.localCount++
	local.depth = 0
	local.isCaptured = false
	if functionType != TypeFunction {
		local.name.lexeme = "this"
	} else {
		local.name.lexeme = ""
	}
}

func getRule(tokeType TokenType) ParseRule {
	return rules[int(tokeType)]
}

func compile(source string) *Function {
	rules = []ParseRule{
		TokenLeftParen:    {grouping, call, PrecCall},
		TokenRightParen:   {nil, nil, PrecNone},
		TokenLeftBrace:    {nil, nil, PrecNone},
		TokenRightBrace:   {nil, nil, PrecNone},
		TokenComma:        {nil, nil, PrecNone},
		TokenDot:          {nil, dot, PrecCall},
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
		TokenAnd:          {nil, _and, PrecAnd},
		TokenClass:        {nil, nil, PrecNone},
		TokenElse:         {nil, nil, PrecNone},
		TokenFalse:        {literal, nil, PrecNone},
		TokenFor:          {nil, nil, PrecNone},
		TokenFun:          {nil, nil, PrecNone},
		TokenIf:           {nil, nil, PrecNone},
		TokenNil:          {literal, nil, PrecNone},
		TokenOr:           {nil, _or, PrecOr},
		TokenPrint:        {nil, nil, PrecNone},
		TokenReturn:       {nil, nil, PrecNone},
		TokenSuper:        {nil, nil, PrecNone},
		TokenThis:         {this, nil, PrecNone},
		TokenTrue:         {literal, nil, PrecNone},
		TokenVar:          {nil, nil, PrecNone},
		TokenWhile:        {nil, nil, PrecNone},
		TokenError:        {nil, nil, PrecNone},
		TokenEof:          {nil, nil, PrecNone},
	}

	scanner = initScanner(source)
	var compiler Compiler
	initCompiler(&compiler, TypeScript)

	parser.advance()

	for !match(TokenEof) {
		declaration()
	}

	function := parser.endCompiler()
	if parser.hadError {
		return nil
	} else {
		return function
	}
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

func classDeclaration() {
	parser.consume(TokenIdentifier, "Expect class name.")
	className := parser.previous
	nameConstant := identifierConstant(parser.previous)
	declareVariable()

	parser.emitBytes(OpClass, nameConstant)
	defineVariable(nameConstant)

	classCompiler := ClassCompiler{}
	classCompiler.enclosing = currentClass
	currentClass = &classCompiler

	namedVariable(className, false)
	parser.consume(TokenLeftBrace, "Expect '{' before class body.")
	for !check(TokenRightBrace) && !check(TokenEof) {
		method()
	}
	parser.consume(TokenRightBrace, "Expect '}' after class body.")
	parser.emitByte(OpPop)

	currentClass = currentClass.enclosing
}

func method() {
	parser.consume(TokenIdentifier, "Expect method name.")
	constant := identifierConstant(parser.previous)

	functionType := TypeMethod
	if parser.previous.lexeme == "init" {
		functionType = TypeInitializer
	}
	function(functionType)
	parser.emitBytes(OpMethod, constant)
}

func function(functionType FunctionType) {
	var compiler Compiler
	initCompiler(&compiler, functionType)
	beginScope()

	parser.consume(TokenLeftParen, "Expect '(' after function name.")
	if !check(TokenRightParen) {
		for ok := true; ok; ok = match(TokenComma) {
			current.function.arity++
			if current.function.arity > 255 {
				errorAtCurrent("Can't have more than 255 parameters.")
			}
			constant := parseVariable("Expect parameter name.")
			defineVariable(constant)
		}
	}
	parser.consume(TokenRightParen, "Expect ')' after parameters.")
	parser.consume(TokenLeftBrace, "Expect '{' before function body.")
	block()

	function := parser.endCompiler()
	parser.emitBytes(OpClosure, makeConstant(functionVal(function)))

	for i := 0; i < function.upvalueCount; i++ {
		if compiler.upvalues[i].isLocal {
			parser.emitByte(1)
		} else {
			parser.emitByte(0)
		}
		parser.emitByte(compiler.upvalues[i].index)
	}
}

func funDeclaration() {
	global := parseVariable("Expect function name.")
	markInitialized()
	function(TypeFunction)
	defineVariable(global)
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

func forStatement() {
	beginScope()
	parser.consume(TokenLeftParen, "Expect '(' after 'for'.")
	if match(TokenSemicolon) {
		// No initializer.
	} else if match(TokenVar) {
		varDeclaration()
	} else {
		expressionStatement()
	}

	loopStart := len(currentChunk().code)
	exitJump := -1
	if !match(TokenSemicolon) {
		expression()
		parser.consume(TokenSemicolon, "Expect ';' after loop condition.")

		// Jump out of the loop if the condition is false.
		exitJump = parser.emitJump(OpJumpIfFalse)
		parser.emitByte(OpPop) // Condition.
	}

	if !match(TokenRightParen) {
		bodyJump := parser.emitJump(OpJump)
		incrementStart := len(currentChunk().code)
		expression()
		parser.emitByte(OpPop)
		parser.consume(TokenRightParen, "Expect ')' after for clauses.")

		parser.emitLoop(loopStart)
		loopStart = incrementStart
		parser.patchJump(bodyJump)
	}

	statement()
	parser.emitLoop(loopStart)

	if exitJump != -1 {
		parser.patchJump(exitJump)
		parser.emitByte(OpPop)
	}

	endScope()
}

func ifStatement() {
	parser.consume(TokenLeftParen, "Expect '(' after if.")
	expression()
	parser.consume(TokenRightParen, "Expect ')' after condition.")

	thenJump := parser.emitJump(OpJumpIfFalse)
	parser.emitByte(OpPop)
	statement()

	elseJump := parser.emitJump(OpJump)

	parser.patchJump(thenJump)
	parser.emitByte(OpPop)

	if match(TokenElse) {
		statement()
	}
	parser.patchJump(elseJump)
}

func printStatement() {
	expression()
	parser.consume(TokenSemicolon, "Expect ';' after value.")
	parser.emitByte(OpPrint)
}

func returnStatement() {
	if current.functionType == TypeScript {
		_error("Can't return from top-level code.")
	}

	if match(TokenSemicolon) {
		parser.emitReturn()
	} else {
		if current.functionType == TypeInitializer {
			_error("Can't return a value from an initializer.")
		}

		expression()
		parser.consume(TokenSemicolon, "Expect ';' after return value.")
		parser.emitByte(OpReturn)
	}
}

func whileStatement() {
	loopStart := len(currentChunk().code)
	parser.consume(TokenLeftParen, "Expect '(' after 'while'.")
	expression()
	parser.consume(TokenRightParen, "Expect ')' after condition.")

	exitJump := parser.emitJump(OpJumpIfFalse)
	parser.emitByte(OpPop)
	statement()
	parser.emitLoop(loopStart)

	parser.patchJump(exitJump)
	parser.emitByte(OpPop)
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
	if match(TokenClass) {
		classDeclaration()
	} else if match(TokenFun) {
		funDeclaration()
	} else if match(TokenVar) {
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
	} else if match(TokenFor) {
		forStatement()
	} else if match(TokenIf) {
		ifStatement()
	} else if match(TokenReturn) {
		returnStatement()
	} else if match(TokenWhile) {
		whileStatement()
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

func (parser *Parser) endCompiler() *Function {
	parser.emitReturn()
	function := current.function

	if DebugPrintCode {
		if !parser.hadError {
			var functionName string
			if function.name != "" {
				functionName = function.name
			} else {
				functionName = "<script>"
			}
			disassembleChunk(currentChunk(), functionName)
		}
	}

	current = current.enclosing
	return function
}

func beginScope() {
	current.scopeDepth++
}

func endScope() {
	current.scopeDepth--

	for current.localCount > 0 && current.locals[current.localCount-1].depth > current.scopeDepth {
		if current.locals[current.localCount-1].isCaptured {
			parser.emitByte(OpCloseUpvalue)
		} else {
			parser.emitByte(OpPop)
		}
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

func addUpvalue(compiler *Compiler, index uint8, isLocal bool) int {
	upvalueCount := compiler.function.upvalueCount

	for i := 0; i < upvalueCount; i++ {
		upvalue := &compiler.upvalues[i]
		if upvalue.index == index && upvalue.isLocal == isLocal {
			return i
		}
	}

	if upvalueCount == 256 {
		_error("Too many closure variables in function.")
		return 0
	}

	compiler.upvalues[upvalueCount].isLocal = isLocal
	compiler.upvalues[upvalueCount].index = index
	compiler.function.upvalueCount++
	return upvalueCount
}

func resolveUpvalue(compiler *Compiler, name Token) int {
	if compiler.enclosing == nil {
		return -1
	}

	local := resolveLocal(compiler.enclosing, name)
	if local != -1 {
		compiler.enclosing.locals[local].isCaptured = true
		return addUpvalue(compiler, uint8(local), true)
	}

	upvalue := resolveUpvalue(compiler.enclosing, name)
	if upvalue != -1 {
		return addUpvalue(compiler, uint8(upvalue), false)
	}

	return -1
}

func addLocal(name Token) {
	if current.localCount == 256 {
		_error("Too many local variables in function.")
		return
	}

	local := &current.locals[current.localCount]
	current.localCount++
	local.name = name
	local.depth = -1
	local.isCaptured = false
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
	if current.scopeDepth == 0 {
		return
	}
	current.locals[current.localCount-1].depth = current.scopeDepth
}

func defineVariable(global uint8) {
	if current.scopeDepth > 0 {
		markInitialized()
		return
	}
	parser.emitBytes(OpDefineGlobal, global)
}

func argumentList() uint8 {
	var argCount uint8 = 0
	if !check(TokenRightParen) {
		for ok := true; ok; ok = match(TokenComma) {
			expression()
			if argCount == 255 {
				_error("Can't have more than 255 arguments.")
			}
			argCount++
		}
	}
	parser.consume(TokenRightParen, "Expect ')' after arguments.")
	return argCount
}

func _and(canAssign bool) {
	endJump := parser.emitJump(OpJumpIfFalse)

	parser.emitByte(OpPop)
	parsePrecedence(PrecAnd)

	parser.patchJump(endJump)
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

func call(canAssign bool) {
	argCount := argumentList()
	parser.emitBytes(OpCall, argCount)
}

func dot(canAssign bool) {
	parser.consume(TokenIdentifier, "Expect property name after '.'.")
	name := identifierConstant(parser.previous)

	if canAssign && match(TokenEqual) {
		expression()
		parser.emitBytes(OpSetProperty, name)
	} else if match(TokenLeftParen) {
		argCount := argumentList()
		parser.emitBytes(OpInvoke, name)
		parser.emitByte(argCount)
	} else {
		parser.emitBytes(OpGetProperty, name)
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

func _or(canAssign bool) {
	elseJump := parser.emitJump(OpJumpIfFalse)
	endJump := parser.emitJump(OpJump)

	parser.patchJump(elseJump)
	parser.emitByte(OpPop)

	parsePrecedence(PrecOr)
	parser.patchJump(endJump)
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
	} else if arg = resolveUpvalue(current, name); arg != -1 {
		getOp = OpGetUpvalue
		setOp = OpSetUpvalue
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

func this(canAssign bool) {
	if currentClass == nil {
		_error("Can't use 'this' outside of a class.")
		return
	}

	variable(false)
}

func makeConstant(value Value) uint8 {
	constant := currentChunk().AddConstant(value)
	if constant > 255 {
		_error("Too many constants in one chunk.")
		return 0
	}
	return constant
}

func (parser *Parser) patchJump(offset int) {
	// -2 to adjust for the bytecode for the jump offset itself.
	jump := len(currentChunk().code) - offset - 2

	if jump > 65535 {
		_error("Too much code to jump over.")
	}

	currentChunk().code[offset] = uint8((jump >> 8) & 0xff)
	currentChunk().code[offset+1] = uint8(jump & 0xff)
}

func (parser *Parser) emitLoop(loopStart int) {
	parser.emitByte(OpLoop)

	offset := len(currentChunk().code) - loopStart + 2
	if offset > 65535 {
		_error("Loop body too large.")
	}

	parser.emitByte(byte((offset >> 8) & 0xff))
	parser.emitByte(byte(offset & 0xff))
}

func (parser *Parser) emitConstant(value Value) {
	parser.emitBytes(OpConstant, makeConstant(value))
}

func (parser *Parser) emitReturn() {
	if current.functionType == TypeInitializer {
		parser.emitBytes(OpGetLocal, 0)
	} else {
		parser.emitByte(OpNil)
	}

	parser.emitByte(OpReturn)
}

func (parser *Parser) emitJump(instruction uint8) int {
	parser.emitByte(instruction)
	parser.emitByte(0xff)
	parser.emitByte(0xff)
	return len(currentChunk().code) - 2
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
