package interpreter

import (
	"fmt"
	"strconv"
)

type ParseFn func(canAssign bool)

type ParseRule struct {
	prefix     ParseFn
	infix      ParseFn
	precedence Precedence
}

type Parser struct {
	current   Token
	previous  Token
	hadError  bool
	panicMode bool
}

type Local struct {
	name       Token
	depth      int
	isCaptured bool
}

type Compiler struct {
	enclosing    *Compiler
	function     *Function
	functionType FunctionType

	locals     [256]Local
	localCount int
	upvalues   [256]struct {
		index   uint8
		isLocal bool
	}
	scopeDepth int
}

type ClassCompiler struct {
	enclosing     *ClassCompiler
	hasSuperclass bool
}

var rules []ParseRule
var scanner *Scanner
var parser = Parser{hadError: false, panicMode: false}
var current *Compiler
var currentClass *ClassCompiler

func currentChunk() *Chunk {
	return &current.function.chunk
}

func getRule(tokeType TokenType) ParseRule {
	return rules[tokeType]
}

func initRules() {
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
		TokenAnd:          {nil, and, PrecAnd},
		TokenClass:        {nil, nil, PrecNone},
		TokenElse:         {nil, nil, PrecNone},
		TokenFalse:        {literal, nil, PrecNone},
		TokenFor:          {nil, nil, PrecNone},
		TokenFun:          {nil, nil, PrecNone},
		TokenIf:           {nil, nil, PrecNone},
		TokenNil:          {literal, nil, PrecNone},
		TokenOr:           {nil, or, PrecOr},
		TokenPrint:        {nil, nil, PrecNone},
		TokenReturn:       {nil, nil, PrecNone},
		TokenSuper:        {super, nil, PrecNone},
		TokenThis:         {this, nil, PrecNone},
		TokenTrue:         {literal, nil, PrecNone},
		TokenVar:          {nil, nil, PrecNone},
		TokenWhile:        {nil, nil, PrecNone},
		TokenError:        {nil, nil, PrecNone},
		TokenEof:          {nil, nil, PrecNone},
	}
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

func compile(source string) *Function {
	initRules()

	scanner = initScanner(source)
	var compiler Compiler
	initCompiler(&compiler, TypeScript)

	advance()

	for !match(TokenEof) {
		declaration()
	}

	function := endCompiler()
	if parser.hadError {
		return nil
	} else {
		return function
	}
}

func endCompiler() *Function {
	emitReturn()
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

func parsePrecedence(precedence Precedence) {
	advance()
	prefixRule := getRule(parser.previous.tokenType).prefix
	if prefixRule == nil {
		_error("Expect expression.")
		return
	}

	canAssign := precedence <= PrecAssignment
	prefixRule(canAssign)

	for precedence <= getRule(parser.current.tokenType).precedence {
		advance()
		infixRule := getRule(parser.previous.tokenType).infix
		infixRule(canAssign)
	}

	if canAssign && match(TokenEqual) {
		_error("Invalid assignment target.")
	}
}

func advance() {
	parser.previous = parser.current

	for {
		parser.current = scanner.scanToken()
		if parser.current.tokenType != TokenError {
			break
		}
		errorAtCurrent(parser.current.lexeme)
	}
}

func consume(tokenType TokenType, message string) {
	if parser.current.tokenType == tokenType {
		advance()
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
	advance()
	return true
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

func classDeclaration() {
	consume(TokenIdentifier, "Expect class name.")
	className := parser.previous
	nameConstant := identifierConstant(parser.previous)
	declareVariable()

	emitBytes(OpClass, nameConstant)
	defineVariable(nameConstant)

	classCompiler := ClassCompiler{}
	classCompiler.hasSuperclass = false
	classCompiler.enclosing = currentClass
	currentClass = &classCompiler

	if match(TokenLess) {
		consume(TokenIdentifier, "Expect superclass name.")
		variable(false)

		if identifiersEqual(className, parser.previous) {
			_error("A class can't inherit from itself.")
		}

		beginScope()
		addLocal(syntheticToken("super"))
		defineVariable(0)

		namedVariable(className, false)
		emitByte(OpInherit)
		classCompiler.hasSuperclass = true
	}

	namedVariable(className, false)
	consume(TokenLeftBrace, "Expect '{' before class body.")
	for !check(TokenRightBrace) && !check(TokenEof) {
		method()
	}
	consume(TokenRightBrace, "Expect '}' after class body.")
	emitByte(OpPop)

	if classCompiler.hasSuperclass {
		endScope()
	}

	currentClass = currentClass.enclosing
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
		emitByte(OpNil)
	}
	consume(TokenSemicolon, "Expect ';' after variable declaration.")

	defineVariable(global)
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

func expression() {
	parsePrecedence(PrecAssignment)
}

func method() {
	consume(TokenIdentifier, "Expect method name.")
	constant := identifierConstant(parser.previous)

	functionType := TypeMethod
	if parser.previous.lexeme == "init" {
		functionType = TypeInitializer
	}
	function(functionType)
	emitBytes(OpMethod, constant)
}

func function(functionType FunctionType) {
	var compiler Compiler
	initCompiler(&compiler, functionType)
	beginScope()

	consume(TokenLeftParen, "Expect '(' after function name.")
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
	consume(TokenRightParen, "Expect ')' after parameters.")
	consume(TokenLeftBrace, "Expect '{' before function body.")
	block()

	function := endCompiler()
	emitBytes(OpClosure, makeConstant(functionVal(function)))

	for i := 0; i < function.upvalueCount; i++ {
		if compiler.upvalues[i].isLocal {
			emitByte(1)
		} else {
			emitByte(0)
		}
		emitByte(compiler.upvalues[i].index)
	}
}

func printStatement() {
	expression()
	consume(TokenSemicolon, "Expect ';' after value.")
	emitByte(OpPrint)
}

func forStatement() {
	beginScope()
	consume(TokenLeftParen, "Expect '(' after 'for'.")
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
		consume(TokenSemicolon, "Expect ';' after loop condition.")

		// Jump out of the loop if the condition is false.
		exitJump = emitJump(OpJumpIfFalse)
		emitByte(OpPop) // Condition.
	}

	if !match(TokenRightParen) {
		bodyJump := emitJump(OpJump)
		incrementStart := len(currentChunk().code)
		expression()
		emitByte(OpPop)
		consume(TokenRightParen, "Expect ')' after for clauses.")

		emitLoop(loopStart)
		loopStart = incrementStart
		patchJump(bodyJump)
	}

	statement()
	emitLoop(loopStart)

	if exitJump != -1 {
		patchJump(exitJump)
		emitByte(OpPop)
	}

	endScope()
}

func ifStatement() {
	consume(TokenLeftParen, "Expect '(' after if.")
	expression()
	consume(TokenRightParen, "Expect ')' after condition.")

	thenJump := emitJump(OpJumpIfFalse)
	emitByte(OpPop)
	statement()

	elseJump := emitJump(OpJump)

	patchJump(thenJump)
	emitByte(OpPop)

	if match(TokenElse) {
		statement()
	}
	patchJump(elseJump)
}

func returnStatement() {
	if current.functionType == TypeScript {
		_error("Can't return from top-level code.")
	}

	if match(TokenSemicolon) {
		emitReturn()
	} else {
		if current.functionType == TypeInitializer {
			_error("Can't return a value from an initializer.")
		}

		expression()
		consume(TokenSemicolon, "Expect ';' after return value.")
		emitByte(OpReturn)
	}
}

func whileStatement() {
	loopStart := len(currentChunk().code)
	consume(TokenLeftParen, "Expect '(' after 'while'.")
	expression()
	consume(TokenRightParen, "Expect ')' after condition.")

	exitJump := emitJump(OpJumpIfFalse)
	emitByte(OpPop)
	statement()
	emitLoop(loopStart)

	patchJump(exitJump)
	emitByte(OpPop)
}

func expressionStatement() {
	expression()
	consume(TokenSemicolon, "Expect ';' after expression.")
	emitByte(OpPop)
}

func beginScope() {
	current.scopeDepth++
}

func endScope() {
	current.scopeDepth--

	for current.localCount > 0 && current.locals[current.localCount-1].depth > current.scopeDepth {
		if current.locals[current.localCount-1].isCaptured {
			emitByte(OpCloseUpvalue)
		} else {
			emitByte(OpPop)
		}
		current.localCount--
	}
}

func block() {
	for !check(TokenRightBrace) && !check(TokenEof) {
		declaration()
	}

	consume(TokenRightBrace, "Expect '}' after block.")
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

func identifierConstant(name Token) uint8 {
	return makeConstant(stringVal(name.lexeme))
}

func identifiersEqual(a Token, b Token) bool {
	return a.lexeme == b.lexeme
}

func parseVariable(errorMessage string) uint8 {
	consume(TokenIdentifier, errorMessage)

	declareVariable()
	if current.scopeDepth > 0 {
		return 0
	}

	return identifierConstant(parser.previous)
}

func defineVariable(global uint8) {
	if current.scopeDepth > 0 {
		markInitialized()
		return
	}
	emitBytes(OpDefineGlobal, global)
}

func markInitialized() {
	if current.scopeDepth == 0 {
		return
	}
	current.locals[current.localCount-1].depth = current.scopeDepth
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
	consume(TokenRightParen, "Expect ')' after arguments.")
	return argCount
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
		emitBytes(setOp, uint8(arg))
	} else {
		emitBytes(getOp, uint8(arg))
	}
}

func syntheticToken(text string) Token {
	token := Token{}
	token.lexeme = text
	return token
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
		advance()
	}
}

// Parse rule functions
func and(bool) {
	endJump := emitJump(OpJumpIfFalse)

	emitByte(OpPop)
	parsePrecedence(PrecAnd)

	patchJump(endJump)
}

func grouping(bool) {
	expression()
	consume(TokenRightParen, "Expect ')' after expression.")
}

func unary(bool) {
	operatorType := parser.previous.tokenType

	// Compile the operand.
	parsePrecedence(PrecUnary)

	// Emit the operator instruction.
	switch operatorType {
	case TokenBang:
		emitByte(OpNot)
		break
	case TokenMinus:
		emitByte(OpNegate)
		break
	default:
		return // Unreachable.
	}
}

func binary(bool) {
	operatorType := parser.previous.tokenType
	rule := getRule(operatorType)
	parsePrecedence(rule.precedence + 1)

	switch operatorType {
	case TokenBangEqual:
		emitBytes(OpEqual, OpNot)
		break
	case TokenEqualEqual:
		emitByte(OpEqual)
		break
	case TokenGreater:
		emitByte(OpGreater)
		break
	case TokenLess:
		emitByte(OpLess)
		break
	case TokenLessEqual:
		emitBytes(OpGreater, OpNot)
		break
	case TokenGreaterEqual:
		emitBytes(OpLess, OpNot)
		break
	case TokenPlus:
		emitByte(OpAdd)
		break
	case TokenMinus:
		emitByte(OpSubtract)
		break
	case TokenStar:
		emitByte(OpMultiply)
		break
	case TokenSlash:
		emitByte(OpDivide)
		break
	default:
		return // Unreachable.
	}
}

func call(bool) {
	argCount := argumentList()
	emitBytes(OpCall, argCount)
}

func dot(canAssign bool) {
	consume(TokenIdentifier, "Expect property name after '.'.")
	name := identifierConstant(parser.previous)

	if canAssign && match(TokenEqual) {
		expression()
		emitBytes(OpSetProperty, name)
	} else if match(TokenLeftParen) {
		argCount := argumentList()
		emitBytes(OpInvoke, name)
		emitByte(argCount)
	} else {
		emitBytes(OpGetProperty, name)
	}
}

func literal(bool) {
	switch parser.previous.tokenType {
	case TokenFalse:
		emitByte(OpFalse)
		break
	case TokenNil:
		emitByte(OpNil)
		break
	case TokenTrue:
		emitByte(OpTrue)
		break
	default:
		return // Unreachable.
	}
}

func number(bool) {
	value, _ := strconv.ParseFloat(parser.previous.lexeme, 64)
	emitConstant(numberVal(value))
}

func or(bool) {
	elseJump := emitJump(OpJumpIfFalse)
	endJump := emitJump(OpJump)

	patchJump(elseJump)
	emitByte(OpPop)

	parsePrecedence(PrecOr)
	patchJump(endJump)
}

func _string(bool) {
	stringValue := Value{valueType: ValString, as: union{string: parser.previous.lexeme}}
	emitConstant(stringValue)
}

func variable(canAssign bool) {
	namedVariable(parser.previous, canAssign)
}

func super(bool) {
	if currentClass == nil {
		_error("Can't use 'super' outside of a class.")
	} else if !currentClass.hasSuperclass {
		_error("Can't use 'super' in a class with no superclass.")
	}

	consume(TokenDot, "Expect '.' after 'super'.")
	consume(TokenIdentifier, "Expect superclass method name.")
	name := identifierConstant(parser.previous)

	namedVariable(syntheticToken("this"), false)
	if match(TokenLeftParen) {
		argCount := argumentList()
		namedVariable(syntheticToken("super"), false)
		emitBytes(OpSuperInvoke, name)
		emitByte(argCount)
	} else {
		namedVariable(syntheticToken("super"), false)
		emitBytes(OpGetSuper, name)
	}
}

func this(bool) {
	if currentClass == nil {
		_error("Can't use 'this' outside of a class.")
		return
	}

	variable(false)
}

// Emit bytecodes for VM.
func makeConstant(value Value) uint8 {
	constant := currentChunk().addConstant(value)
	if constant > 255 {
		_error("Too many constants in one chunk.")
		return 0
	}
	return constant
}

func patchJump(offset int) {
	// -2 to adjust for the bytecode for the jump offset itself.
	jump := len(currentChunk().code) - offset - 2

	if jump > 65535 {
		_error("Too much code to jump over.")
	}

	currentChunk().code[offset] = uint8((jump >> 8) & 0xff)
	currentChunk().code[offset+1] = uint8(jump & 0xff)
}

func emitLoop(loopStart int) {
	emitByte(OpLoop)

	offset := len(currentChunk().code) - loopStart + 2
	if offset > 65535 {
		_error("Loop body too large.")
	}

	emitByte(byte((offset >> 8) & 0xff))
	emitByte(byte(offset & 0xff))
}

func emitConstant(value Value) {
	emitBytes(OpConstant, makeConstant(value))
}

func emitReturn() {
	if current.functionType == TypeInitializer {
		emitBytes(OpGetLocal, 0)
	} else {
		emitByte(OpNil)
	}

	emitByte(OpReturn)
}

func emitJump(instruction uint8) int {
	emitByte(instruction)
	emitByte(0xff)
	emitByte(0xff)
	return len(currentChunk().code) - 2
}

func emitBytes(byte1 uint8, byte2 uint8) {
	emitByte(byte1)
	emitByte(byte2)
}

func emitByte(byte uint8) {
	currentChunk().write(byte, parser.previous.line)
}

// Report errors.
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
