package interpreter

type Scanner struct {
	source  string
	start   int
	current int
	line    int
}

type Token struct {
	tokenType TokenType
	lexeme    string
	line      int
}

func initScanner(source string) *Scanner {
	scanner := &Scanner{}
	scanner.source = source
	scanner.start = 0
	scanner.current = 0
	scanner.line = 1
	return scanner
}

func isAlpha(c uint8) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isDigit(c uint8) bool {
	return c >= '0' && c <= '9'
}

func (scanner *Scanner) scanToken() Token {
	scanner.skipWhiteSpace()
	scanner.start = scanner.current
	if scanner.isAtEnd() {
		return scanner.makeToken(TokenEof)
	}

	c := scanner.advance()
	if isAlpha(c) {
		return scanner.identifier()
	}
	if isDigit(c) {
		return scanner.number()
	}

	switch c {
	case '(':
		return scanner.makeToken(TokenLeftParen)
	case ')':
		return scanner.makeToken(TokenRightParen)
	case '{':
		return scanner.makeToken(TokenLeftBrace)
	case '}':
		return scanner.makeToken(TokenRightBrace)
	case ';':
		return scanner.makeToken(TokenSemicolon)
	case ',':
		return scanner.makeToken(TokenComma)
	case '.':
		return scanner.makeToken(TokenDot)
	case '-':
		return scanner.makeToken(TokenMinus)
	case '+':
		return scanner.makeToken(TokenPlus)
	case '/':
		return scanner.makeToken(TokenSlash)
	case '*':
		return scanner.makeToken(TokenStar)
	case '!':
		if scanner.match('=') {
			return scanner.makeToken(TokenBangEqual)
		} else {
			return scanner.makeToken(TokenBang)
		}
	case '=':
		if scanner.match('=') {
			return scanner.makeToken(TokenEqualEqual)
		} else {
			return scanner.makeToken(TokenEqual)
		}
	case '<':
		if scanner.match('=') {
			return scanner.makeToken(TokenLessEqual)
		} else {
			return scanner.makeToken(TokenLess)
		}
	case '>':
		if scanner.match('=') {
			return scanner.makeToken(TokenGreaterEqual)
		} else {
			return scanner.makeToken(TokenGreater)
		}
	case '"':
		return scanner.string()
	}

	return scanner.errorToken("Unexpected character.")
}

func (scanner *Scanner) isAtEnd() bool {
	return scanner.current >= len(scanner.source)
}

func (scanner *Scanner) advance() uint8 {
	c := scanner.source[scanner.current]
	scanner.current++
	return c
}

func (scanner *Scanner) peek() uint8 {
	if scanner.isAtEnd() {
		return '\000'
	}
	return scanner.source[scanner.current]
}

func (scanner *Scanner) peekNext() uint8 {
	if scanner.current+1 >= len(scanner.source) {
		return '\000'
	}
	return scanner.source[scanner.current+1]
}

func (scanner *Scanner) match(expected uint8) bool {
	if scanner.isAtEnd() {
		return false
	}
	if scanner.source[scanner.current] != expected {
		return false
	}
	scanner.current++
	return true
}

func (scanner *Scanner) makeToken(tokenType TokenType) Token {
	token := Token{}
	token.tokenType = tokenType
	token.lexeme = scanner.source[scanner.start:scanner.current]
	token.line = scanner.line
	return token
}

func (scanner *Scanner) errorToken(message string) Token {
	token := Token{}
	token.tokenType = TokenError
	token.lexeme = message
	token.line = scanner.line
	return token
}

func (scanner *Scanner) skipWhiteSpace() {
	for {
		c := scanner.peek()
		switch c {
		case ' ':
			scanner.advance()
			break
		case '\r':
			scanner.advance()
			break
		case '\t':
			scanner.advance()
			break
		case '\n':
			scanner.line++
			scanner.advance()
			break
		case '/':
			if scanner.peekNext() == '/' {
				// A comment goes until the end of the line.
				for scanner.peek() != '\n' && !scanner.isAtEnd() {
					scanner.advance()
				}
			} else {
				return
			}
			break
		default:
			return
		}
	}
}

func (scanner *Scanner) checkKeyword(start int, length int, rest string, tokenType TokenType) TokenType {
	if scanner.current-scanner.start == start+length && scanner.source[scanner.start+start:scanner.current] == rest {
		return tokenType
	}
	return TokenIdentifier
}

func (scanner *Scanner) identifierType() TokenType {
	switch scanner.source[scanner.start] {
	case 'a':
		return scanner.checkKeyword(1, 2, "nd", TokenAnd)
	case 'c':
		return scanner.checkKeyword(1, 4, "lass", TokenClass)
	case 'e':
		return scanner.checkKeyword(1, 3, "lse", TokenElse)
	case 'f':
		if scanner.current-scanner.start > 1 {
			switch scanner.source[scanner.start] {
			case 'a':
				return scanner.checkKeyword(2, 3, "lse", TokenFalse)
			case 'o':
				return scanner.checkKeyword(2, 1, "r", TokenFor)
			case 'u':
				return scanner.checkKeyword(2, 1, "n", TokenFun)
			}
		}
	case 'i':
		return scanner.checkKeyword(1, 1, "f", TokenIf)
	case 'n':
		return scanner.checkKeyword(1, 2, "il", TokenNil)
	case 'o':
		return scanner.checkKeyword(1, 1, "r", TokenOr)
	case 'p':
		return scanner.checkKeyword(1, 4, "rint", TokenPrint)
	case 'r':
		return scanner.checkKeyword(1, 5, "eturn", TokenReturn)
	case 's':
		return scanner.checkKeyword(1, 4, "uper", TokenSuper)
	case 't':
		if scanner.current-scanner.start > 1 {
			switch scanner.source[scanner.start] {
			case 'h':
				return scanner.checkKeyword(2, 2, "is", TokenThis)
			case 'r':
				return scanner.checkKeyword(2, 2, "ue", TokenTrue)
			}
		}
	case 'v':
		return scanner.checkKeyword(1, 2, "ar", TokenVar)
	case 'w':
		return scanner.checkKeyword(1, 4, "hile", TokenWhile)
	}
	return TokenIdentifier
}

func (scanner *Scanner) identifier() Token {
	for isAlpha(scanner.peek()) || isDigit(scanner.peek()) {
		scanner.advance()
	}
	return scanner.makeToken(scanner.identifierType())
}

func (scanner *Scanner) number() Token {
	for isDigit(scanner.peek()) {
		scanner.advance()
	}

	// Look for a fractional part.
	if scanner.peek() == '.' && isDigit(scanner.peekNext()) {
		// Consume the ".".
		scanner.advance()
		for isDigit(scanner.peek()) {
			scanner.advance()
		}
	}

	return scanner.makeToken(TokenNumber)
}

func (scanner *Scanner) string() Token {
	for scanner.peek() != '"' && !scanner.isAtEnd() {
		if scanner.peek() == '\n' {
			scanner.line++
		}
		scanner.advance()
	}

	if scanner.isAtEnd() {
		return scanner.errorToken("Unterminated string.")
	}

	// The closing quote.
	scanner.advance()
	return scanner.makeToken(TokenString)
}
