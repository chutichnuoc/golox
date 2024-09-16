package interpreter

import "fmt"

func compile(source string) {
	scanner := initScanner(source)
	line := -1
	for {
		token := scanner.scanToken()
		if token.line != line {
			fmt.Printf("%4d", token.line)
			line = token.line
		} else {
			fmt.Print("   | ")
		}
		fmt.Printf(" %2d '%s'\n", token.tokenType, token.lexeme)

		if token.tokenType == TokenEof {
			break
		}
	}
}
