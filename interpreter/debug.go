package interpreter

import "fmt"

const DebugPrintCode = true
const DebugTraceExecution = true

func DisassembleChunk(chunk *Chunk, name string) {
	fmt.Printf("== %s ==\n", name)
	for offset := 0; offset < len(chunk.code); {
		offset = disassembleInstruction(chunk, offset)
	}
}

func disassembleInstruction(chunk *Chunk, offset int) int {
	fmt.Printf("%04d ", offset)
	if offset > 0 && chunk.lines[offset] == chunk.lines[offset-1] {
		fmt.Print("   | ")
	} else {
		fmt.Printf("%4d ", chunk.lines[offset])
	}
	instruction := chunk.code[offset]
	switch instruction {
	case OpConstant:
		return constantInstruction("OpConstant", chunk, offset)
	case OpNil:
		return simpleInstruction("OpNil", offset)
	case OpTrue:
		return simpleInstruction("OpTrue", offset)
	case OpFalse:
		return simpleInstruction("OpFalse", offset)
	case OpEqual:
		return simpleInstruction("OpEqual", offset)
	case OpGreater:
		return simpleInstruction("OpGreater", offset)
	case OpLess:
		return simpleInstruction("OpLess", offset)
	case OpAdd:
		return simpleInstruction("OpAdd", offset)
	case OpSubtract:
		return simpleInstruction("OpSubtract", offset)
	case OpMultiply:
		return simpleInstruction("OpMultiply", offset)
	case OpDivide:
		return simpleInstruction("OpDivide", offset)
	case OpNot:
		return simpleInstruction("OpNot", offset)
	case OpNegate:
		return simpleInstruction("OpNegate", offset)
	case OpReturn:
		return simpleInstruction("OpReturn", offset)
	default:
		fmt.Printf("Unknown opcode %d\n", instruction)
		return offset + 1
	}
}

func constantInstruction(name string, chunk *Chunk, offset int) int {
	constant := chunk.code[offset+1]
	fmt.Printf("%-16s %4d '", name, constant)
	printValue(chunk.constants.values[constant])
	fmt.Println()
	return offset + 2
}

func simpleInstruction(name string, offset int) int {
	fmt.Printf("%s\n", name)
	return offset + 1
}
