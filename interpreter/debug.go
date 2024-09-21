package interpreter

import "fmt"

const DebugPrintCode = false
const DebugTraceExecution = false

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
	case OpPop:
		return simpleInstruction("OpPop", offset)
	case OpGetLocal:
		return byteInstruction("OpGetLocal", chunk, offset)
	case OpSetLocal:
		return byteInstruction("OpSetLocal", chunk, offset)
	case OpGetGlobal:
		return constantInstruction("OpGetGlobal", chunk, offset)
	case OpDefineGlobal:
		return constantInstruction("OpDefineGlobal", chunk, offset)
	case OpSetGlobal:
		return constantInstruction("OpSetGlobal", chunk, offset)
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
	case OpPrint:
		return simpleInstruction("OpPrint", offset)
	case OpJump:
		return jumpInstruction("OpJump", 1, chunk, offset)
	case OpJumpIfFalse:
		return jumpInstruction("OpJumpIfFalse", 1, chunk, offset)
	case OpLoop:
		return jumpInstruction("OpLoop", -1, chunk, offset)
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

func byteInstruction(name string, chunk *Chunk, offset int) int {
	slot := chunk.code[offset+1]
	fmt.Printf("%-16s %4d\n", name, slot)
	return offset + 2
}

func jumpInstruction(name string, sign int, chunk *Chunk, offset int) int {
	jump := uint16(chunk.code[offset+1] << 8)
	jump |= uint16(chunk.code[offset+2])
	fmt.Printf("%-16s %4d -> %d\n", name, offset, offset+3+sign*int(jump))
	return offset + 3
}
