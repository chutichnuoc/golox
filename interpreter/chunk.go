package interpreter

type Chunk struct {
	code      []uint8
	lines     []int
	constants *ValueArray
}

func newChunk() *Chunk {
	chunk := &Chunk{}
	chunk.code = make([]uint8, 0)
	chunk.lines = make([]int, 0)
	chunk.constants = newValueArray()
	return chunk
}

func (chunk *Chunk) free() {
	chunk.code = make([]uint8, 0)
	chunk.lines = make([]int, 0)
	chunk.constants.free()
}

func (chunk *Chunk) write(byte uint8, line int) {
	chunk.code = append(chunk.code, byte)
	chunk.lines = append(chunk.lines, line)
}

func (chunk *Chunk) addConstant(value Value) uint8 {
	chunk.constants.write(value)
	return uint8(len(chunk.constants.values) - 1)
}
