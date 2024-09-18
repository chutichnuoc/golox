package interpreter

type Precedence int

const (
	PrecNone       = iota
	PrecAssignment // =
	PrecOr         // or
	PrecAnd        // and
	PrecEquality   // == !=
	PrecComparison // < > <= >=
	PrecTerm       // + -
	PrecFactor     // * /
	PrecUnary      // ! -
	PrecCall       // . ()
	PrecPrimary
)
