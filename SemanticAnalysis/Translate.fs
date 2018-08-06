module Translate

// Intermediate language. Dummy for now.
type Exp = unit

// As marker that 'for' and 'while' should pass. Dummy for now.
type BreakPoint = string option
let newBreakpoint = Some "_break"

