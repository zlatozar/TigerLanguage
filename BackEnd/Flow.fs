module Flow

open GraphRep

type FlowGraph =
    {
      // DIRECTED graph representing control flow from instruction to instruction
      control: Graph;

      // sets of destination registers of the instruction (assign)
      def: Temp.Temp list Graph.Table;

      // sets of source registers of the instruction (rhs)
      uses: Temp.Temp list Graph.Table;

      // is MOVE, then delete if 'use' and 'def' are indentical
      isMove: bool Graph.Table
    }

// Notes:
// Variable could be on both sides so: def(3) = {c}; use(3) = {b, c}

// If there are any nonzero number of "def"s, mention def(x).
// If there are any nonzero number of "use"s BEFORE THE FIRST "def", mention use(x).
