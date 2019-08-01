module Flow

open GraphRep

type FlowGraph =
    {
      // current (up to node) graph
      control: Graph;

      // Folloing contains the information that we need at the current Node (moment)

      // sets of destination registers of the instruction (assign)
      def: Graph.Table<Temp.Temp list>;

      // sets of source registers of the instruction (rhs)
      uses: Graph.Table<Temp.Temp list>;

      // is MOVE, then delete if 'use' and 'def' are indentical
      isMove: Graph.Table<bool>
    }

// Notes:
// Variable could be on both sides so: def(3) = {c}; use(3) = {b, c}

// If there are any nonzero number of "def"s, mention def(x).
// If there are any nonzero number of "use"s BEFORE THE FIRST "def", mention use(x).
