module Flow

open GraphRep

type FlowGraph =
  { control: Graph;
    def: Temp.Temp list Graph.Table;
    uses: Temp.Temp list Graph.Table;
    isMove: bool Graph.Table }
