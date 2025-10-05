// data/schema.ts (for your reference)
export type Node = {
    id: string;                 // slug, e.g., "bayes-theorem"
    title: string;              // "Bayes' Theorem"
    kind: "concept"|"skill"|"example"|"theorem";
    summary: string;
    prerequisites: string[];    // ids
    outcomes: string[];         // ids of what this unlocks
    resources: {label:string,url?:string, localRef?:string}[];
  };
  
  export type Edge = {
    source: string;             // node id
    target: string;             // node id
    type: "prereq"|"refines"|"applies-to"|"related";
    weight?: number;
  };
  
  export type Graph = { nodes: Node[]; edges: Edge[]; }
  