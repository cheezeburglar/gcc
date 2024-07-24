In general the idea is that the dump flag -fdump-tree-original-json emits something of the form

{"index": 1, "tree_code": foo, "child_1": {...}, ... "child_n": {...}}

or

{"index": 1, "tree_code": foo, "children": [{...}, {...}]}

where index denotes not any part of the tree class but the order in which the nodes are traversed
and the "child_n" or "children" contain the index, tree_codes, and other data in the child nodes.

In the above example, I don't think it's worth recursing over each child nodes' child. And, if we
look at say COMPLEX_TYPE, we'd want each child node to contain at least some of the data if not 
all (save the child node's children). 

One option to translating this to HTML is making each index effectively a hyperlink to i's node,
so one could traverse the tree by clicking on each index. We could also feed in a recursion depth parameter
here, so an individual HTML document would display one node, it's children, and it's children's children
up to some depth (possibly 0). I haven't yet gotten the JSON emitted in either of the above forms,
so I haven't looked at the technical details of this too closely yet. 

//Look at debug_tree

//Show some examples to community, feedback for HTML once prototypical examples

// Also track addresses of tree_nodes, use for unique identifier instead of visitation order?
// Make sure that we iterate over data fields in parent nest; in particular make sure all those that are in RAW and maybe all

// Static_flag, e.g., has different semantics in different positions - all in tree-core.h. Worry about overloaded meaning. see debug_tree for inspiration
// Keep track of different semantics in differing positions. dump_generic_node() might not do this

All of this is essentially just a debugging nicety, so I think it'd be acceptable to write this
translator in Python. One would also, probably, call into this utility from GDB. I have not yet
looked deeply into GDB's python API yet, but I *think* having this script in Python would be
ever-so-more convenient. One would ultimately call some function from GDB that takes in
some tree and displays all of the above in some web browser.
