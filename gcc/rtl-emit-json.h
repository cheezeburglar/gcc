typedef struct dump_info *dump_info_p;

/* Information about a node to be dumped.  */

typedef struct dump_node_info
{} *dump_node_info_p;

/* A dump_queue is a link in the queue of things to be dumped.  */

typedef struct dump_queue
{
  /* The queued tree node.  */
  splay_tree_node node;
  /* The next node in the queue.  */
  struct dump_queue *next;
} *dump_queue_p;

/* A dump_info gives information about how we should perform the dump
   and about the current state of the dump.  */

typedef struct dump_info_rtl
{
  /* The stream on which to dump the information.  */
  FILE *stream;
  /* The original node.  */
  const_rtx node;
  /* User flags.  */
  dump_flags_t flags;
  /* The next unused node index.  */
  dump_queue_p queue;
  /* The last node in the queue.  */
  dump_queue_p queue_end;
  /* Free queue nodes.  */
  dump_queue_p free_list;
  /* The tree nodes which we have already written out.  The
     keys are the addresses of the nodes; the values are the integer
     indices we assigned them.  */
  splay_tree nodes;
  /* json tree holder. carries everything, each node is a sub-array */
  std::unique_ptr<json::array> json_dump;
} *dump_info_rtl_p;

