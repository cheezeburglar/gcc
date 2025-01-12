/* Serialization of some data structures to JSON. */

#include "typed-splay-tree.h"


// We don't currently really use the KEY-VALUE feature of
// These splay tree nodes - is that a problem?
// This is used by a function as we serialize
// to keep track of nodes that need to be visited.
// Altering how queue is accessed should affect order that the nodes are dumped in
template<typename T>
class visitor
{
  visitor ();
  ~visitor ();

public:
  FILE *stream;
  const T node;
  dump_flags_t flags;
  visitor_queue_p<T> queue;
  visitor_queue_p<T> queue_end;
  visitor_queue_p<T> free_list;
  typed_splay_tree<(void *), T> nodes;

  void dequeue_and_add(visitor di);
  void queue(visitor di);
  void * execute_fn_and_queue (void (void *) fn (), data);
  json::object emit_json_and_queue (V); // TODO: replace with function pointer?
};

template<typename T>
struct visitor_queue 
{
  splay_tree_node<T> node;
  visitor_queue *next;
};

struct generic_tree_visitor : public visitor<tree>;
{

}

struct gimple_visitor : public visitor<gimple>;
{

}

struct rtl_visitor : public visitor<rtl>;
{

}

class tree_json_writer
{
public:
  tree_json_writer ();
  ~tree_json_writer ();
  void write (FILE * file);
  void add_node (tree t);
  void add_fndecl_tree (const_tree fndecl, dump_flags_t flags);
  void set_stream (tree_dump_index tdi);

private:
  std::unique_ptr<json::array> m_json_root_tuple;
  dump_file_info m_dfi;
  FILE *m_stream;
};
