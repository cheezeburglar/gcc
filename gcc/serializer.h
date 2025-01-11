/* Serialization of some data structures to JSON. */

#include "typed-splay-tree.h"

template<typename T>
class visitor
{
public:
  FILE *stream;
  const T node;
  dump_flags_t flags;
  visitor_queue_p dump_queue;
  visitor_queue_p queue_end;
  visitor_queue_p free_list;
  typed_splay_tree<void*,  T> nodes;

  void dequeue_and_add(T);
  void queue(T);
};

struct visitor_queue 
{
  splay_tree_node<T> node;
  visitor_queue *next;
};

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
