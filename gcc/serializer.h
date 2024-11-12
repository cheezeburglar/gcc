/* Serialization of some data structures to JSON. */

class tree_json_writer
{
public:
  tree_json_writer ();
  ~tree_json_writer ();
  void write (FILE * file);
  void add_node (tree t);
  void add_fndecl_tree (tree fndecl, dump_flags_t flags);
  void set_stream (tree_dump_index tdi);

private:
  std::unique_ptr<json::array> m_json_root_tuple;
  dump_file_info m_dfi;
  FILE *m_stream;
};
