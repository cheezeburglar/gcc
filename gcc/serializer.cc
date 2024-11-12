/*  Serialization of some data structures to JSON.
 *
 *  Currently writes stuff out as:
 *   {"key" : "value"} \n
 *   { }...
 *
 *  For different objects.*/

#include "tree-emit-json.h"

tree_json_writer::tree_json_writer ()
  : m_json_root_tuple ()
{

}

tree_json_writer::~tree_json_writer ()
{

}

void
tree_json_writer::set_stream (tree_dump_index tdi)
{
  if (m_stream)
    delete m_stream;
  m_stream = g->get_dumps()->get_dump_file_info(tdi)->pstream;
}

void
tree_json_writer::add_fndecl_tree (tree fndecl, dump_flags_t flags)
{
  auto json_obj = new json::object ();

  json_obj->set(lang_hooks.decl_printable_name (fndecl, 2),
                tree_to_json(DECL_SAVED_TREE(fndecl), flags));
  m_json_root_tuple->append(json_obj);
  if (!m_stream)
    set_stream(TDI_original);
}

void
tree_json_writer::write (FILE * file)
{
//  pretty_printer pp;
//  m_json_root_tuple->print (&pp, true);
//
//  bool emitted_error = false;
//  char *filename = concat (dump_base_name, ".tree.json.gz", NULL);
//  gzFile outfile = gzopen (filename, "w");
//  if (outfile == NULL)
//    {
//      goto cleanup;
//    }
//
//  if (gzputs (outfile, pp_formatted_text (&pp)) <= 0)
//    {
//      int tmp;
//      emitted_error = true;
//    }
//
// cleanup:
//  if (outfile)
//    if (gzclose (outfile) != Z_OK)
//      if (!emitted_error)
//        {}
//
//  free (filename);
  m_json_root_tuple->dump(file, true);
}
