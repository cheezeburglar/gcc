/*  Serialization of some data structures to JSON.
 *
 *  Currently writes stuff out as:
 *   {"key" : "value"} \n
 *   { }...
 *
 *  For different objects.*/

#include "tree-emit-json.h"
#include "gimple-emit-json.h"
#include "rtl-emit-json.h"

template<typename T>
static void
dequeue_and_add (visitor *di)
{
  dump_queue_p dq;
  splay_tree_node stn;
  T t;

  /* Get the next node from the queue.  */
  dq = di->queue;
  stn = dq->node;
  t = (tree) stn->key;

  /* Remove the node from the queue, and put it on the free list.  */
  di->queue = dq->next;
  if (!di->queue)
    di->queue_end = 0;
  dq->next = di->free_list;
  di->free_list = dq;

  /* Convert the node to JSON and store it to be dumped later. */
  auto dummy = node_emit_json(t, di).release();
  di->json_dump->append(dummy);
}

template<typename T>
static void
queue (visitor *di, T node)
{
  visitor_queue *dq;

  /* Obtain a new queue node.  */
  if (di->free_list)
    {
      dq = di->free_list;
      di->free_list = dq->next;
    }
  else
    dq = XNEW (struct visitor_queue);

  /* Create a new entry in the splay-tree and insert into queue iff new.
   * Else, end.*/
  if (!splay_tree_lookup (di->nodes, (splay_tree_key) node))
  {
    dq->node = splay_tree_insert (di->nodes, (splay_tree_key) t,
          			(splay_tree_value) dni);
    dq->next = 0;
    if (!di->queue_end)
      di->queue = dq;
    else
      di->queue_end->next = dq;
    di->queue_end = dq;
  }
}

template <typename T> void
execute_per_node (T node, void func)
{

}

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
