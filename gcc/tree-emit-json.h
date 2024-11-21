/* Tree-dumping functionality for intermediate representation.
   Copyright (C) 1999-2024 Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_TREE_JSON_H
#define GCC_TREE_JSON_H

#include "splay-tree.h"
#include "dumpfile.h"
#define INCLUDE_MEMORY
#include "json.h"

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

struct dump_info
{
  /* The stream on which to dump the information.  */
  FILE *stream;
  /* The original node.  */
  const_tree node;
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
  /* JSON tree holder. Carries everything, each node is a sub-array */
  std::unique_ptr<json::array> json_dump;
};

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

extern std::unique_ptr<json::object> node_emit_json(tree t, dump_info_p di);
extern json::array * generic_to_json (tree t, dump_flags_t flag);
extern void dump_node_json (const_tree t, dump_flags_t flags, FILE *stream);
extern void set_xloc_as (json::object & json_obj, expanded_location xloc, const char *label);
#endif /* ! GCC_TREE_JSON_H */
