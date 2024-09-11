/* Dumping functionality for GENERIC Trees as JSON. Both for 
   the dump -fdump-tree-original-json and a new debug 
   function.
   Copyright (C) 2024 Thor Preimesberger <tcpreimesberger@gmail.com>
   Adapted from tree-pretty-print.cc, tree-dump.cc, and 
   print-tree.cc.

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

#define INCLUDE_STRING
#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pretty-print.h"
#include "tree-emit-json.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "dumpfile.h"
#include "json.h"
#include "tm.h"
#include "wide-int-print.h"
#include "real.h" //for real printing
#include "poly-int.h"
#include "gomp-constants.h"
#include "internal-fn.h"
#include "cgraph.h"
#include "predict.h"
#include "fold-const.h"
#include "vec.h"

static unsigned int queue (dump_info_p, const_tree);
static void dequeue_and_dump (dump_info_p);
static json::array* function_decl_emit_json (tree);
static void identifier_node_add_json (tree, json::object*);
static void decl_node_add_json (tree, json::object*);
static void function_name_add_json (tree, json::object*);
static void omp_iterator_add_json (tree, json::object*);
static void omp_clause_add_json(tree, json::object*);
static void omp_atomic_memory_order_add_json (json::object*,
                                              enum omp_memory_order);
static json::object* omp_atomic_memory_order_emit_json(omp_memory_order mo);
static json::object* omp_clause_emit_json (tree);
static json::array* loc_emit_json(expanded_location);

/* Add T to the end of the queue of nodes to dump.  Returns the index
   assigned to T.  */

static unsigned int
queue (dump_info_p di, const_tree t)
{
  dump_queue_p dq;
  dump_node_info_p dni;
  unsigned int index;

  /* Assign the next available index to T.  */
  index = ++di->index;

  /* Obtain a new queue node.  */
  if (di->free_list)
    {
      dq = di->free_list;
      di->free_list = dq->next;
    }
  else
    dq = XNEW (struct dump_queue);

  /* Create a new entry in the splay-tree.  */
  dni = XNEW (struct dump_node_info);
  dni->index = index;
  dq->node = splay_tree_insert (di->nodes, (splay_tree_key) t,
				(splay_tree_value) dni);

  /* Add it to the end of the queue.  */
  dq->next = 0;
  if (!di->queue_end)
    di->queue = dq;
  else
    di->queue_end->next = dq;
  di->queue_end = dq;

  /* Return the index.  */
  return index;
}

void
address_add_json (const void* t, json::object* dummy, char* address_buffer)
{
  sprintf(address_buffer, HOST_PTR_PRINTF, t);
  dummy->set_string("addr", address_buffer);
}

/* Return args of a function declaration */

json::array*
function_decl_emit_json (tree t)
{
  bool wrote_arg = false;
  tree arg;
  json::array* arg_holder;
  json::object* arg_json;

  arg_json = new json::object ();
  arg_holder = new json::array ();

  arg = TYPE_ARG_TYPES (t);

  while (arg && arg != void_list_node && arg != error_mark_node)
    {
      wrote_arg = true;
      arg_json = node_emit_json(TREE_VALUE (arg));
      arg_holder->append(arg_json);
      arg = TREE_CHAIN (arg);
    }

  if (arg == void_list_node && !wrote_arg)
{
      arg_json->set_bool("void_list_node", true);
      arg_holder->append(arg_json);
    }

  return arg_holder;
}

/* Adds Identifier information to JSON object */

void
identifier_node_add_json (tree t, json::object* dummy)
  {
    const char* buff = IDENTIFIER_POINTER (t);
    dummy->set_string("id_to_locale", identifier_to_locale(buff));
    buff = IDENTIFIER_POINTER (t);
    dummy->set_string("id_point", buff);
  }

void
decl_node_add_json (tree t, json::object* dummy)
{
  tree name = DECL_NAME (t);
    
  if (name)
      {
        if (HAS_DECL_ASSEMBLER_NAME_P(t)
            && DECL_ASSEMBLER_NAME_SET_P(t))
          identifier_node_add_json(DECL_ASSEMBLER_NAME_RAW(t), dummy);
        else if (DECL_NAMELESS(t)
                 && DECL_IGNORED_P(t))
          name = NULL_TREE;
        else 
          identifier_node_add_json(name, dummy);
      }
    if (name == NULL_TREE)
      {
        if (TREE_CODE (t) == LABEL_DECL && LABEL_DECL_UID (t) != -1)
          {
            dummy->set_integer("Label_UID", LABEL_DECL_UID(t));
          }
        else if (TREE_CODE (t) == DEBUG_EXPR_DECL)
          {
            dummy->set_integer("Debug_UID", DEBUG_TEMP_UID (t));
          }
        else 
          {
            const char* c = TREE_CODE (t) == CONST_DECL ? "Const_UID" 
                                                        : "Decl_UID";
            dummy->set_integer(c, DECL_UID(t));
          }
      }
    if (DECL_PT_UID (t) != DECL_UID (t))
      {
        dummy->set_integer("ptDecl", DECL_PT_UID(t));
      }
}

/* Function call */

void
function_name_add_json (tree t, json::object* dummy)
{
  if (CONVERT_EXPR_P (t))
    t = TREE_OPERAND (t, 0);
  if (DECL_NAME (t))
    {
      dummy->set_string("decl_name", lang_hooks.decl_printable_name (t, 1));
      dummy->set_integer("uid", DECL_UID(t));
    }
  else
    decl_node_add_json(t, dummy);
}

/* Adds the name of a call. Enter iff t is CALL_EXPR_FN */

void
call_name_add_json (tree t, json::object* dummy)
{
  tree op0 = t;

  if (TREE_CODE (op0) == NON_LVALUE_EXPR)
    op0 = TREE_OPERAND (op0, 0);
  again:
    switch (TREE_CODE (op0))
      {
      case VAR_DECL:
      case PARM_DECL:
      case FUNCTION_DECL:
        function_name_add_json (op0, dummy);
        break;
  
      case ADDR_EXPR:
      case INDIRECT_REF:
      CASE_CONVERT:
        op0 = TREE_OPERAND (op0, 0);
        goto again;
  
      case COND_EXPR:
        {
          json::object *_x;
          _x = new json::object ();
          _x->set("if", node_emit_json(TREE_OPERAND(op0, 0)));
          _x->set("then", node_emit_json(TREE_OPERAND(op0, 1)));
          _x->set("else", node_emit_json(TREE_OPERAND(op0, 2)));
          dummy->set("call_name", _x);
        }
        break;
  
      case ARRAY_REF:
        if (VAR_P (TREE_OPERAND (op0, 0)))
  	function_name_add_json (TREE_OPERAND (op0, 0), dummy);
        else
  	dummy->set("call_name", node_emit_json (op0));
        break;
  
      case MEM_REF:
        if (integer_zerop (TREE_OPERAND (op0, 1)))
  	{
  	  op0 = TREE_OPERAND (op0, 0);
  	  goto again;
  	}
        /* Fallthrough  */
      case COMPONENT_REF:
      case SSA_NAME:
      case OBJ_TYPE_REF:
        dummy->set("call_name", node_emit_json (op0));
        break;
      default:
        break;
    }
}

/* OMP helper. */
void
omp_iterator_add_json(tree iter, json::object* dummy)
{
  json::array* iter_holder;
  iter_holder = new json::array ();

  for (tree it = iter; it; it = TREE_CHAIN (it))
    {
      for (int i = 0; i < 4; i++)
        {
          if (TREE_VEC_ELT (it, 0) != NULL_TREE)
            iter_holder->append (node_emit_json (TREE_VEC_ELT (it, 0)));
        }
    }
  dummy->set("omp_iter", iter_holder);
}
/* Omp helper. */
void
omp_clause_add_json(tree clause, json::object* dummy)
{
  char buffer[100] = {'\0'};
  const char* name;
  const char* modifier = NULL;
  switch (OMP_CLAUSE_CODE (clause))
    {
    case OMP_CLAUSE_PRIVATE:
      name = "private";
      goto print_remap;
    case OMP_CLAUSE_SHARED:
      name = "shared";
      goto print_remap;
    case OMP_CLAUSE_FIRSTPRIVATE:
      name = "firstprivate";
      goto print_remap;
    case OMP_CLAUSE_LASTPRIVATE:
      name = "lastprivate";
      if (OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (clause))
	modifier = "conditional:";
      goto print_remap;
    case OMP_CLAUSE_COPYIN:
      name = "copyin";
      goto print_remap;
    case OMP_CLAUSE_COPYPRIVATE:
      name = "copyprivate";
      goto print_remap;
    case OMP_CLAUSE_UNIFORM:
      name = "uniform";
      goto print_remap;
    case OMP_CLAUSE_USE_DEVICE_PTR:
      name = "use_device_ptr";
      if (OMP_CLAUSE_USE_DEVICE_PTR_IF_PRESENT (clause))
	modifier = "if_present:";
      goto print_remap;
    case OMP_CLAUSE_USE_DEVICE_ADDR:
      name = "use_device_addr";
      goto print_remap;
    case OMP_CLAUSE_HAS_DEVICE_ADDR:
      name = "has_device_addr";
      goto print_remap;
    case OMP_CLAUSE_IS_DEVICE_PTR:
      name = "is_device_ptr";
      goto print_remap;
    case OMP_CLAUSE_INCLUSIVE:
      name = "inclusive";
      goto print_remap;
    case OMP_CLAUSE_EXCLUSIVE:
      name = "exclusive";
      goto print_remap;
    case OMP_CLAUSE__LOOPTEMP_:
      name = "_looptemp_";
      goto print_remap;
    case OMP_CLAUSE__REDUCTEMP_:
      name = "_reductemp_";
      goto print_remap;
    case OMP_CLAUSE__CONDTEMP_:
      name = "_condtemp_";
      goto print_remap;
    case OMP_CLAUSE__SCANTEMP_:
      name = "_scantemp_";
      goto print_remap;
    case OMP_CLAUSE_ENTER:
      if (OMP_CLAUSE_ENTER_TO (clause))
	name = "to";
      else
	name = "enter";
      goto print_remap;
    case OMP_CLAUSE_LINK:
      name = "link";
      goto print_remap;
    case OMP_CLAUSE_NONTEMPORAL:
      name = "nontemporal";
      goto print_remap;
  print_remap: //FIX LATER
      strcpy(buffer, name);
      if (modifier)
        strcat(buffer, modifier);
      dummy->set (buffer, node_emit_json (OMP_CLAUSE_DECL (clause)));
      break;

    case OMP_CLAUSE_TASK_REDUCTION:
    case OMP_CLAUSE_IN_REDUCTION:
    case OMP_CLAUSE_REDUCTION:
      break;

    case OMP_CLAUSE_IF:
      strcpy(buffer, "omp_clause_if");
      switch (OMP_CLAUSE_IF_MODIFIER (clause))
        {
        case ERROR_MARK: break;
        case VOID_CST: strcat(buffer, "_cancel"); break;
        case OMP_PARALLEL: strcat(buffer, "_parallel"); break;
        case OMP_SIMD: strcat(buffer, "_simd"); break; 
        case OMP_TASK: strcat(buffer, "_task"); break;
        case OMP_TASKLOOP: strcat(buffer, "_taskloop"); break;
        case OMP_TARGET_DATA: strcat(buffer, "_target_data"); break;
        case OMP_TARGET: strcat(buffer, "_target"); break;
        case OMP_TARGET_UPDATE: strcat(buffer, "_target_update"); break;
        case OMP_TARGET_ENTER_DATA: strcat(buffer, "_target_enter_data"); break;
        case OMP_TARGET_EXIT_DATA: strcat(buffer, "_target_exit_data"); break;
        default: gcc_unreachable ();
        }
      dummy->set(buffer, node_emit_json (OMP_CLAUSE_IF_EXPR (clause)));
      break;

    case OMP_CLAUSE_SELF:
      dummy->set("omp_self", node_emit_json (OMP_CLAUSE_SELF_EXPR(clause)));
      break;

    case OMP_CLAUSE_NUM_THREADS:
      dummy->set("omp_num_threads",
                 node_emit_json (OMP_CLAUSE_NUM_THREADS_EXPR(clause)));
      break;

    case OMP_CLAUSE_NOWAIT:
      break;

    case OMP_CLAUSE_ORDERED:
      dummy->set("omp_ordered",
                 node_emit_json (OMP_CLAUSE_ORDERED_EXPR (clause)));
      break;

    case OMP_CLAUSE_DEFAULT:
      strcpy(buffer, "omp_default");
      switch (OMP_CLAUSE_DEFAULT_KIND (clause))
        {
	case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
	  break;
	case OMP_CLAUSE_DEFAULT_SHARED:
	  strcat (buffer, "_shared");
	  break;
	case OMP_CLAUSE_DEFAULT_NONE:
	  strcat (buffer, "_none");
	  break;
	case OMP_CLAUSE_DEFAULT_PRIVATE:
	  strcat (buffer, "_private");
	  break;
	case OMP_CLAUSE_DEFAULT_FIRSTPRIVATE:
	  strcat (buffer, "_firstprivate");
	  break;
	case OMP_CLAUSE_DEFAULT_PRESENT:
	  strcat (buffer, "_present");
	  break;
	default:
	  gcc_unreachable ();
        }
      dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE_SCHEDULE:
      strcpy (buffer, "schedule");
      if (OMP_CLAUSE_SCHEDULE_KIND (clause)
	  & (OMP_CLAUSE_SCHEDULE_MONOTONIC
	     | OMP_CLAUSE_SCHEDULE_NONMONOTONIC))
	{
	  if (OMP_CLAUSE_SCHEDULE_KIND (clause)
	      & OMP_CLAUSE_SCHEDULE_MONOTONIC)
	    strcat (buffer, "_monotonic");
	  else
	    strcat (buffer, "_nonmonotonic");
	}
      if (OMP_CLAUSE_SCHEDULE_SIMD (clause))
	strcat (buffer, "_simd");

      switch (OMP_CLAUSE_SCHEDULE_KIND (clause) & OMP_CLAUSE_SCHEDULE_MASK)
	{
	case OMP_CLAUSE_SCHEDULE_STATIC:
	  strcat (buffer, "_static");
	  break;
	case OMP_CLAUSE_SCHEDULE_DYNAMIC:
	  strcat (buffer, "_dynamic");
	  break;
	case OMP_CLAUSE_SCHEDULE_GUIDED:
	  strcat (buffer, "_guided");
	  break;
	case OMP_CLAUSE_SCHEDULE_RUNTIME:
	  strcat (buffer, "_runtime");
	  break;
	case OMP_CLAUSE_SCHEDULE_AUTO:
	  strcat (buffer, "_auto");
	  break;
	default:
	  gcc_unreachable ();
	}
      dummy->set(buffer,
                 node_emit_json(OMP_CLAUSE_SCHEDULE_CHUNK_EXPR(clause)));
      break;

    case OMP_CLAUSE_UNTIED:
      dummy->set_bool("omp_clause_untied", true);
      break;

    case OMP_CLAUSE_COLLAPSE:
      dummy->set("omp_collapse_collapse",
                 node_emit_json (OMP_CLAUSE_COLLAPSE_EXPR (clause)));
      break;

    case OMP_CLAUSE_FINAL:
      dummy->set("omp_final", node_emit_json (OMP_CLAUSE_FINAL_EXPR (clause)));
      break;

    case OMP_CLAUSE_MERGEABLE:
      dummy->set_bool("omp_clause_mergeable", true);
      break;

    case OMP_CLAUSE_LINEAR:
      strcpy (buffer, "omp_linear");
      if (OMP_CLAUSE_LINEAR_OLD_LINEAR_MODIFIER (clause))
	switch (OMP_CLAUSE_LINEAR_KIND (clause))
	  {
	  case OMP_CLAUSE_LINEAR_DEFAULT:
	    break;
	  case OMP_CLAUSE_LINEAR_REF:
	    strcat (buffer, "_ref");
	    break;
	  case OMP_CLAUSE_LINEAR_VAL:
	    strcat (buffer, "_val");
	    break;
	  case OMP_CLAUSE_LINEAR_UVAL:
	    strcat (buffer, "_uval");
	    break;
	  default:
	    gcc_unreachable ();
	  }
      dummy->set(buffer, node_emit_json (OMP_CLAUSE_DECL (clause)));
      if (!OMP_CLAUSE_LINEAR_OLD_LINEAR_MODIFIER (clause)
	  && OMP_CLAUSE_LINEAR_KIND (clause) != OMP_CLAUSE_LINEAR_DEFAULT)
	switch (OMP_CLAUSE_LINEAR_KIND (clause))
	  {
	    case OMP_CLAUSE_LINEAR_REF:
	      strcpy (buffer, "_ref,step");
	      break;
	    case OMP_CLAUSE_LINEAR_VAL:
	      strcpy (buffer, "_val,step");
	      break;
	    case OMP_CLAUSE_LINEAR_UVAL:
	      strcpy (buffer, "_uval,step");
	      break;
	    default:
	      gcc_unreachable ();
	  }
      dummy->set(buffer, node_emit_json (OMP_CLAUSE_LINEAR_STEP (clause)));
      break;

    case OMP_CLAUSE_ALIGNED:
      dummy->set("omp_aligned", node_emit_json (OMP_CLAUSE_DECL(clause)));
      if (OMP_CLAUSE_ALIGNED_ALIGNMENT (clause))
	{
          dummy->set("omp_aligned_alignment",
                     node_emit_json (OMP_CLAUSE_ALIGNED_ALIGNMENT (clause)));
	}
      break;

    case OMP_CLAUSE_ALLOCATE:
      if (OMP_CLAUSE_ALLOCATE_ALLOCATOR (clause))
          dummy->set("omp_allocator",
                     node_emit_json (OMP_CLAUSE_ALLOCATE_ALLOCATOR (clause)));
      if (OMP_CLAUSE_ALLOCATE_ALIGN (clause))
          dummy->set("omp_allocate_align",
                     node_emit_json (OMP_CLAUSE_ALLOCATE_ALIGN (clause)));
      dummy->set("omp_allocate",
                 node_emit_json (OMP_CLAUSE_DECL (clause)));
      break;

    case OMP_CLAUSE_AFFINITY:
      {
	tree t = OMP_CLAUSE_DECL (clause);
	if (TREE_CODE (t) == TREE_LIST
	    && TREE_PURPOSE (t)
	    && TREE_CODE (TREE_PURPOSE (t)) == TREE_VEC)
	  {
            omp_iterator_add_json(TREE_PURPOSE (t), dummy);
	    t = TREE_VALUE (t);
	  }
        dummy->set("omp_affinity", node_emit_json(t));
      }
      break;

    case OMP_CLAUSE_DEPEND:
      strcpy (buffer, "omp_depend");
      switch (OMP_CLAUSE_DEPEND_KIND (clause))
	{
	case OMP_CLAUSE_DEPEND_DEPOBJ:
	  strcat (buffer, "_depobj");
	  break;
	case OMP_CLAUSE_DEPEND_IN:
	  strcat (buffer, "_in");
	  break;
	case OMP_CLAUSE_DEPEND_OUT:
	  strcat (buffer, "_out");
	  break;
	case OMP_CLAUSE_DEPEND_INOUT:
	  strcat (buffer, "_inout");
	  break;
	case OMP_CLAUSE_DEPEND_MUTEXINOUTSET:
	  strcat (buffer, "_mutexinoutset");
	  break;
	case OMP_CLAUSE_DEPEND_INOUTSET:
	  strcat (buffer, "_inoutset");
	  break;
	case OMP_CLAUSE_DEPEND_LAST:
	  strcat (buffer, "__internal__");
	  break;
	default:
	  gcc_unreachable ();
	}
      {
	tree t = OMP_CLAUSE_DECL (clause);
	if (TREE_CODE (t) == TREE_LIST
	    && TREE_PURPOSE (t)
	    && TREE_CODE (TREE_PURPOSE (t)) == TREE_VEC)
	  {
	    omp_iterator_add_json(TREE_PURPOSE (t), dummy);
	    t = TREE_VALUE (t);
	  }
	if (t == null_pointer_node)
	  dummy->set_bool("omp_all_memory", true);
	else
          dummy->set(buffer, node_emit_json(t));
      }
      break;

    case OMP_CLAUSE_DOACROSS:
      strcpy (buffer, OMP_CLAUSE_DOACROSS_DEPEND (clause)
		     ? "omp_depend" : "omp_doacross");
      switch (OMP_CLAUSE_DOACROSS_KIND (clause))
	{
	case OMP_CLAUSE_DOACROSS_SOURCE:
	  if (OMP_CLAUSE_DOACROSS_DEPEND (clause))
	    strcat (buffer, "_source");
	  else
	    strcat (buffer, "_source:");
          dummy->set_bool(buffer, true);
	  break;
        
	case OMP_CLAUSE_DOACROSS_SINK:
          json::array* iter_holder;
          iter_holder = new json::array ();
	  strcat (buffer, "_sink:");
	  if (OMP_CLAUSE_DECL (clause) == NULL_TREE)
	    {
	      strcat (buffer, "_omp_cur_iteration-1");
              dummy->set_bool(buffer, true);
	      break;
	    }
	  for (tree t = OMP_CLAUSE_DECL (clause); t; t = TREE_CHAIN (t))
	    if (TREE_CODE (t) == TREE_LIST)
	      {
		iter_holder->append(node_emit_json(TREE_VALUE (t)));
		if (TREE_PURPOSE (t) != integer_zero_node)
		  {
                    json::object *_x;
                    _x = new json::object ();
		    if (OMP_CLAUSE_DOACROSS_SINK_NEGATIVE (t))
                      _x->set("-", node_emit_json(TREE_PURPOSE (t)));
		    else
                      _x->set("+", node_emit_json(TREE_PURPOSE (t)));
                    iter_holder->append(_x);
		  }
	      }
	    else
	      gcc_unreachable ();
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    case OMP_CLAUSE_MAP:
      strcat (buffer, "omp_map");
      if (OMP_CLAUSE_MAP_READONLY (clause))
	strcat (buffer, "_readonly,");
      switch (OMP_CLAUSE_MAP_KIND (clause))
	{
	case GOMP_MAP_ALLOC:
	case GOMP_MAP_POINTER:
	case GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION:
	  strcat (buffer, "_alloc");
	  break;
	case GOMP_MAP_IF_PRESENT:
	  strcat (buffer, "_no_alloc");
	  break;
	case GOMP_MAP_TO:
	case GOMP_MAP_TO_PSET:
	  strcat (buffer, "_to");
	  break;
	case GOMP_MAP_FROM:
	  strcat (buffer, "_from");
	  break;
	case GOMP_MAP_TOFROM:
	  strcat (buffer, "_tofrom");
	  break;
	case GOMP_MAP_FORCE_ALLOC:
	  strcat (buffer, "_force_alloc");
	  break;
	case GOMP_MAP_FORCE_TO:
	  strcat (buffer, "_force_to");
	  break;
	case GOMP_MAP_FORCE_FROM:
	  strcat (buffer, "_force_from");
	  break;
	case GOMP_MAP_FORCE_TOFROM:
	  strcat (buffer, "_force_tofrom");
	  break;
	case GOMP_MAP_FORCE_PRESENT:
	  strcat (buffer, "_force_present");
	  break;
	case GOMP_MAP_DELETE:
	  strcat (buffer, "_delete");
	  break;
	case GOMP_MAP_FORCE_DEVICEPTR:
	  strcat (buffer, "_force_deviceptr");
	  break;
	case GOMP_MAP_ALWAYS_TO:
	  strcat (buffer, "_always,to");
	  break;
	case GOMP_MAP_ALWAYS_FROM:
	  strcat (buffer, "_always,from");
	  break;
	case GOMP_MAP_ALWAYS_TOFROM:
	  strcat (buffer, "_always,tofrom");
	  break;
	case GOMP_MAP_RELEASE:
	  strcat (buffer, "_release");
	  break;
	case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  strcat (buffer, "_firstprivate");
	  break;
	case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	  strcat (buffer, "_firstprivate ref");
	  break;
	case GOMP_MAP_STRUCT:
	  strcat (buffer, "_struct");
	  break;
	case GOMP_MAP_STRUCT_UNORD:
	  strcat (buffer, "_struct_unord");
	  break;
	case GOMP_MAP_ALWAYS_POINTER:
	  strcat (buffer, "_always_pointer");
	  break;
	case GOMP_MAP_DEVICE_RESIDENT:
	  strcat (buffer, "_device_resident");
	  break;
	case GOMP_MAP_LINK:
	  strcat (buffer, "_link");
	  break;
	case GOMP_MAP_ATTACH:
	  strcat (buffer, "_attach");
	  break;
	case GOMP_MAP_DETACH:
	  strcat (buffer, "_detach");
	  break;
	case GOMP_MAP_FORCE_DETACH:
	  strcat (buffer, "_force_detach");
	  break;
	case GOMP_MAP_ATTACH_DETACH:
	  strcat (buffer, "_attach_detach");
	  break;
	case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
	  strcat (buffer, "_attach_zero_length_array_section");
	  break;
	case GOMP_MAP_PRESENT_ALLOC:
	  strcat (buffer, "_present,alloc");
	  break;
	case GOMP_MAP_PRESENT_TO:
	  strcat (buffer, "_present,to");
	  break;
	case GOMP_MAP_PRESENT_FROM:
	  strcat (buffer, "_present,from");
	  break;
	case GOMP_MAP_PRESENT_TOFROM:
	  strcat (buffer, "_present,tofrom");
	  break;
	case GOMP_MAP_ALWAYS_PRESENT_TO:
	  strcat (buffer, "_always,present,to");
	  break;
	case GOMP_MAP_ALWAYS_PRESENT_FROM:
	  strcat (buffer, "_always,present,from");
	  break;
	case GOMP_MAP_ALWAYS_PRESENT_TOFROM:
	  strcat (buffer, "_always,present,tofrom");
	  break;
	default:
	  gcc_unreachable ();
	}
      dummy->set (buffer, node_emit_json (OMP_CLAUSE_DECL (clause)));
     print_clause_size:
      if (OMP_CLAUSE_SIZE (clause))
	{
	  switch (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_MAP
		  ? OMP_CLAUSE_MAP_KIND (clause) : GOMP_MAP_TO)
	    {
	    case GOMP_MAP_POINTER:
	    case GOMP_MAP_FIRSTPRIVATE_POINTER:
	    case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	    case GOMP_MAP_ALWAYS_POINTER:
	      strcpy (buffer, "pointer assign, bias");
	      break;
	    case GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION:
	      strcpy (buffer, "pointer assign, zero-length array section, bias");
	      break;
	    case GOMP_MAP_TO_PSET:
	      strcpy (buffer, "pointer set, len");
	      break;
	    case GOMP_MAP_ATTACH:
	    case GOMP_MAP_DETACH:
	    case GOMP_MAP_FORCE_DETACH:
	    case GOMP_MAP_ATTACH_DETACH:
	    case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
	      strcpy (buffer, "bias");
	      break;
	    case GOMP_MAP_RELEASE:
	    case GOMP_MAP_DELETE:
	      if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_MAP
		  && OMP_CLAUSE_RELEASE_DESCRIPTOR (clause))
		{
		  strcpy (buffer, "pointer set, len");
		  break;
		}
	      /* Fallthrough.  */
	    default:
	      strcpy (buffer, "len");
	      break;
	    }
          dummy->set (buffer, node_emit_json (OMP_CLAUSE_SIZE (clause)));
	}
      if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_MAP
	  && OMP_CLAUSE_MAP_RUNTIME_IMPLICIT_P (clause))
        dummy->set_bool("implicit", true);
      break;

    case OMP_CLAUSE_FROM:
      strcat (buffer, "omp_from");
      if (OMP_CLAUSE_MOTION_PRESENT (clause))
	strcat (buffer, "_present");
      dummy->set(buffer, node_emit_json (OMP_CLAUSE_DECL (clause)));
      goto print_clause_size;

    case OMP_CLAUSE_TO:
      strcpy (buffer, "omp_to");
      if (OMP_CLAUSE_MOTION_PRESENT (clause))
	strcat (buffer, "_present");
      dummy->set(buffer, node_emit_json (OMP_CLAUSE_DECL (clause)));
      goto print_clause_size;

    case OMP_CLAUSE__CACHE_:
      strcat (buffer, "omp__cache__");
      if (OMP_CLAUSE__CACHE__READONLY (clause))
	strcat (buffer, "_readonly");
      dummy->set(buffer, node_emit_json (OMP_CLAUSE_DECL (clause)));
      goto print_clause_size;

    case OMP_CLAUSE_NUM_TEAMS:
      strcat (buffer, "omp_num_teams");
      if (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (clause))
	{
          dummy->set("omp_num_teams_lower",
                     node_emit_json (OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (clause)));
          strcat (buffer, "_upper");
	}
      else
        dummy->set(buffer,
                   node_emit_json (OMP_CLAUSE_NUM_TEAMS_UPPER_EXPR (clause)));
      break;

    case OMP_CLAUSE_THREAD_LIMIT:
      dummy->set("omp_thread_limit",
                 node_emit_json (OMP_CLAUSE_THREAD_LIMIT_EXPR (clause)));
      break;

    case OMP_CLAUSE_DEVICE:
      strcpy (buffer, "omp_device(");
      if (OMP_CLAUSE_DEVICE_ANCESTOR (clause))
	strcat (buffer, "_ancestor:");
      dummy->set(buffer,
                 node_emit_json (OMP_CLAUSE_DEVICE_ID (clause)));
      break;

    case OMP_CLAUSE_DIST_SCHEDULE:
      strcat (buffer, "omp_dist_schedule(static)");
      if (OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (clause))
	{
          dummy->set(buffer,
                     node_emit_json (OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (clause)));
	}
      else
        dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE_PROC_BIND:
      strcpy (buffer, "omp_proc_bind(");
      switch (OMP_CLAUSE_PROC_BIND_KIND (clause))
	{
	case OMP_CLAUSE_PROC_BIND_MASTER:
	  strcat (buffer, "_master");
	  break;
	case OMP_CLAUSE_PROC_BIND_CLOSE:
	  strcat (buffer, "_close");
	  break;
	case OMP_CLAUSE_PROC_BIND_SPREAD:
	  strcat (buffer, "_spread");
	  break;
	default:
	  gcc_unreachable ();
	}
      dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE_DEVICE_TYPE:
    strcpy (buffer, "omp_device_type");
      switch (OMP_CLAUSE_DEVICE_TYPE_KIND (clause))
	{
	case OMP_CLAUSE_DEVICE_TYPE_HOST:
	  strcat (buffer, "_host");
	  break;
	case OMP_CLAUSE_DEVICE_TYPE_NOHOST:
	  strcat (buffer, "_nohost");
	  break;
	case OMP_CLAUSE_DEVICE_TYPE_ANY:
	  strcat (buffer, "_any");
	  break;
	default:
	  gcc_unreachable ();
	}
      dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE_SAFELEN:
      dummy->set("omp_safelen",
                 node_emit_json (OMP_CLAUSE_SAFELEN_EXPR (clause)));
      break;

    case OMP_CLAUSE_SIMDLEN:
      dummy->set("omp_simdlen",
                 node_emit_json (OMP_CLAUSE_SIMDLEN_EXPR (clause)));
      break;

    case OMP_CLAUSE_PRIORITY:
      dummy->set("omp_priority",
                 node_emit_json (OMP_CLAUSE_PRIORITY_EXPR (clause)));
      break;

    case OMP_CLAUSE_GRAINSIZE:
      strcpy (buffer, "omp_grainsize");
      if (OMP_CLAUSE_GRAINSIZE_STRICT (clause))
        strcat (buffer, "_strict");
      dummy->set(buffer,
                 node_emit_json (OMP_CLAUSE_GRAINSIZE_EXPR (clause)));
      break;

    case OMP_CLAUSE_NUM_TASKS:
      strcpy (buffer, "omp_num_tasks");
      if (OMP_CLAUSE_NUM_TASKS_STRICT (clause))
	strcat (buffer, "_strict");
      dummy->set(buffer,
                 node_emit_json (OMP_CLAUSE_NUM_TASKS_EXPR (clause)));
      break;

    case OMP_CLAUSE_HINT:
      dummy->set("omp_hint",
                 node_emit_json (OMP_CLAUSE_HINT_EXPR(clause)));
      break;

    case OMP_CLAUSE_FILTER:
      dummy->set("omp_filter",
                 node_emit_json (OMP_CLAUSE_FILTER_EXPR(clause)));
      break;

    case OMP_CLAUSE_DEFAULTMAP:
      strcat (buffer, "omp_defaultmap");
      switch (OMP_CLAUSE_DEFAULTMAP_BEHAVIOR (clause))
	{
	case OMP_CLAUSE_DEFAULTMAP_ALLOC:
	  strcat (buffer, "_alloc");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_TO:
	  strcat (buffer, "_to");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_FROM:
	  strcat (buffer, "_from");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_TOFROM:
	  strcat (buffer, "_tofrom");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE:
	  strcat (buffer, "_firstprivate");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_NONE:
	  strcat (buffer, "_none");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_PRESENT:
	  strcat (buffer, "_present");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_DEFAULT:
	  strcat (buffer, "_default");
	  break;
	default:
	  gcc_unreachable ();
	}
      //Check later if we're happy with this
      switch (OMP_CLAUSE_DEFAULTMAP_CATEGORY (clause))
	{
	case OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED:
	  break;
	case OMP_CLAUSE_DEFAULTMAP_CATEGORY_ALL:
	  strcat (buffer, ":all");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_CATEGORY_SCALAR:
	  strcat (buffer, ":scalar");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_CATEGORY_AGGREGATE:
	  strcat (buffer, ":aggregate");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_CATEGORY_ALLOCATABLE:
	  strcat (buffer, ":allocatable");
	  break;
	case OMP_CLAUSE_DEFAULTMAP_CATEGORY_POINTER:
	  strcat (buffer, ":pointer");
	  break;
	default:
	  gcc_unreachable ();
	}
      dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE_ORDER:
      strcpy (buffer, "omp_order_");
      if (OMP_CLAUSE_ORDER_UNCONSTRAINED (clause))
	strcat (buffer, "unconstrained:");
      else if (OMP_CLAUSE_ORDER_REPRODUCIBLE (clause))
	strcat (buffer, "reproducible:");
      strcat (buffer, "concurrent)");
      dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE_BIND:
      strcpy (buffer, "omp_bind");
      switch (OMP_CLAUSE_BIND_KIND (clause))
	{
	case OMP_CLAUSE_BIND_TEAMS:
	  strcat (buffer, "_teams");
	  break;
	case OMP_CLAUSE_BIND_PARALLEL:
	  strcat (buffer, "_parallel");
	  break;
	case OMP_CLAUSE_BIND_THREAD:
	  strcat (buffer, "_thread");
	  break;
	default:
	  gcc_unreachable ();
	}
      dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE__SIMDUID_:
      dummy->set("omp__simduid__", 
                 node_emit_json(OMP_CLAUSE__SIMDUID__DECL (clause)));
      break;

    case OMP_CLAUSE__SIMT_:
      dummy->set_bool("omp__simt__", true);
      break;

    //In our current implementation, we dump exprs etc even if not NULL.
    case OMP_CLAUSE_GANG: 
      dummy->set_bool ("omp_gang", true);
      if (OMP_CLAUSE_GANG_EXPR (clause) != NULL_TREE)
          dummy->set("num", node_emit_json (OMP_CLAUSE_GANG_EXPR (clause)));
      if (OMP_CLAUSE_GANG_STATIC_EXPR (clause) != NULL_TREE)
	{
	  strcpy (buffer, "static");
	  if (OMP_CLAUSE_GANG_STATIC_EXPR (clause)
	      == integer_minus_one_node)
	    strcat (buffer, "*"); 
          dummy->set(buffer,
                     node_emit_json (OMP_CLAUSE_GANG_STATIC_EXPR (clause)));
	}
      break;

    case OMP_CLAUSE_ASYNC:
      dummy->set("omp_async", 
                 node_emit_json (OMP_CLAUSE_ASYNC_EXPR (clause)));
      break;

    case OMP_CLAUSE_AUTO:
    case OMP_CLAUSE_SEQ:
      strcpy (buffer, omp_clause_code_name[OMP_CLAUSE_CODE (clause)]);
      dummy->set_bool(buffer, true);
      break;

    case OMP_CLAUSE_WAIT:
      dummy->set("omp_wait",
                 node_emit_json (OMP_CLAUSE_WAIT_EXPR (clause)));
      break;

    case OMP_CLAUSE_WORKER:
      dummy->set("omp_worker",
                 node_emit_json (OMP_CLAUSE_WORKER_EXPR (clause)));
      break;

    case OMP_CLAUSE_VECTOR:
      dummy->set("omp_vector",
                 node_emit_json (OMP_CLAUSE_VECTOR_EXPR (clause)));
      break;

    case OMP_CLAUSE_NUM_GANGS:
      dummy->set("omp_num_gangs",
                 node_emit_json (OMP_CLAUSE_NUM_GANGS_EXPR (clause)));
      break;

    case OMP_CLAUSE_NUM_WORKERS:
      dummy->set("omp_num_workers",
                 node_emit_json (OMP_CLAUSE_NUM_WORKERS_EXPR (clause)));
      break;

    case OMP_CLAUSE_VECTOR_LENGTH:
      dummy->set("omp_vector_length",
                 node_emit_json (OMP_CLAUSE_VECTOR_LENGTH_EXPR (clause)));
      break;

    case OMP_CLAUSE_INBRANCH:
      dummy->set_bool ("inbranch", true);
      break;
    case OMP_CLAUSE_NOTINBRANCH:
      dummy->set_bool ("notinbranch", true);
      break;
    case OMP_CLAUSE_FOR:
      dummy->set_bool ("for", true);
      break;
    case OMP_CLAUSE_PARALLEL:
      dummy->set_bool ("parallel", true);
      break;
    case OMP_CLAUSE_SECTIONS:
      dummy->set_bool ("sections", true);
      break;
    case OMP_CLAUSE_TASKGROUP:
      dummy->set_bool ("taskgroup", true);
      break;
    case OMP_CLAUSE_NOGROUP:
      dummy->set_bool ("nogroup", true);
      break;
    case OMP_CLAUSE_THREADS:
      dummy->set_bool ("threads", true);
      break;
    case OMP_CLAUSE_SIMD:
      dummy->set_bool ("simd", true);
      break;
    case OMP_CLAUSE_INDEPENDENT:
      dummy->set_bool ("independent", true);
      break;
    case OMP_CLAUSE_TILE:
      dummy->set ("omp_tile", node_emit_json (OMP_CLAUSE_TILE_LIST (clause)));
      break;
    case OMP_CLAUSE_PARTIAL:
      dummy->set ("omp_partial",
                  node_emit_json (OMP_CLAUSE_PARTIAL_EXPR (clause)));
      break;
    case OMP_CLAUSE_FULL:
      dummy->set_bool("full", true);
      break;
    case OMP_CLAUSE_SIZES:
      dummy->set("omp_sizes", node_emit_json (OMP_CLAUSE_SIZES_LIST (clause)));
      break;
    case OMP_CLAUSE_IF_PRESENT:
      dummy->set_bool("if_present", true);
      break;
    case OMP_CLAUSE_FINALIZE:
      dummy->set_bool("finalize", true);
      break;
    case OMP_CLAUSE_NOHOST:
      dummy->set_bool("nohost", true);
      break;
    case OMP_CLAUSE_DETACH:
      dummy->set("omp_detach", node_emit_json (OMP_CLAUSE_DECL (clause)));
      break;
    default:
      gcc_unreachable();
    }
}

void
omp_atomic_memory_order_add_json (json::object* dummy, enum omp_memory_order mo)
{
  switch (mo & OMP_MEMORY_ORDER_MASK)
    {
    case OMP_MEMORY_ORDER_RELAXED:
      dummy->set_string ("omp_memory_order", "relaxed");
      break;
    case OMP_MEMORY_ORDER_SEQ_CST:
      dummy->set_string ("omp_memory_order", "seq_cst");
      break;
    case OMP_MEMORY_ORDER_ACQ_REL:
      dummy->set_string ("omp_memory_order", "acq_rel");
      break;
    case OMP_MEMORY_ORDER_ACQUIRE:
      dummy->set_string ("omp_memory_order", "acquire");
      break;
    case OMP_MEMORY_ORDER_RELEASE:
      dummy->set_string ("omp_memory_order", "release");
      break;
    case OMP_MEMORY_ORDER_UNSPECIFIED:
      break;
    default:
      gcc_unreachable ();
    }
  switch (mo & OMP_FAIL_MEMORY_ORDER_MASK)
    {
    case OMP_FAIL_MEMORY_ORDER_RELAXED:
      dummy->set_string ("omp_memory_order", "fail(relaxed)");
      break;
    case OMP_FAIL_MEMORY_ORDER_SEQ_CST:
      dummy->set_string ("omp_memory_order", "fail(seq_cst)");
      break;
    case OMP_FAIL_MEMORY_ORDER_ACQUIRE:
      dummy->set_string ("omp_memory_order", "fail(acquire)");
      break;
    case OMP_FAIL_MEMORY_ORDER_UNSPECIFIED:
      dummy->set_string ("omp_memory_order", "fail(unspecified)");
      break;
    default:
      gcc_unreachable ();
    }
}

json::object*
omp_atomic_memory_order_emit_json(omp_memory_order mo)
{
  json::object* _x;
  _x = new json::object ();
  omp_atomic_memory_order_add_json(_x, mo);
  return _x;
}

json::object*
omp_clause_emit_json(tree t)
{
  json::object* _x;
  _x = new json::object ();
  omp_clause_add_json(t, _x);
  return _x;
}

json::array*
loc_emit_json (expanded_location xloc)
{
  json::array *loc_holder;
  json::object *loc_info;
  loc_info = new json::object ();
  loc_info->set_string("file", xloc.file);
  loc_info->set_integer("line", xloc.line);
  loc_info->set_integer("column", xloc.column);
  loc_holder = new json::array ();
  loc_holder->append(loc_info);
  return loc_holder;
}

/* Here we emit JSON data for a GENERIC node and children. 
 * c.f. dump_generic_node and print-tree's debug_tree().   */

json::object* 
node_emit_json(tree t)
{
  tree op0, op1, type;
  enum tree_code code;
  expanded_location xloc;
  json::object *dummy;
  json::array* holder;
  char address_buffer[sizeof(&t)] = {"\0"};

  dummy = new json::object ();
  holder = new json::array ();

  code = TREE_CODE (t);

  address_add_json(t, dummy, address_buffer);

  if (TREE_CODE_CLASS (code) == tcc_declaration
      && code != TRANSLATION_UNIT_DECL)
    {
    xloc = expand_location (DECL_SOURCE_LOCATION (t));
    dummy->set("decl_loc", loc_emit_json(xloc));
    }
  if (EXPR_HAS_LOCATION(t))
    {
      xloc = expand_location (EXPR_LOCATION (t));
      dummy->set("expr_loc", loc_emit_json(xloc));
    }
  if (EXPR_HAS_RANGE (t))
    {
      source_range r = EXPR_LOCATION_RANGE (t);
      if (r.m_start)
      {
        xloc = expand_location (r.m_start);
        dummy->set("start_loc", loc_emit_json(xloc));
      } else {
        dummy->set_string("start_loc", "unknown");
      }
      if (r.m_finish)
      {
        xloc = expand_location (r.m_finish);
        dummy->set("finish_loc", loc_emit_json(xloc));
      } else {
        dummy->set_string("finish_loc", "unknown");
      }
    }
  dummy->set_string("tree code", get_tree_code_name(TREE_CODE (t)));
  //Flag handling
  if (TREE_ADDRESSABLE (t))
    dummy->set_bool ("addressable", true);
  if (TREE_THIS_VOLATILE (t))
    dummy->set_bool ("volatile", true);
  if (TREE_ASM_WRITTEN (t))
    dummy->set_bool ("asm_written", true);
  if (TREE_USED (t))
    dummy->set_bool ("used", true);
  if (TREE_NOTHROW (t))
    dummy->set_bool ("nothrow", true);
  if (TREE_PUBLIC (t))
    dummy->set_bool ("public", true);
  if (TREE_PRIVATE (t))
    dummy->set_bool ("private", true);
  if (TREE_PROTECTED (t))
    dummy->set_bool ("protected", true);
  if (TREE_STATIC (t))
    dummy->set_bool (code == CALL_EXPR ? " must-tail-call"
                                       : " static", true);
  if (TREE_DEPRECATED (t))
    dummy->set_bool ("deprecated", true);
  if (TREE_VISITED (t))
    dummy->set_bool ("visited", true);

  if (code != TREE_VEC && code != SSA_NAME && code != INTEGER_CST)
    {
      if (TREE_UNAVAILABLE (t))
	dummy->set_bool ("unavailable", true);
      if (TREE_LANG_FLAG_0 (t))
	dummy->set_bool ("tree_0", true);
      if (TREE_LANG_FLAG_1 (t))
        dummy->set_bool ("tree_1", true);
      if (TREE_LANG_FLAG_2 (t))
	dummy->set_bool ("tree_2", true);
      if (TREE_LANG_FLAG_3 (t))
	dummy->set_bool ("tree_3", true);
      if (TREE_LANG_FLAG_4 (t))
	dummy->set_bool ("tree_4", true);
      if (TREE_LANG_FLAG_5 (t))
	dummy->set_bool ("tree_5", true);
      if (TREE_LANG_FLAG_6 (t))
	dummy->set_bool ("tree_6", true);
    }

  if (TREE_CODE_CLASS(code) == tcc_declaration)
    {
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
        {
          if (DECL_UNSIGNED (t))
	    dummy->set_bool ("unsigned", true);
	  if (DECL_IGNORED_P (t))
      	    dummy->set_bool ("ignored", true);
	  if (DECL_ABSTRACT_P (t))
	    dummy->set_bool ("abstract", true);
          if (DECL_EXTERNAL (t))
            dummy->set_bool ("external", true);
	  if (DECL_NONLOCAL (t))
	    dummy->set_bool ("nonlocal", true);
	}
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
        {
	  if (DECL_WEAK (t))
	    dummy->set_bool ("weak", true);
	  if (DECL_IN_SYSTEM_HEADER (t))
	    dummy->set_bool ("in_system_header", true);
        }
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL)
	  && code != LABEL_DECL
	  && code != FUNCTION_DECL
	  && DECL_REGISTER (t))
	dummy->set_bool ("regdecl", true);

      if (code == TYPE_DECL && TYPE_DECL_SUPPRESS_DEBUG (t))
	dummy->set_bool ("suppress-debug", true);
      
      if (code == FUNCTION_DECL
	  && DECL_FUNCTION_SPECIFIC_TARGET (t))
	dummy->set_bool ("function-specific-target", true);
      if (code == FUNCTION_DECL
	  && DECL_FUNCTION_SPECIFIC_OPTIMIZATION (t))
	dummy->set_bool ("function-specific-opt", true);
      if (code == FUNCTION_DECL && DECL_DECLARED_INLINE_P (t))
	dummy->set_bool ("autoinline", true);
      if (code == FUNCTION_DECL && DECL_UNINLINABLE (t))
	dummy->set_bool ("uninlinable", true);
      if (code == FUNCTION_DECL && fndecl_built_in_p (t))
	dummy->set_bool ("built-in", true);
      if (code == FUNCTION_DECL && DECL_STATIC_CHAIN (t))
	dummy->set_bool ("static-chain", true);
      if (TREE_CODE (t) == FUNCTION_DECL && decl_is_tm_clone (t))
	dummy->set_bool ("tm-clone", true);

      if (code == FIELD_DECL && DECL_PACKED (t))
	dummy->set_bool ("packed", true);
      if (code == FIELD_DECL && DECL_BIT_FIELD (t))
	dummy->set_bool ("bit-field", true);
      if (code == FIELD_DECL && DECL_NONADDRESSABLE_P (t))
	dummy->set_bool ("nonaddressable", true);

      if (code == LABEL_DECL && EH_LANDING_PAD_NR (t))
        dummy->set_integer("landing_pad", EH_LANDING_PAD_NR(t));

      if (code == VAR_DECL && DECL_IN_TEXT_SECTION (t))
	dummy->set_bool ("in-text-section", true);
      if (code == VAR_DECL && DECL_IN_CONSTANT_POOL (t))
	dummy->set_bool ("in-constant-pool", true);
      if (code == VAR_DECL && DECL_COMMON (t))
	dummy->set_bool ("common", true);
      if ((code == VAR_DECL || code == PARM_DECL) && DECL_READ_P (t))
	dummy->set_bool ("read", true);
      if (code == VAR_DECL && DECL_THREAD_LOCAL_P (t))
	  dummy->set_bool (tls_model_names[DECL_TLS_MODEL (t)], true);

      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  if (DECL_VIRTUAL_P (t))
	    dummy->set_bool ("virtual", true);
	  if (DECL_PRESERVE_P (t))
	    dummy->set_bool ("preserve", true);
	  if (DECL_LANG_FLAG_0 (t))
	    dummy->set_bool ("decl_0", true);
	  if (DECL_LANG_FLAG_1 (t))
	    dummy->set_bool ("decl_1", true);
	  if (DECL_LANG_FLAG_2 (t))
	    dummy->set_bool ("decl_2", true);
	  if (DECL_LANG_FLAG_3 (t))
	    dummy->set_bool ("decl_3", true);
	  if (DECL_LANG_FLAG_4 (t))
	    dummy->set_bool ("decl_4", true);
	  if (DECL_LANG_FLAG_5 (t))
	    dummy->set_bool ("decl_5", true);
	  if (DECL_LANG_FLAG_6 (t))
	    dummy->set_bool ("decl_6", true);
	  if (DECL_LANG_FLAG_7 (t))
	    dummy->set_bool ("decl_7", true);
	  if (DECL_LANG_FLAG_8 (t))
	    dummy->set_bool ("decl_8", true);

	  dummy->set_string("mode", GET_MODE_NAME (DECL_MODE (t)));
	}


      if ((code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
	  && DECL_BY_REFERENCE (t))
	dummy->set_bool ("passed-by-reference", true);

      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS)  && DECL_DEFER_OUTPUT (t))
	dummy->set_bool ("defer-output", true);

    } //end tcc_decl flags

    if (TREE_CODE_CLASS(code) == tcc_type)
      {
      if (TYPE_UNSIGNED (t))
	dummy->set_bool ("unsigned", true);

      if (TYPE_NO_FORCE_BLK (t))
	dummy->set_bool ("no-force-blk", true);
      
      if (code == ARRAY_TYPE && TYPE_STRING_FLAG (t))
	dummy->set_bool ("string-flag", true);

      if (TYPE_NEEDS_CONSTRUCTING (t))
	dummy->set_bool ("needs-constructing", true);

      if ((code == RECORD_TYPE
	   || code == UNION_TYPE
	   || code == QUAL_UNION_TYPE
	   || code == ARRAY_TYPE)
	  && TYPE_REVERSE_STORAGE_ORDER (t))
	dummy->set_bool ("reverse-storage-order", true);

      if ((code == RECORD_TYPE
	   || code == UNION_TYPE)
	  && TYPE_CXX_ODR_P (t))
	dummy->set_bool ("cxx-odr-p", true);

      if ((code == RECORD_TYPE
	   || code == UNION_TYPE)
	  && TYPE_CXX_ODR_P (t))
	dummy->set_bool ("cxx-odr-p", true);

      if ((code == RECORD_TYPE
	   || code == UNION_TYPE)
	  && TYPE_INCLUDES_FLEXARRAY (t))
	dummy->set_bool ("includes-flexarray", true);
      
      if ((code == UNION_TYPE || code == RECORD_TYPE)
	  && TYPE_TRANSPARENT_AGGR (t))
	dummy->set_bool ("transparent-aggr", true);
      else if (code == ARRAY_TYPE
	       && TYPE_NONALIASED_COMPONENT (t))
	dummy->set_bool ("nonaliased-component", true);

      if (TYPE_PACKED (t))
	dummy->set_bool ("packed", true);

      if (TYPE_RESTRICT (t))
	dummy->set_bool ("restrict", true);

      if (TYPE_LANG_FLAG_0 (t))
	dummy->set_bool ("type_0", true);
      if (TYPE_LANG_FLAG_1 (t))
	dummy->set_bool ("type_1", true);
      if (TYPE_LANG_FLAG_2 (t))
	dummy->set_bool ("type_2", true);
      if (TYPE_LANG_FLAG_3 (t))
	dummy->set_bool ("type_3", true);
      if (TYPE_LANG_FLAG_4 (t))
	dummy->set_bool ("type_4", true);
      if (TYPE_LANG_FLAG_5 (t))
	dummy->set_bool ("type_5", true);
      if (TYPE_LANG_FLAG_6 (t))
	dummy->set_bool ("type_6", true);
      if (TYPE_LANG_FLAG_7 (t))
	dummy->set_bool ("type_7", true);
      } //end tcc_type flags
  
  // Accessors
  switch (code)
  {
    case IDENTIFIER_NODE:
      dummy->set_string("identifier", identifier_to_locale ((IDENTIFIER_POINTER(t))));
      break;
    case TREE_LIST:
      while (t && t != error_mark_node)
      {
        if (TREE_PURPOSE (t))
	{
          holder->append(node_emit_json(TREE_PURPOSE(t)));
	}
	holder->append(node_emit_json(TREE_VALUE(t)));
        t = TREE_CHAIN(t);
      }
      break;
    case TREE_BINFO:
      holder->append(node_emit_json(BINFO_TYPE(t)));
      break;
    case TREE_VEC:
      {
        size_t i;
        if (TREE_VEC_LENGTH(t) > 0)
	  {
            size_t len = TREE_VEC_LENGTH (t);
	    for (i = 0; i < len ; i++)
	    {
	      holder->append(node_emit_json(TREE_VEC_ELT(t, i)));
	    }
	  }
      }
      break;

    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case BITINT_TYPE:
    case OPAQUE_TYPE:
      {
	unsigned int quals = TYPE_QUALS (t);
	enum tree_code_class tclass;
        json::object* _x;

        _x = new json::object ();

	if (quals & TYPE_QUAL_ATOMIC)
	  _x->set_bool("atomic", true);
	if (quals & TYPE_QUAL_CONST)
	  _x->set_bool("const", true);
	if (quals & TYPE_QUAL_VOLATILE)
	  _x->set_bool("volatile", true);
	if (quals & TYPE_QUAL_RESTRICT)
	  _x->set_bool("restrict", true);
      
        dummy->set("quals", _x);

	if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (t)))
	    dummy->set_integer("address space", TYPE_ADDR_SPACE(t));
	
	tclass = TREE_CODE_CLASS (TREE_CODE(t));

	if (tclass == tcc_declaration)
	  {
	  if (DECL_NAME (t))
            decl_node_add_json(t, dummy);
	  else
	    dummy->set_string("decl", "<unnamed type decl>");
	  }
	else if (tclass == tcc_type)
	  {
	    if (TYPE_NAME (t))
	    {  
	      if (TREE_CODE(TYPE_NAME (t)) == IDENTIFIER_NODE)
                dummy->set("identifier", node_emit_json(TYPE_NAME(t)));
	      else if (TREE_CODE (TYPE_NAME (t)) == TYPE_DECL
	               && DECL_NAME (TYPE_NAME (t)))
                decl_node_add_json( TYPE_NAME (t), dummy);
	      else 
                dummy->set_string("type_name", "unnamed");
	    }
            else if (TREE_CODE (t) == VECTOR_TYPE)
    	      {
              for (long unsigned int i = 0; i < NUM_POLY_INT_COEFFS; i++)
                {
                  auto _poly_int = TYPE_VECTOR_SUBPARTS(t);
                  dummy->set_integer("foo", _poly_int.coeffs[i]);
                }
              dummy->set("vector_subparts", holder);
    	      }
            else if (TREE_CODE (t) == INTEGER_TYPE)
    	      {
    	        if (TYPE_PRECISION (t) == CHAR_TYPE_SIZE)
    	          dummy->set_string("type precision", (TYPE_UNSIGNED(t)
    	          				   ? "unsigned char"
    	          				   : "signed char"));
    	        else if (TYPE_PRECISION (t) == SHORT_TYPE_SIZE)
    	          dummy->set_string("type precision", (TYPE_UNSIGNED(t)
    	          				   ? "unsigned short"
    	          				   : "signed short"));
    	        else if (TYPE_PRECISION (t) == INT_TYPE_SIZE)
    	          dummy->set_string("type precision", (TYPE_UNSIGNED(t)
    	          				   ? "unsigned int"
    	          				   : "signed int"));
    	        else if (TYPE_PRECISION (t) == LONG_TYPE_SIZE)
    	          dummy->set_string("type precision", (TYPE_UNSIGNED(t)
    	          				   ? "unsigned long"
    	          				   : "signed long"));
    	        else if (TYPE_PRECISION (t) == LONG_LONG_TYPE_SIZE)
    	          dummy->set_string("type precision", (TYPE_UNSIGNED(t)
    	          				   ? "unsigned long long"
    	          				   : "signed long long"));
    	        else if (TYPE_PRECISION (t) == CHAR_TYPE_SIZE
 && pow2p_hwi (TYPE_PRECISION (t)))
    	            dummy->set_integer(TYPE_UNSIGNED(t) ? "uint": "int",
	                               TYPE_PRECISION(t));
	        else
		    dummy->set_integer(TYPE_UNSIGNED(t)
				      ? "unnamed-unsigned"
				      : "unnamed-signed", TYPE_PRECISION(t));
  	      }
	    else if (TREE_CODE (t) == COMPLEX_TYPE) //make sure this is okay later, need track cmplx here?
	      {
	        holder->append(node_emit_json(TREE_TYPE(t)));
                dummy->set("complex", holder);
	      }
	    else if (TREE_CODE (t) == REAL_TYPE)
	      {
		dummy->set_integer("float", TYPE_PRECISION(t));
	      }
	    else if (TREE_CODE (t) == FIXED_POINT_TYPE)
	      {
		dummy->set_integer("fixed point", TYPE_PRECISION(t));
	      }
	    else if (TREE_CODE (t) == BOOLEAN_TYPE)
	      {
		dummy->set_integer(TYPE_UNSIGNED(t) 
		                   ? "unsigned boolean"
				   : "signed boolean", TYPE_PRECISION(t));
	      }
	    else if (TREE_CODE (t) == BITINT_TYPE)
	      {
		dummy->set_integer(TYPE_UNSIGNED (t)
		                   ? "unsigned_BitInt"
				   : "_BitInt", TYPE_PRECISION(t));
	      }
	    else if (TREE_CODE (t) == VOID_TYPE)
	      dummy->set_bool("float", true);
	    else
	      dummy->set_bool("unnamed type", true);
          }
      }
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
        if (TREE_TYPE (t) == NULL)
   	  dummy->set_bool("null type", true);
        else if (TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
          {
            tree function_node = TREE_TYPE(t);
   	    tree arg_node = TYPE_ARG_TYPES(function_node);
   	    json::array* args_holder;
	    json::object* it_args;
   	    json::object* _id;

	    args_holder = new json::array ();
	    _id = new json::object ();
	    
            dummy->set("fnode", node_emit_json(function_node));
   
   	    if (TYPE_IDENTIFIER (t))
   	      _id->set("type identifier", node_emit_json(TYPE_NAME(t)));
            else 
              {
                char* buff;
                buff = new char ();
                print_hex(TYPE_UID(t), buff);
   	        _id->set_string("uid", buff);
              }
            dummy->set("function decl", function_decl_emit_json(function_node));
            if (arg_node && arg_node != void_list_node && arg_node != error_mark_node)
	      it_args = new json::object();
            while (arg_node && arg_node != void_list_node && arg_node != error_mark_node)
   	      {
	        it_args = node_emit_json(arg_node);
	        args_holder->append(it_args);
   	        arg_node = TREE_CHAIN (arg_node);
   	      }
   	    dummy->set("type_uid", _id);
   	    dummy->set("args", args_holder);
          }
        else
          {
   	  unsigned int quals = TYPE_QUALS (t);
   	  dummy->set("tree_type", node_emit_json(TREE_TYPE(t)));
   	  json::object *_x; 

          _x = new json::object ();
  
  	  if (quals & TYPE_QUAL_ATOMIC)
  	    _x->set_bool("atomic", true);
  	  if (quals & TYPE_QUAL_CONST)
  	    _x->set_bool("const", true);
  	  if (quals & TYPE_QUAL_VOLATILE)
  	    _x->set_bool("volatile", true);
  	  if (quals & TYPE_QUAL_RESTRICT)
  	    _x->set_bool("restrict", true);
        
          dummy->set("quals", _x);
   	  if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (t)))
   	    dummy->set_integer("address space", TYPE_ADDR_SPACE (t));
   	  if (TYPE_REF_CAN_ALIAS_ALL (t))
   	    dummy->set_bool("ref can alias all", true);
          }
      }
      break;

    case OFFSET_TYPE:
      break;

    case MEM_REF:
    case TARGET_MEM_REF:
      {
        if (TREE_CODE (t) == MEM_REF
                 && integer_zerop (TREE_OPERAND (t, 1))
                 && TREE_CODE (TREE_OPERAND (t, 0)) != INTEGER_CST
                 && TREE_TYPE (TREE_OPERAND (t, 0)) != NULL_TREE
                 && (TREE_TYPE (TREE_TYPE (TREE_OPERAND (t, 0)))
             	     == TREE_TYPE (TREE_TYPE (TREE_OPERAND (t, 1))))
             	 && (TYPE_MODE (TREE_TYPE (TREE_OPERAND (t, 0)))
             	     == TYPE_MODE (TREE_TYPE (TREE_OPERAND (t, 1))))
             	 && (TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (t, 0)))
             	     == TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (t, 1))))
                 && (TYPE_MAIN_VARIANT (TREE_TYPE (t))
                     == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (TREE_OPERAND (t, 1))))))
          {
            if (TREE_CODE (TREE_OPERAND (t, 0)) != ADDR_EXPR)
              {
                dummy->set("base", node_emit_json(TREE_OPERAND (t, 0)));
              }
            else
              dummy->set("base", node_emit_json(TREE_OPERAND (TREE_OPERAND (t, 0), 0)));
          }
        else
          { //Check later
            tree type = TREE_TYPE(t);
            tree op1 = TREE_OPERAND(t, 1);
            tree op1type = TYPE_MAIN_VARIANT (TREE_TYPE (op1));

            tree op0size = TYPE_SIZE(type);
            tree op1size = TYPE_SIZE (TREE_TYPE(op1type));

            if (!op0size || !op1size 
                || ! operand_equal_p(op0size, op1size, 0))
            {
              dummy->set("tree_type_size", node_emit_json(type));
            }
            dummy->set("op1_type_size", node_emit_json(op1type));

            if (!integer_zerop (op1))
              dummy->set("offset", node_emit_json(op1));
            if (TREE_CODE(t) == TARGET_MEM_REF)
              {
                tree temp = TMR_INDEX2 (t);
                if (temp)
                  dummy->set("tmr_index2", node_emit_json(temp));
                temp = TMR_INDEX (t);
                if (temp)
                  {
                    dummy->set("tmr_index", node_emit_json(temp));
                    temp = TMR_STEP(t);
                    if (temp)
                      dummy->set("tmr_step", node_emit_json(temp));
                  }
              }
          }
        if (MR_DEPENDENCE_CLIQUE (t) != 0)
          {
            dummy->set_integer("clique", MR_DEPENDENCE_CLIQUE(t));
            dummy->set_integer("base", MR_DEPENDENCE_BASE(t));
          }
      }
    break;

    /* FIXME : As in tree-pretty-print.cc, the following four codes are
     *         incomplete. See print_struct_decl therein the above. */
    case ARRAY_TYPE:
      {
        unsigned int quals = TYPE_QUALS (t);
	if (quals & TYPE_QUAL_ATOMIC)
	  dummy->set_bool("atomic", true);
	if (quals & TYPE_QUAL_CONST)
	  dummy->set_bool("const", true);
	if (quals & TYPE_QUAL_VOLATILE)
	  dummy->set_bool("volatile", true);
      }
      break;
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
        unsigned int quals = TYPE_QUALS (t);
        json::object *_x;

        _x = new json::object ();

        if (quals & TYPE_QUAL_ATOMIC)
          _x->set_bool("atomic", true);
        if (quals & TYPE_QUAL_CONST)
          _x->set_bool("const", true);
        if (quals & TYPE_QUAL_VOLATILE)
          _x->set_bool("volatile", true);
        
        dummy->set("type_quals", _x);

        if (TYPE_NAME(t))
          dummy->set("type_name", node_emit_json(TYPE_NAME(t)));
      }
      break;
    case LANG_TYPE:
      break;
    case INTEGER_CST:
      {
        dummy->set_bool("integer_cst", true);

        if ((POINTER_TYPE_P (TREE_TYPE (t))) 
            || (TYPE_PRECISION (TREE_TYPE (t))
                < TYPE_PRECISION (integer_type_node))
            || exact_log2 (TYPE_PRECISION (TREE_TYPE (t))) == -1
            || tree_int_cst_sgn (t) < 0)
          {
            holder->append( node_emit_json (TREE_TYPE(t)));
            dummy->set("_Literal", holder);
          }
        if (TREE_CODE (TREE_TYPE(t)) == POINTER_TYPE) 
          {
            // In bytes
            dummy->set_bool("pointer", true);
            dummy->set_integer("val", TREE_INT_CST_LOW(t));
          }
        else if (tree_fits_shwi_p(t))
          dummy->set_integer("val", tree_to_shwi (t));
        else if (tree_fits_uhwi_p(t))
          dummy->set_integer("val", tree_to_uhwi (t));
        else
          {
            wide_int val = wi::to_wide (t);
            char buff[WIDE_INT_PRINT_BUFFER_SIZE];
        
            print_dec(wi::to_wide (t), buff, TYPE_SIGN (TREE_TYPE (t)));
            dummy->set_string("val", buff);
        }
      if (TREE_OVERFLOW (t))
        dummy->set_bool("overflow", true);
      }
      break;

    case POLY_INT_CST:
      for (unsigned int i = 1; i < NUM_POLY_INT_COEFFS; i++)
        holder->append(node_emit_json(POLY_INT_CST_COEFF(t, i)));
      dummy->set("poly_int_cst", holder);
      break;

    case REAL_CST:
      {
        REAL_VALUE_TYPE d;

        d = TREE_REAL_CST (t);

        if (TREE_OVERFLOW (t))
          dummy->set_bool("overflow", true);

        if (REAL_VALUE_ISINF (d))
          dummy->set_string("value", REAL_VALUE_NEGATIVE(d) ? "-Inf" : "Inf");
        else if (REAL_VALUE_ISNAN(d))
          dummy->set_string("value", "NaN");
        else
          {
            char string[100];
            real_to_decimal(string, &d, sizeof (string), 0, 1);
            dummy->set_string("real_value", string);
          }
      }
      break;

    case FIXED_CST:
      {
        char string[100];
        fixed_to_decimal(string, TREE_FIXED_CST_PTR(t), sizeof (string));
        dummy->set_string("fixed_cst", string);
      }
      break;

    case STRING_CST:
        dummy->set_string("string_cst", TREE_STRING_POINTER(t));
        break;

    case VECTOR_CST:
      {
        unsigned int i;
        unsigned HOST_WIDE_INT nunits;

        if (!VECTOR_CST_NELTS (t).is_constant (&nunits))
          nunits = vector_cst_encoded_nelts (t);
        for (i = 0; i < nunits; i++)
          {
            holder->append(node_emit_json(VECTOR_CST_ELT (t, i)));
          }
        dummy->set("vector_cst", holder);
      }
      break;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      dummy->set("type_data", node_emit_json(TREE_TYPE (t)));

      if (TREE_CODE (t) == METHOD_TYPE)
        {
          dummy->set_bool("method_type", true);
          if (TYPE_METHOD_BASETYPE (t))
            dummy->set("basetype", node_emit_json (TYPE_NAME (TYPE_METHOD_BASETYPE(t))));
          else 
            dummy->set_string("basetype", "null method basetype");
        }
      if (TYPE_IDENTIFIER (t))
        dummy->set("type_identifier", node_emit_json (TYPE_NAME(t)));
      else if ( TYPE_NAME (t) && DECL_NAME (TYPE_NAME (t)) )
        decl_node_add_json( TYPE_NAME(t), dummy);
      else //TDF_NOUID
        {
          char* buff;
          buff = new char ();
          print_hex(TYPE_UID(t), buff);
   	  dummy->set_string("uid", buff);
        }
      dummy->set("function_decl", function_decl_emit_json(t));
      break;


    case FUNCTION_DECL:
    case CONST_DECL:
      decl_node_add_json(t, dummy);
      break;
    case LABEL_DECL:
      if (DECL_NAME (t))
        decl_node_add_json(t, dummy);
      else if (LABEL_DECL_UID (t) != -1)
        dummy->set_integer("LabelDeclUID", LABEL_DECL_UID (t));
      else 
        dummy->set_integer("DeclUID", DECL_UID(t));
      break;

    case TYPE_DECL:
      if (DECL_IS_UNDECLARED_BUILTIN (t)) //parity w/ dump_generic_node
        break;
      if (DECL_NAME (t))
        decl_node_add_json(t, dummy);
      else if (TYPE_NAME (TREE_TYPE (t)) != t)
        {
          dummy->set(TREE_CODE (TREE_TYPE (t)) == UNION_TYPE ? "union"
                                                             : "struct",
                     node_emit_json( TREE_TYPE (t)));
        }
      else
        dummy->set_bool("anon_type_decl", true);
      break;

    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case DEBUG_EXPR_DECL:
    case NAMESPACE_DECL:
    case NAMELIST_DECL:
      decl_node_add_json(t, dummy);
      break;
      
    case RESULT_DECL:
      dummy->set_bool("retval", true);
      break;

    case COMPONENT_REF:
      op0 = TREE_OPERAND (t, 0);
      op1 = TREE_OPERAND (t, 1);
      //Check if the following is okay later
      if (op0
          && (TREE_CODE (op0) == INDIRECT_REF
              || (TREE_CODE (op0) == MEM_REF
                  && TREE_CODE (TREE_OPERAND (op0, 0)) != ADDR_EXPR
                  && integer_zerop (TREE_OPERAND (op0, 1))
                  //We want to be explicit about Integer_CSTs
                  && TREE_CODE (TREE_OPERAND (op0, 0)) != INTEGER_CST
                  // To play nice with SSA
                  && TREE_TYPE (TREE_OPERAND (op0, 0)) != NULL_TREE
                  // I don't understand what the qualm is here
                  && (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 0)))
                      == (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 1)))))
                  && (TYPE_MODE (TREE_TYPE (TREE_OPERAND (op0, 0)))
                      == (TYPE_MODE (TREE_TYPE (TREE_OPERAND (op0, 1)))))
                  && (TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (op0, 0)))
                      == (TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (op0, 1)))))
                  // Understand this later too
                  && (TYPE_MAIN_VARIANT (TREE_TYPE (op0))
                      == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 1)))))
                  && MR_DEPENDENCE_CLIQUE (op0) == 0)))
        {
          //
          op0 = TREE_OPERAND (op0, 0);
        }
      dummy->set("expr", node_emit_json(op0));
      dummy->set("field", node_emit_json(op1));
      if (DECL_P (op1))
        if (tree off = component_ref_field_offset(t))
          if (TREE_CODE (off) != INTEGER_CST)
            {
              dummy->set("offset", node_emit_json(off));
            }
    break;

    case BIT_FIELD_REF:
      dummy->set_string("tree_code", "bit_field_ref");
      dummy->set("expr",
                 node_emit_json( TREE_OPERAND (t, 0)));
      dummy->set("bits_ref",
                 node_emit_json( TREE_OPERAND (t, 1)));
      dummy->set("bits_first_pos",
                 node_emit_json( TREE_OPERAND (t, 2)));
      break;

    case BIT_INSERT_EXPR:
      dummy->set_string("tree_code", "bit_insert_expr");
      dummy->set("container",
                 node_emit_json( TREE_OPERAND (t, 0)));
      dummy->set("replacement",
                 node_emit_json( TREE_OPERAND (t, 1)));
      dummy->set("constant_bit_pos",
                 node_emit_json( TREE_OPERAND (t, 2)));
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      op0 = TREE_OPERAND (t, 0);
      dummy->set("array",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("index",
                 node_emit_json (TREE_OPERAND (t, 1)));
      if (TREE_OPERAND(t, 2))
        dummy->set("type_min_val", 
                   node_emit_json (TREE_OPERAND (t, 2)));
      if (TREE_OPERAND(t, 3))
        dummy->set("element_size", 
                   node_emit_json (TREE_OPERAND (t, 3)));
      break;
  
    case OMP_ARRAY_SECTION:
      dummy->set("op0",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("op1",
                 node_emit_json (TREE_OPERAND (t, 1)));
      dummy->set("op2",
                 node_emit_json (TREE_OPERAND (t, 2)));
      break;

    case CONSTRUCTOR:
      {
        unsigned HOST_WIDE_INT ix;
        tree field, val;
        bool is_struct_init = false;
        bool is_array_init = false;
        widest_int curidx;

        if (TREE_CLOBBER_P (t))
          switch (CLOBBER_KIND(t))
            {
          case CLOBBER_STORAGE_BEGIN:
              dummy->set_string("CLOBBER", "storage_begin");
              break;
            case CLOBBER_STORAGE_END:
              dummy->set_bool("CLOBBER", "storage_end");
              break;
            case CLOBBER_OBJECT_BEGIN:
              dummy->set_bool("CLOBBER", "object_begin");
              break;
            case CLOBBER_OBJECT_END:
              dummy->set_bool("CLOBBER", "object_end");
              break;
            default:
              break;
          }
        else if (TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE
                 || TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
          is_struct_init = true;
        else if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE
                 && TYPE_DOMAIN (TREE_TYPE (t))
                 && TREE_CODE (TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (t))))
                    == INTEGER_CST)
          {
            tree minv = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (t)));
            is_array_init = true;
            curidx = wi::to_widest (minv);
          }
        FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), ix, field, val)
          {
            json::object* cst_elt;
            json::object* _val_json;
            cst_elt = new json::object ();
            cst_elt->set_integer("", ix); //fix
            if (field)
              {
                if (is_struct_init)
                  cst_elt->set("field", node_emit_json(field));
                else if (is_array_init 
                         && (TREE_CODE (field) != INTEGER_CST
                             || curidx != wi::to_widest (field)))
                  {
                    json::array* _array_init_json;
                    if (TREE_CODE (field) == RANGE_EXPR)
                      {
                        _array_init_json = new json::array ();
                        _array_init_json->append( node_emit_json( TREE_OPERAND( field, 0)));
                        _array_init_json->append( node_emit_json( TREE_OPERAND( field, 1)));
                        cst_elt->set("field", _array_init_json);
                      }
                    else 
                      cst_elt->set ("field", node_emit_json (field));
                  }
              }
            if (is_array_init)
              curidx += 1;
            if (val && TREE_CODE (val) == ADDR_EXPR)
              if (TREE_CODE (TREE_OPERAND (val, 0)) == FUNCTION_DECL)
                val = TREE_OPERAND (val, 0);
            if (val && TREE_CODE (val) == FUNCTION_DECL)
              {
                _val_json = new json::object ();
                decl_node_add_json (val, _val_json);
                cst_elt->set("val", _val_json);
              }
            else
              cst_elt->set("val", node_emit_json(val));

            if (TREE_CODE (field) == INTEGER_CST)
              curidx = wi::to_widest (field);
            holder->append(cst_elt);
          }
        dummy->set("ctor_elts", holder);
      }
    break;

   case COMPOUND_EXPR:
     {
        tree *tp;
        holder->append( node_emit_json( TREE_OPERAND (t, 0)));

        for (tp = &TREE_OPERAND (t, 1);
             TREE_CODE (*tp) == COMPOUND_EXPR;
             tp = &TREE_OPERAND (*tp, 1))
          {
          holder->append( node_emit_json ( TREE_OPERAND (*tp, 0)));
          }

        dummy->set("compound_expr", holder);
      }
      break;

    case STATEMENT_LIST:
      {
        tree_stmt_iterator si;

        for (si =  tsi_start (t); !tsi_end_p (si); tsi_next (&si))
          {
            holder->append( node_emit_json (tsi_stmt (si)));
          }
        dummy->set("statement_list", holder);
      }
      break;

    case MODIFY_EXPR:
    case INIT_EXPR:
      dummy->set("op0",
                 node_emit_json( TREE_OPERAND (t, 0)));
      dummy->set("op1",
                 node_emit_json( TREE_OPERAND (t, 1)));
      break;

    case TARGET_EXPR:
      dummy->set("slot", node_emit_json (TARGET_EXPR_SLOT(t)));
      dummy->set("initial", node_emit_json (TARGET_EXPR_INITIAL(t)));
      break;

    case DECL_EXPR:
      decl_node_add_json(DECL_EXPR_DECL(t), dummy);
      break;

    case COND_EXPR:
      dummy->set("if",
                 node_emit_json (TREE_OPERAND (t, 0)));
      if (COND_EXPR_THEN(t))
      {
        dummy->set("then",
                   node_emit_json (TREE_OPERAND (t, 1)));
      }
      if (COND_EXPR_ELSE(t))
      {
        dummy->set("else",
                 node_emit_json (TREE_OPERAND (t, 2)));
      }
      break;

    case BIND_EXPR:
      if (BIND_EXPR_VARS (t))
        {
          for (op0 = BIND_EXPR_VARS (t); op0; op0 = DECL_CHAIN (op0))
            {
              holder->append(node_emit_json (op0));
            }
          dummy->set("bind_expr_vars", holder);
        }
      dummy->set("bind_expr_body", 
                 node_emit_json(BIND_EXPR_BODY(t)));
      break;

    case CALL_EXPR:
      {
        if (CALL_EXPR_FN (t) != NULL_TREE)
          call_name_add_json (CALL_EXPR_FN(t), dummy);
        else
          dummy->set_string("internal_fn", internal_fn_name (CALL_EXPR_IFN (t)));
        dummy->set_bool("return_slot_optimization", CALL_EXPR_RETURN_SLOT_OPT(t));
        dummy->set_bool("tail_call", CALL_EXPR_TAILCALL(t));

        tree arg;
        call_expr_arg_iterator iter;
        bool call;
        FOR_EACH_CALL_EXPR_ARG(arg, iter, t)
          {
            call = true;
            holder->append(node_emit_json(arg));
          }
        if (call)
          dummy->set("call_expr_arg", holder);

        if (CALL_EXPR_VA_ARG_PACK (t))
          dummy->set_bool("__builtin_va_arg_pack", true);
        
        op1 = CALL_EXPR_STATIC_CHAIN (t);
        if (op1)
          dummy->set("static_chain", node_emit_json(op1));
      }
      break;
    case WITH_CLEANUP_EXPR:
      break;
    case CLEANUP_POINT_EXPR:
      dummy->set("cleanup_point", node_emit_json (TREE_OPERAND(t, 0)));
      break;
    case PLACEHOLDER_EXPR:
      dummy->set("placeholder_expr", node_emit_json (TREE_OPERAND(t, 0)));
      break;

    /* Binary operations */
    case WIDEN_SUM_EXPR:
    case WIDEN_MULT_EXPR:
    case MULT_EXPR:
    case MULT_HIGHPART_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case POINTER_DIFF_EXPR:
    case MINUS_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case WIDEN_LSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
      {
        const char* c = op_symbol_code(TREE_CODE(t), TDF_NONE); //ugly
        op0 = TREE_OPERAND(t, 0);
        op1 = TREE_OPERAND(t, 1);
        
        dummy->set_string("bin_operator", c);
        holder->append(node_emit_json(op0));
        holder->append(node_emit_json(op1));
        dummy->set("operands", holder);
      }
      break;

    case ADDR_EXPR:
      //TDF_GIMPLE_VAL
      dummy->set("_Literal", node_emit_json( TREE_TYPE (t)));
      /* FALLTHROUGH */
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case INDIRECT_REF:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      {
        const char * c = op_symbol_code(code, TDF_NONE);
        dummy->set(c, node_emit_json (TREE_OPERAND (t, 0)));
      }
      break;

    case MIN_EXPR:
      holder->append (node_emit_json (TREE_OPERAND (t,0)));
      holder->append (node_emit_json (TREE_OPERAND (t,1)));
      dummy->set("min_expr", holder);
      break;

    case MAX_EXPR:
      holder->append (node_emit_json (TREE_OPERAND (t,0)));
      holder->append (node_emit_json (TREE_OPERAND (t,1)));
      dummy->set("max_expr", holder);
      break;

    case ABS_EXPR:
      holder->append (node_emit_json (TREE_OPERAND (t,0)));
      dummy->set("abs_expr", holder);
      break;

    case ABSU_EXPR:
      holder->append (node_emit_json (TREE_OPERAND (t,0)));
      dummy->set("absu_expr", holder);
      break;

    case RANGE_EXPR:
      break;

    case ADDR_SPACE_CONVERT_EXPR:
    case FIXED_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT: 
      type = TREE_TYPE (t);
      op0 = TREE_OPERAND (t, 0);
      if (type != TREE_TYPE(op0))
        {
          dummy->set ("type", node_emit_json (type));
        }
      dummy->set ("operand", node_emit_json (op0));
      break;

    case VIEW_CONVERT_EXPR:
      holder->append (node_emit_json (TREE_TYPE(t)));
      holder->append (node_emit_json (TREE_OPERAND(t, 0)));
      dummy->set("view_convert_expr", holder);
      break;

    case PAREN_EXPR:
      dummy->set("paren_expr",
                 node_emit_json (TREE_OPERAND (t,0)));
      break;

    case NON_LVALUE_EXPR:
      dummy->set("non_lvalue_expr",
                 node_emit_json (TREE_OPERAND (t,0)));
      break;

    case SAVE_EXPR:
      dummy->set("save_expr",
                 node_emit_json (TREE_OPERAND (t,0)));
      break;

    case COMPLEX_EXPR:
      holder->append (node_emit_json (TREE_OPERAND (t,0)));
      holder->append (node_emit_json (TREE_OPERAND (t,1)));
      dummy->set("complex_expr", holder);
      break;

    case CONJ_EXPR:
      dummy->set("conj_expr",
                 node_emit_json (TREE_OPERAND (t,0)));
      break;

    case REALPART_EXPR:
      dummy->set("realpart_expr",
                 node_emit_json (TREE_OPERAND (t,0)));
      break;

    case IMAGPART_EXPR:
      dummy->set("imagpart_expr",
                 node_emit_json (TREE_OPERAND (t,0)));
      break;

    case VA_ARG_EXPR:
      dummy->set("va_arg_expr",
                 node_emit_json (TREE_OPERAND (t,0)));
      break;

    case TRY_FINALLY_EXPR:
    case TRY_CATCH_EXPR:
      {
        tree _t;
        dummy->set("try",
                   node_emit_json (TREE_OPERAND (t, 0)));
        if (TREE_CODE (t) == TRY_CATCH_EXPR)
          {
            _t = TREE_OPERAND(t, 1);
            dummy->set("catch",
                        node_emit_json (_t));
          }
        else
          {
            gcc_assert (TREE_CODE (t) == TRY_FINALLY_EXPR);
            _t = TREE_OPERAND(t, 1);
            if (TREE_CODE (t) == EH_ELSE_EXPR)
              {
                _t = TREE_OPERAND (_t, 0);
                dummy->set("finally",
                           node_emit_json (_t));
                _t = TREE_OPERAND(_t, 1);
                dummy->set("else",
                           node_emit_json (_t));
              }
            else
              {
              dummy->set("finally",
                         node_emit_json(_t));
              }
          }
      }
      break;

    case CATCH_EXPR:
      dummy->set("catch_types", node_emit_json(CATCH_TYPES(t)));
      dummy->set("catch_body", node_emit_json(CATCH_BODY(t)));
      break;

    case EH_FILTER_EXPR:
      dummy->set("eh_filter_types", node_emit_json(EH_FILTER_TYPES(t)));
      dummy->set("eh_filter_failure", node_emit_json(EH_FILTER_FAILURE(t)));
      break;

    case LABEL_EXPR:
      decl_node_add_json (TREE_OPERAND (t, 0), dummy);
      break;

    case LOOP_EXPR:
      dummy->set("while (1)", node_emit_json (LOOP_EXPR_BODY (t)));
      break;

    case PREDICT_EXPR:
      if (PREDICT_EXPR_OUTCOME (t))
        dummy->set_string("likely by",
                          predictor_name (PREDICT_EXPR_PREDICTOR (t)));
      else
        dummy->set_string("unlikely by",
                          predictor_name (PREDICT_EXPR_PREDICTOR (t)));
      break;
    case ANNOTATE_EXPR:
      {
      switch ((enum annot_expr_kind) TREE_INT_CST_LOW (TREE_OPERAND(t,1)))
        { //Is this expensive? ask later
          case annot_expr_ivdep_kind:
            dummy->set("ivdep", node_emit_json(TREE_OPERAND (t, 0)));
            break;
          case annot_expr_unroll_kind:
            dummy->set("unroll", node_emit_json(TREE_OPERAND (t, 0)));
            break;
          case annot_expr_no_vector_kind:
            dummy->set("no-vector", node_emit_json(TREE_OPERAND (t, 0)));
            break;
          case annot_expr_vector_kind:
            dummy->set("vector", node_emit_json(TREE_OPERAND (t, 0)));
            break;
          case annot_expr_parallel_kind:
            dummy->set("parallel", node_emit_json(TREE_OPERAND (t, 0)));
            break;
          case annot_expr_maybe_infinite_kind:
            dummy->set("maybe_infinite", node_emit_json(TREE_OPERAND (t, 0)));
            break;
          default:
            gcc_unreachable();
        }
      }
      break;

    case RETURN_EXPR:
      {
        op0 = TREE_OPERAND (t, 0);
        if (op0)
          {
            if (TREE_CODE (op0) == MODIFY_EXPR)
              dummy->set("return_expr", node_emit_json (TREE_OPERAND (op0, 1)));
            else
              dummy->set("return_expr", node_emit_json (op0));
          }
      }
      break;

    case EXIT_EXPR:
      dummy->set("exit_if", node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case SWITCH_EXPR:
      dummy->set("switch_cond", node_emit_json(SWITCH_COND(t)));
      dummy->set("switch_body", node_emit_json(SWITCH_BODY(t)));
      break;

    case GOTO_EXPR:
      op0 = GOTO_DESTINATION (t);
      dummy->set("goto", node_emit_json(op0));
      break;

    case ASM_EXPR:
      dummy->set("asm_string", node_emit_json (ASM_STRING (t)));
      dummy->set("asm_outputs", node_emit_json (ASM_OUTPUTS (t)));
      dummy->set("asm_inputs", node_emit_json (ASM_INPUTS (t)));
      if (ASM_CLOBBERS (t))
        dummy->set("asm_clobbers", node_emit_json (ASM_CLOBBERS (t)));
      break;

    case CASE_LABEL_EXPR:
      if (CASE_LOW(t) && CASE_HIGH(t))
        {
          dummy->set ("case_low", node_emit_json (CASE_LOW(t)));
          dummy->set ("case_high", node_emit_json (CASE_HIGH(t)));
        }
      else if (CASE_LOW(t))
        dummy->set ("case", node_emit_json (CASE_LOW(t)));
      else
        dummy->set_string("case", "default");
      break;

    // check later - okay?
    case OBJ_TYPE_REF:
      dummy->set("obj_type_ref_expr",
                 node_emit_json(OBJ_TYPE_REF_EXPR(t)));
      dummy->set("obj_type_ref_object",
                 node_emit_json(OBJ_TYPE_REF_OBJECT(t)));
      dummy->set("obj_type_ref_token",
                 node_emit_json(OBJ_TYPE_REF_TOKEN(t)));
      break;

    case SSA_NAME: //check later- augment with more accesssors?
      {
      if (SSA_NAME_IDENTIFIER (t))
        dummy->set("ssa_name_identifier",
                   node_emit_json (SSA_NAME_IDENTIFIER (t)));
      if (SSA_NAME_IS_DEFAULT_DEF (t))
        dummy->set_bool("ssa_default_def", true);
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (t))
	dummy->set_bool("abnormal_phi", true);
      }
      break;

    case WITH_SIZE_EXPR:
      dummy->set("expr", node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("size", node_emit_json (TREE_OPERAND (t, 1)));
      break;

    case SCEV_KNOWN:
    case SCEV_NOT_KNOWN:
      break;

    case POLYNOMIAL_CHREC:
      dummy->set_integer("chrec_var", CHREC_VARIABLE(t));
      dummy->set("chrec_left", node_emit_json (CHREC_LEFT(t)));
      dummy->set("chrec_right", node_emit_json (CHREC_RIGHT(t)));
      dummy->set_bool("chrec_nowrap", CHREC_NOWRAP(t));
      break;

    case REALIGN_LOAD_EXPR:
      dummy->set("input_0", node_emit_json(TREE_OPERAND (t, 0)));
      dummy->set("input_1", node_emit_json(TREE_OPERAND (t, 1)));
      dummy->set("offset", node_emit_json(TREE_OPERAND (t, 2)));
      break;

    case VEC_COND_EXPR:
      dummy->set("if",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("then",
                 node_emit_json (TREE_OPERAND (t, 1)));
      dummy->set("else",
                 node_emit_json (TREE_OPERAND (t, 2)));
      break;

    case VEC_PERM_EXPR:
      dummy->set("v0",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("v1",
                 node_emit_json (TREE_OPERAND (t, 1)));
      dummy->set("mask",
                 node_emit_json (TREE_OPERAND (t, 2)));
      break;

    case DOT_PROD_EXPR: //check later
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("arg2",
                 node_emit_json (TREE_OPERAND (t, 1)));
      dummy->set("arg3",
                 node_emit_json (TREE_OPERAND (t, 2)));
      break;

    case WIDEN_MULT_PLUS_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("arg2",
                 node_emit_json (TREE_OPERAND (t, 1)));
      dummy->set("arg3",
                 node_emit_json (TREE_OPERAND (t, 2)));
      break;

    case WIDEN_MULT_MINUS_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("arg2",
                 node_emit_json (TREE_OPERAND (t, 1)));
      dummy->set("arg3",
                 node_emit_json (TREE_OPERAND (t, 2)));
      break;

//    case VEC_SERIES_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
      //check later - why do we use this trick only here?
      dummy->set ("arg1", 
                  node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set ("arg2",
                  node_emit_json (TREE_OPERAND (t, 1)));
      break;

    case VEC_DUPLICATE_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case VEC_UNPACK_HI_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case VEC_UNPACK_LO_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case VEC_UNPACK_FLOAT_HI_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case VEC_UNPACK_FLOAT_LO_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case VEC_UNPACK_FIX_TRUNC_HI_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case VEC_UNPACK_FIX_TRUNC_LO_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      break;

    case VEC_PACK_TRUNC_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("arg2",
                 node_emit_json (TREE_OPERAND (t, 1)));
      break;

    case VEC_PACK_SAT_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("arg2",
                 node_emit_json (TREE_OPERAND (t, 1)));
      break;

    case VEC_PACK_FIX_TRUNC_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("arg2",
                 node_emit_json (TREE_OPERAND (t, 1)));
      break;

    case VEC_PACK_FLOAT_EXPR:
      dummy->set("arg1",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("arg2",
                 node_emit_json (TREE_OPERAND (t, 1)));
      break;
    /*OACC and OMP */
    case OACC_PARALLEL:
      goto dump_omp_clauses_body;

    case OACC_KERNELS:
      goto dump_omp_clauses_body;

    case OACC_SERIAL:
      goto dump_omp_clauses_body;

    case OACC_DATA:
      goto dump_omp_clauses_body;

    case OACC_HOST_DATA:
      dummy->set("oacc_host_data_clauses", 
                 omp_clause_emit_json (OACC_HOST_DATA_CLAUSES(t)));
      dummy->set("oacc_host_data_body", 
                 omp_clause_emit_json (OACC_HOST_DATA_BODY(t)));
      break;

    case OACC_DECLARE:
      dummy->set("oacc_declare_clauses", 
                 omp_clause_emit_json (OACC_DECLARE_CLAUSES(t)));
      break;

    case OACC_UPDATE:
      dummy->set("oacc_update_clauses",
                 omp_clause_emit_json (OACC_UPDATE_CLAUSES(t)));
      break;

    case OACC_ENTER_DATA:
      dummy->set("oacc_enter_data_clauses",
                 omp_clause_emit_json (OACC_ENTER_DATA_CLAUSES(t)));
      break;
      
    case OACC_EXIT_DATA:
      dummy->set("oacc_exit_data_clauses",
                 omp_clause_emit_json (OACC_EXIT_DATA_CLAUSES(t)));
      break;

    case OACC_CACHE:
      dummy->set("oacc_cache_clauses",
                 omp_clause_emit_json (OACC_CACHE_CLAUSES(t)));
      break;

    case OMP_PARALLEL:
      dummy->set("omp_parallel_body",
                 node_emit_json (OMP_PARALLEL_BODY(t)));
      dummy->set("omp_parallel_clauses",
                 omp_clause_emit_json (OMP_PARALLEL_CLAUSES(t)));
      break;

    case OMP_TASK:
      {
      if (OMP_TASK_BODY (t))
        {
          dummy->set_bool("omp_task", true);
          dummy->set("omp_task_body",
                     node_emit_json(OMP_TASK_BODY (t)));
          dummy->set("omp_task_clauses",
                     omp_clause_emit_json (OMP_TASK_CLAUSES(t)));
        } else {
          dummy->set_bool("omp_taskwait", true);
          dummy->set("omp_taskwait_clauses",
                     omp_clause_emit_json (OMP_TASK_CLAUSES(t)));
        }
        break;
      }

    dump_omp_clauses_body:
      dummy->set("omp_clauses", omp_clause_emit_json (OMP_CLAUSES (t)));
      goto dump_omp_body;

    dump_omp_body:
      dummy->set("omp_body", node_emit_json (OMP_BODY(t)));
      dummy->set_bool("is_expr", false);
      break;

    case OMP_FOR:
      goto dump_omp_loop;
      
    case OMP_SIMD:
      goto dump_omp_loop;

    case OMP_DISTRIBUTE:
      goto dump_omp_loop;

    case OMP_TASKLOOP:
      goto dump_omp_loop;

    case OMP_LOOP:
      goto dump_omp_loop;

    case OMP_TILE:
      goto dump_omp_loop;

    case OMP_UNROLL:
      goto dump_omp_loop;

    case OACC_LOOP:
      goto dump_omp_loop;
      
    dump_omp_loop:
      dummy->set("omp_for_body",
                 node_emit_json(OMP_FOR_BODY (t)));
      dummy->set("omp_for_clauses",
                 omp_clause_emit_json (OMP_FOR_CLAUSES(t)));
      dummy->set("omp_for_init",
                 node_emit_json(OMP_FOR_INIT (t)));
      dummy->set("omp_for_incr",
                 node_emit_json(OMP_FOR_INCR (t)));
      dummy->set("omp_for_pre_body",
                 node_emit_json(OMP_FOR_PRE_BODY (t)));
      dummy->set("omp_for_orig_decls",
                 node_emit_json(OMP_FOR_ORIG_DECLS (t)));
      break;

    case OMP_TEAMS:
      dummy->set("omp_teams_body",
                 omp_clause_emit_json (OMP_TEAMS_BODY(t)));
      dummy->set("omp_teams_clauses",
                 omp_clause_emit_json (OMP_TEAMS_CLAUSES(t)));
      break;

    case OMP_TARGET_DATA:
      dummy->set("omp_target_data_body",
                 omp_clause_emit_json (OMP_TARGET_DATA_BODY(t)));
      dummy->set("omp_target_data_clauses",
                 omp_clause_emit_json (OMP_TARGET_DATA_CLAUSES(t)));
      break;

    case OMP_TARGET_ENTER_DATA:
      dummy->set("omp_target_enter_data_clauses",
                 omp_clause_emit_json (OMP_TARGET_ENTER_DATA_CLAUSES(t)));
      break;

    case OMP_TARGET_EXIT_DATA:
      dummy->set("omp_target_exit_data_clauses",
                 omp_clause_emit_json (OMP_TARGET_EXIT_DATA_CLAUSES(t)));
      break;

    case OMP_TARGET:
      dummy->set("omp_target_body",
                 omp_clause_emit_json (OMP_TARGET_BODY(t)));
      dummy->set("omp_target_clauses",
                 omp_clause_emit_json (OMP_TARGET_CLAUSES(t)));
      break;

    case OMP_TARGET_UPDATE:
      dummy->set("omp_target_update_clauses",
                 omp_clause_emit_json (OMP_TARGET_UPDATE_CLAUSES(t)));
      break;

    case OMP_SECTIONS:
      dummy->set("omp_sections_body",
                 omp_clause_emit_json (OMP_SECTIONS_BODY(t)));
      dummy->set("omp_sections_clauses",
                 omp_clause_emit_json (OMP_SECTIONS_CLAUSES(t)));
      break;

    case OMP_SECTION:
      dummy->set("omp_section_body",
                 omp_clause_emit_json (OMP_SECTION_BODY(t)));
      break;

    case OMP_STRUCTURED_BLOCK:
      dummy->set("omp_structured_block_body",
                 omp_clause_emit_json (OMP_STRUCTURED_BLOCK_BODY(t)));
      break;

    case OMP_SCAN:
      dummy->set("omp_scan_body",
                 omp_clause_emit_json (OMP_SCAN_BODY(t)));
      dummy->set("omp_scan_clauses",
                 omp_clause_emit_json (OMP_SCAN_CLAUSES(t)));
      break;

    case OMP_MASTER:
      dummy->set("omp_master_body",
                 omp_clause_emit_json (OMP_MASTER_BODY(t)));
      break;

    case OMP_MASKED:
      dummy->set("omp_masked_body",
                 omp_clause_emit_json (OMP_MASKED_BODY(t)));
      dummy->set("omp_masked_clauses",
                 omp_clause_emit_json (OMP_MASKED_CLAUSES(t)));
      break;

    case OMP_TASKGROUP:
      dummy->set("omp_taskgroup_body",
                 omp_clause_emit_json (OMP_TASKGROUP_BODY(t)));
      dummy->set("omp_taskgroup_clauses",
                 omp_clause_emit_json (OMP_TASKGROUP_CLAUSES(t)));
      break;

    case OMP_ORDERED:
      dummy->set("omp_ordered_body",
                 omp_clause_emit_json (OMP_ORDERED_BODY(t)));
      dummy->set("omp_ordered_clauses",
                 omp_clause_emit_json (OMP_ORDERED_CLAUSES(t)));
      break;

    case OMP_CRITICAL:
      dummy->set("omp_masked_body",
                 omp_clause_emit_json (OMP_CRITICAL_BODY(t)));
      dummy->set("omp_masked_clauses",
                 omp_clause_emit_json (OMP_CRITICAL_CLAUSES(t)));
      dummy->set("omp_masked_name",
                 node_emit_json (OMP_CRITICAL_NAME(t)));
      break;

    case OMP_ATOMIC:
      if (OMP_ATOMIC_WEAK (t))
        dummy->set_bool("omp_atomic_weak", true);
      else
        dummy->set_bool("omp_atomic_weak", false);
      dummy->set("omp_atomic_memory_order",
                 omp_atomic_memory_order_emit_json (OMP_ATOMIC_MEMORY_ORDER(t)));
      dummy->set("op0",
                 node_emit_json (TREE_OPERAND(t, 0)));
      dummy->set("op1",
                 node_emit_json (TREE_OPERAND(t, 1)));
      break;

    case OMP_ATOMIC_READ:
      dummy->set("omp_atomic_memory_order",
                 omp_atomic_memory_order_emit_json (OMP_ATOMIC_MEMORY_ORDER(t)));
      dummy->set("op0",
                 node_emit_json (TREE_OPERAND(t, 0)));
      break;

    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
      if (OMP_ATOMIC_WEAK (t))
        dummy->set_bool("omp_atomic_capture_weak", true);
      else
        dummy->set_bool("omp_atomic_capture", true);
      dummy->set("omp_atomic_memory_order",
                 omp_atomic_memory_order_emit_json (OMP_ATOMIC_MEMORY_ORDER(t)));
      dummy->set("op0",
                 node_emit_json (TREE_OPERAND(t, 0)));
      dummy->set("op1",
                 node_emit_json (TREE_OPERAND(t, 1)));
      break;

    case OMP_SINGLE:
      dummy->set("omp_single_body",
                 omp_clause_emit_json (OMP_SINGLE_BODY(t)));
      dummy->set("omp_single_clauses",
                 omp_clause_emit_json (OMP_SINGLE_CLAUSES(t)));
      break;

    case OMP_SCOPE:
      dummy->set("omp_scope_body",
                 omp_clause_emit_json (OMP_SCOPE_BODY(t)));
      dummy->set("omp_scope_clauses",
                 omp_clause_emit_json (OMP_SCOPE_CLAUSES(t)));
      break;

    case OMP_CLAUSE:
      dummy->set("omp_clause",
                 omp_clause_emit_json(t));
      break;

    case TRANSACTION_EXPR:
      if (TRANSACTION_EXPR_OUTER (t))
	dummy->set_bool ("transaction_expr_outer", true);
      if (TRANSACTION_EXPR_RELAXED (t))
	dummy->set_bool ("transaction_expr_relaxed", true);
      dummy->set("omp_transaction_body", 
                 node_emit_json (TRANSACTION_EXPR_BODY(t)));
      break;

    case BLOCK:
      {
      tree iter;
      json::array *subblock, *chain, *vars, *fragment_chain, *nlv_holder;
      dummy->set_integer ("block #", BLOCK_NUMBER (t));
      if (BLOCK_SUPERCONTEXT (t))
        dummy->set("block_supercontext",
                   node_emit_json(BLOCK_SUPERCONTEXT (t)));
      if (BLOCK_SUBBLOCKS(t))
        {
          subblock = new json::array ();
          for (iter = BLOCK_SUBBLOCKS (t); iter; iter = BLOCK_CHAIN (t))
            subblock->append(node_emit_json(iter));
          dummy->set("block_subblocks", subblock);
        }
      if (BLOCK_CHAIN (t))
        {
          chain = new json::array ();
          for (iter = BLOCK_SUBBLOCKS (t); iter; iter = BLOCK_CHAIN (t))
              chain->append(node_emit_json(iter));
          dummy->set("block_chain", chain);
        
        }
      if (BLOCK_VARS (t))
        {
          vars = new json::array ();
          for (iter = BLOCK_VARS (t); iter; iter = TREE_CHAIN (t))
              vars->append(node_emit_json(iter));
          dummy->set("block_vars", vars);
        }
      if (vec_safe_length (BLOCK_NONLOCALIZED_VARS (t)) > 0)
        {
          unsigned i;
          vec<tree, va_gc> *nlv = BLOCK_NONLOCALIZED_VARS (t);

          nlv_holder = new json::array ();

          FOR_EACH_VEC_ELT (*nlv, i, t)
            {
              nlv_holder->append(node_emit_json(t));
            }
          dummy->set("block_nonlocalized_vars", nlv_holder);
        }
      if (BLOCK_ABSTRACT_ORIGIN (t))
        dummy->set("block_abstract_origin",
                   node_emit_json (BLOCK_ABSTRACT_ORIGIN (t)));
      if (BLOCK_FRAGMENT_ORIGIN (t))
        dummy->set("block_fragment_origin",
                   node_emit_json (BLOCK_FRAGMENT_ORIGIN (t)));

      if (BLOCK_FRAGMENT_CHAIN (t))
        {
          fragment_chain = new json::array ();
          for (iter = BLOCK_FRAGMENT_CHAIN (t); iter; iter = BLOCK_FRAGMENT_CHAIN (t))
              fragment_chain->append(node_emit_json(iter));
          dummy->set("block_fragment_chain", fragment_chain);
        }
      }
      break;

    case DEBUG_BEGIN_STMT:
      dummy->set_bool("debug_begin", true);
      break;
    default:
      dummy->set_bool("unsupported code", true);
      dummy->set_string("fallthrough", get_tree_code_name(code));
      break;
  }
  return dummy;
}

/* Dump the next node in the queue.  */

static void
dequeue_and_dump (dump_info_p di)
{
  dump_queue_p dq;
  splay_tree_node stn;
//  dump_node_info_p dni;
  tree t;
  json::object* dummy;

  dummy = new json::object ();

  /* Get the next node from the queue.  */
  dq = di->queue;
  stn = dq->node;
  t = (tree) stn->key;
//  dni = (dump_node_info_p) stn->value;

  /* Remove the node from the queue, and put it on the free list.  */
  di->queue = dq->next;
  if (!di->queue)
    di->queue_end = 0;
  dq->next = di->free_list;
  di->free_list = dq;

  dummy = node_emit_json(t);
  di->json_dump->append(dummy);
}

/* Dump T, and all its children, on STREAM.  */

void
dump_node_json (const_tree t, dump_flags_t flags, FILE *stream)
{
  struct dump_info di;
  dump_queue_p dq;
  dump_queue_p next_dq;
  pretty_printer pp;
  /* Initialize the dump-information structure.  */
  di.stream = stream;
  di.index = 0;
  di.column = 0;
  di.queue = 0;
  di.queue_end = 0;
  di.free_list = 0;
  di.flags = flags;
  di.node = t;
  di.nodes = splay_tree_new (splay_tree_compare_pointers, 0,
			     splay_tree_delete_pointers);
  di.json_dump = new json::array ();
  /* Queue up the first node.  */
  queue (&di, t);

  /* Until the queue is empty, keep dumping nodes.  */
  while (di.queue)
    dequeue_and_dump (&di);

  di.json_dump->dump(stream, true);
  fputs("\n", stream);
  /* Now, clean up.  */
  for (dq = di.free_list; dq; dq = next_dq)
    {
      next_dq = dq->next;
      free (dq);
    }
  splay_tree_delete (di.nodes);
}

DEBUG_FUNCTION void
debug_tree_json (tree t)
{
  json::object* _x = node_emit_json(t);
  _x->dump(stderr, true);
  fprintf(stderr, "\n");
}
