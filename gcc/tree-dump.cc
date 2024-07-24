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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pretty-print.h"
#include "tree-dump.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "dumpfile.h"
#include "json.h"
#include "tm.h"

static unsigned int queue (dump_info_p, const_tree, int);
static void dump_index (dump_info_p, unsigned int);
static void dequeue_and_dump (dump_info_p);
static void dump_new_line (dump_info_p);
static void dump_maybe_newline (dump_info_p);

/* Add T to the end of the queue of nodes to dump.  Returns the index
   assigned to T.  */

static unsigned int
queue (dump_info_p di, const_tree t, int flags)
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
  dni->binfo_p = ((flags & DUMP_BINFO) != 0);
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

static void
dump_index (dump_info_p di, unsigned int index, bool parent)
{
  json::object* node;
  node = new json::object ();
  //DANGER, type cast implicit
    node->set_integer("index", index);
  if (parent == true)
    node->set_bool("has child", true);
  else
    node->set_bool("has child", false);
  di->tree_json->append(node);
}

/* If T has not already been output, queue it for subsequent output.
   FIELD is a string to print before printing the index.  Then, the
   index of T is printed.  */

void
queue_and_dump_index (dump_info_p di, const char *field, const_tree t, int flags)
{
  unsigned int index;
  splay_tree_node n;
  json::array* child;
  json::object* dummy;
  /* If there's no node, just return.  This makes for fewer checks in
     our callers.  */
  if (!t)
    return;

  /* See if we've already queued or dumped this node.  */
  n = splay_tree_lookup (di->nodes, (splay_tree_key) t);
  if (n) {
    index = ((dump_node_info_p) n->value)->index;
    //DO RECURSION STUFF HERE
  } else {
    /* If we haven't, add it to the queue.  */
    index = queue (di, t, flags);
  }
  /* Print the index of the node.  */
  if (n) {
    child = new json::array ();
    dummy = new json::object ();
    dummy->set_integer("index", index);
    //This is hacky and should be fixed
    dummy->set_bool(field, true);
    child->append(dummy);
    di->tree_json->append(child);
  } else {
    dump_index (di, index, false);
  }
}

/* Dump the type of T.  */

void
queue_and_dump_type (dump_info_p di, const_tree t)
{
  queue_and_dump_index (di, "type", TREE_TYPE (t), DUMP_NONE);
}

/* Dump column control */
#define SOL_COLUMN 25		/* Start of line column.  */
#define EOL_COLUMN 55		/* End of line column.  */
#define COLUMN_ALIGNMENT 15	/* Alignment.  */

/* Insert a new line in the dump output, and indent to an appropriate
   place to start printing more fields.  */

static void
dump_new_line (dump_info_p di)
{
  fprintf (di->stream, "\n%*s", SOL_COLUMN, "");
  di->column = SOL_COLUMN;
}

/* If necessary, insert a new line.  */

static void
dump_maybe_newline (dump_info_p di)
{
  int extra;

  /* See if we need a new line.  */
  if (di->column > EOL_COLUMN)
    dump_new_line (di);
  /* See if we need any padding.  */
  else if ((extra = (di->column - SOL_COLUMN) % COLUMN_ALIGNMENT) != 0)
    {
      fprintf (di->stream, "%*s", COLUMN_ALIGNMENT - extra, "");
      di->column += COLUMN_ALIGNMENT - extra;
    }
}

/* Dump pointer PTR using FIELD to identify it.  */

void
dump_pointer (dump_info_p di, const char *field, void *ptr)
{
  json::object* dummy;
  dummy = new json::object ();
  dummy->set_integer(field, (uintptr_t) ptr);
  di->tree_json->append(dummy);
}

/* Dump integer I using FIELD to identify it.  */

void
dump_int (dump_info_p di, const char *field, int i)
{
  json::object* dummy;
  dummy = new json::object ();
  dummy->set_integer(field, i);
  di->tree_json->append(dummy);
}

/* Dump the floating point value R, using FIELD to identify it.  */

static void
dump_real (dump_info_p di, const char *field, const REAL_VALUE_TYPE *r)
{
  json::object* dummy;
  char buf[32];
  dummy = new json::object ();
  real_to_decimal (buf, r, sizeof (buf), 0, true);
  dummy->set_string(field, buf);
  di->tree_json->append(dummy);
}

/* Dump the fixed-point value F, using FIELD to identify it.  */

static void
dump_fixed (dump_info_p di, const char *field, const FIXED_VALUE_TYPE *f)
{
  json::object* dummy;
  char buf[32];
  dummy = new json::object ();
  fixed_to_decimal (buf, f, sizeof (buf));
  dummy->set_string(field, buf);
  di->tree_json->append(dummy);
}


/* Dump the string S.  */
// CHECK LATER
void
dump_string (dump_info_p di, const char *string)
{
  dump_maybe_newline (di);
  fprintf (di->stream, "%-13s ", string);
  if (strlen (string) > 13)
    di->column += strlen (string) + 1;
  else
    di->column += 14;
}

/* Dump the string field S.  */

void
dump_string_field (dump_info_p di, const char *field, const char *string)
{
  json::object* dummy;
  dummy = new json::object ();
  dummy->set_string(field, string);
  di->tree_json->append(dummy);
}

/* Here we emit data in the generic tree without traversing the tree. base on tree-pretty-print.cc */

json::object* 
node_emit_json(tree t)
{
  json::object* dummy;
  json::array* holder;
  enum tree_code code;
  
  dummy = new json::object ();
  holder = new json::array ();

  if (EXPR_HAS_LOCATION(t))
    
  
  code = TREE_CODE(t);
  switch (code)
  {
    case ERROR_MARK:
      {
      dummy->set_bool("error_mark", true);
      }
    case IDENTIFIER_NODE:
      {
      //Error on this access - figure out whats going wrong later.
      //dummy->set_string("identifier", (IDENTIFIER_POINTER(t)));
      break;
      }
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
    {
      holder->append(node_emit_json(BINFO_TYPE(t)));
    }
    //Make sure this actually goes through all the elements - cf dump_generic_node
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

	if (quals & TYPE_QUAL_ATOMIC)
	  dummy->set_string("qual", "atomic");
	if (quals & TYPE_QUAL_CONST)
	  dummy->set_string("qual", "const");
	if (quals & TYPE_QUAL_VOLATILE)
	  dummy->set_string("qual", "volatile");
	if (quals & TYPE_QUAL_RESTRICT)
	  dummy->set_string("qual", "restrict");

	if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (t)))
	  {
	    dummy->set_integer("address space", TYPE_ADDR_SPACE(t));
	  }
	
	tclass = TREE_CODE_CLASS (TREE_CODE(t));

	if (tclass == tcc_declaration)
	  {
	  if (DECL_NAME (t))
	    dummy->set_string("decl", "TODO");
	  else
	    dummy->set_string("decl", "<unnamed type decl>");
	  }
	else if (tclass = tcc_type)
	  {
	  if (TYPE_NAME (t))
	  {  
	    if (TREE_CODE(TYPE_NAME (t)) == IDENTIFIER_NODE)
	      break;
	    else if (TREE_CODE (TYPE_NAME (t)) == TYPE_DECL
		       && DECL_NAME (TYPE_NAME (t)))
	      break; //same DECL_NAME sol
	    else // unnamed 
	      break;
	   }
    	else if (TREE_CODE (t) == VECTOR_TYPE)
	  {
    	  holder->append(node_emit_json(TREE_TYPE (t)));
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
	      {
	      
	      }
	  }
    	  
    	    
    	  }

      }
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:

    case OFFSET_TYPE:

    case MEM_REF:
    case TARGET_MEM_REF:

    case ARRAY_TYPE:

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:

    case LANG_TYPE:

    case INTEGER_CST:

    case POLY_INT_CST:

    case REAL_CST:

    case FIXED_CST:

    case STRING_CST:

    case VECTOR_CST:

    case FUNCTION_TYPE:
    case METHOD_TYPE:

    case FUNCTION_DECL:
    case CONST_DECL:

    case LABEL_DECL:

    case TYPE_DECL:

    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case DEBUG_EXPR_DECL:
    case NAMESPACE_DECL:
    case NAMELIST_DECL:

    case RESULT_DECL:

    case COMPONENT_REF:

    case BIT_FIELD_REF:

    case BIT_INSERT_EXPR:

    case ARRAY_REF:
//    case ARRANGE_ARRAY_REF:

    case OMP_ARRAY_SECTION:

    case CONSTRUCTOR:

    case COMPOUND_EXPR:

    case STATEMENT_LIST:

    case MODIFY_EXPR:
    case INIT_EXPR:

    case TARGET_EXPR:

    case DECL_EXPR:

    case COND_EXPR:

    case BIND_EXPR:

    case CALL_EXPR:

    case WITH_CLEANUP_EXPR:

    case CLEANUP_POINT_EXPR:

    case PLACEHOLDER_EXPR:

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
    
    case ADDR_EXPR:

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case INDIRECT_REF:

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:

    case MIN_EXPR:

    case MAX_EXPR:

    case ABS_EXPR:

    case ABSU_EXPR:

    case RANGE_EXPR:
    
    case ADDR_SPACE_CONVERT_EXPR:
    case FIXED_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:

    case VIEW_CONVERT_EXPR:

    case PAREN_EXPR:

    case NON_LVALUE_EXPR:

    case SAVE_EXPR:

    case COMPLEX_EXPR:

    case CONJ_EXPR:

    case REALPART_EXPR:

    case IMAGPART_EXPR:

    case VA_ARG_EXPR:

    case TRY_FINALLY_EXPR:
    case TRY_CATCH_EXPR:

    case CATCH_EXPR:
    
    case EH_FILTER_EXPR:

    case LABEL_EXPR:

    case LOOP_EXPR:

    case PREDICT_EXPR:

    case ANNOTATE_EXPR:

    case RETURN_EXPR:

    case EXIT_EXPR:

    case SWITCH_EXPR:

    case GOTO_EXPR:

    case ASM_EXPR:

    case CASE_LABEL_EXPR:

    case OBJ_TYPE_REF:

    case SSA_NAME:

    case WITH_SIZE_EXPR:

    case SCEV_KNOWN:

    case SCEV_NOT_KNOWN:

    case POLYNOMIAL_CHREC:

    case REALIGN_LOAD_EXPR:

    case VEC_COND_EXPR:

    case VEC_PERM_EXPR:

    case DOT_PROD_EXPR:

    case WIDEN_MULT_PLUS_EXPR:

    case WIDEN_MULT_MINUS_EXPR:

    /*OACC and OMP */
    case OACC_PARALLEL:

    case OACC_KERNELS:

    case OACC_SERIAL:

    case OACC_DATA:

    case OACC_HOST_DATA:

    case OACC_DECLARE:

    case OACC_UPDATE:

    case OACC_ENTER_DATA:

    case OACC_EXIT_DATA:

    case OACC_CACHE:

    case OMP_PARALLEL:

    case OMP_TASK:

    case OMP_FOR:

    case OMP_SIMD:

    case OMP_DISTRIBUTE:

    case OMP_TASKLOOP:

    case OMP_LOOP:

    case OMP_TILE:

    case OMP_UNROLL:

    case OACC_LOOP:

    case OMP_TEAMS:

    case OMP_TARGET_DATA:

    case OMP_TARGET_ENTER_DATA:

    case OMP_TARGET_EXIT_DATA:

    case OMP_TARGET:

    case OMP_TARGET_UPDATE:

    case OMP_SECTIONS:

    case OMP_SECTION:

    case OMP_STRUCTURED_BLOCK:

    case OMP_SCAN:

    case OMP_MASTER:

    case OMP_MASKED:

    case OMP_TASKGROUP:

    case OMP_ORDERED:

    case OMP_CRITICAL:

    case OMP_ATOMIC:

    case OMP_ATOMIC_READ:

    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:

    case OMP_SINGLE:

    case OMP_SCOPE:

    case OMP_CLAUSE:

    case TRANSACTION_EXPR:

    case VEC_SERIES_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:

    case VEC_DUPLICATE_EXPR:

    case VEC_UNPACK_HI_EXPR:

    case VEC_UNPACK_LO_EXPR:

    case VEC_UNPACK_FLOAT_HI_EXPR:

    case VEC_UNPACK_FLOAT_LO_EXPR:

    case VEC_UNPACK_FIX_TRUNC_HI_EXPR:

    case VEC_UNPACK_FIX_TRUNC_LO_EXPR:

    case VEC_PACK_TRUNC_EXPR:

    case VEC_PACK_SAT_EXPR:

    case VEC_PACK_FIX_TRUNC_EXPR:

    case VEC_PACK_FLOAT_EXPR:

    case BLOCK:

    case DEBUG_BEGIN_STMT:
      {
      dummy->set_bool("fallthrough", true);
      }
      break;
    default:
      break;
  }
  return dummy;
}

//json::array*
//traverse_tree_emit_json (dump_info_p di, tree t,)
//{
//  json::object* dummy;
//}

/* Dump the next node in the queue.  */

static void
dequeue_and_dump (dump_info_p di)
{
  dump_queue_p dq;
  splay_tree_node stn;
  dump_node_info_p dni;
  tree t;
  unsigned int index;
  enum tree_code code;
  enum tree_code_class code_class;
  const char* code_name;
  
  json::array* parent;
  json::array* child;
  json::object* dummy;

  parent = new json::array ();
  child = new json::array ();
  dummy = new json::object ();

  /* Get the next node from the queue.  */
  dq = di->queue;
  stn = dq->node;
  t = (tree) stn->key;
  dni = (dump_node_info_p) stn->value;
  index = dni->index;

  /* Remove the node from the queue, and put it on the free list.  */
  di->queue = dq->next;
  if (!di->queue)
    di->queue_end = 0;
  dq->next = di->free_list;
  di->free_list = dq;

  /* Print the node index.  */
  dummy->set_integer("index", index);
  dump_index (di, index, true);
  /* And the type of node this is.  */
  if (dni->binfo_p)
    code_name = "binfo";
  else
    code_name = get_tree_code_name (TREE_CODE (t));
  dummy->set_string("tree_code", code_name);
//  fprintf (di->stream, "%-16s ", code_name);
//  di->column = 25;

  /* Figure out what kind of node this is.  */
  code = TREE_CODE (t);
  code_class = TREE_CODE_CLASS (code);

  /* Although BINFOs are TREE_VECs, we dump them specially so as to be
     more informative.  */
  if (dni->binfo_p)
    {
      unsigned ix;
      tree base;
      vec<tree, va_gc> *accesses = BINFO_BASE_ACCESSES (t);

//      dummy->set_string("type", BINFO_TYPE (t));

      if (BINFO_VIRTUAL_P (t))
//        dummy->set_string("spec", "virt");
	dump_string_field (di, "spec", "virt");

//      dummy->set_integer("bases", BINFO_N_BASE_BINFOS (t))
      dump_int (di, "bases", BINFO_N_BASE_BINFOS (t));
      for (ix = 0; BINFO_BASE_ITERATE (t, ix, base); ix++)
	{
	  tree access = (accesses ? (*accesses)[ix] : access_public_node);
	  const char *string = NULL;

	  if (access == access_public_node)
	    string = "pub";
	  else if (access == access_protected_node)
	    string = "prot";
	  else if (access == access_private_node)
	    string = "priv";
	  else
	    gcc_unreachable ();

	  dummy->set_string("accs", string);
          //dump_string_field (di, "accs", string);
          //This recurses over base. Fix later
	  queue_and_dump_index (di, "binf", base, DUMP_BINFO);
	}

      goto done;
    }
  /* We can knock off a bunch of expression nodes in exactly the same
     way.  */
  if (IS_EXPR_CODE_CLASS (code_class))
    {
      /* If we're dumping children, dump them now.  */
      queue_and_dump_type (di, t);

      switch (code_class)
	{
	case tcc_unary:
	  dump_child ("op 0", TREE_OPERAND (t, 0));
	  break;

	case tcc_binary:
	case tcc_comparison:
	  dump_child ("op 0", TREE_OPERAND (t, 0));
	  dump_child ("op 1", TREE_OPERAND (t, 1));
	  break;

	case tcc_expression:
	case tcc_reference:
	case tcc_statement:
	case tcc_vl_exp:
	  /* These nodes are handled explicitly below.  */
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else if (DECL_P (t))
    {
      expanded_location xloc;
      /* All declarations have names.  */
      if (DECL_NAME (t))
	dump_child ("name", DECL_NAME (t));
      if (HAS_DECL_ASSEMBLER_NAME_P (t)
	  && DECL_ASSEMBLER_NAME_SET_P (t)
	  && DECL_ASSEMBLER_NAME (t) != DECL_NAME (t))
	dump_child ("mngl", DECL_ASSEMBLER_NAME (t));
      if (DECL_ABSTRACT_ORIGIN (t))
        dump_child ("orig", DECL_ABSTRACT_ORIGIN (t));
      /* And types.  */
      queue_and_dump_type (di, t);
      dump_child ("scpe", DECL_CONTEXT (t));
      /* And a source position.  */
      xloc = expand_location (DECL_SOURCE_LOCATION (t));
      if (xloc.file)
	{
	  const char *filename = lbasename (xloc.file);

	  dump_maybe_newline (di);
	  fprintf (di->stream, "srcp: %s:%-6d ", filename,
		   xloc.line);
	  di->column += 6 + strlen (filename) + 8;
	}
      /* And any declaration can be compiler-generated.  */
      if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_COMMON)
	  && DECL_ARTIFICIAL (t))
	dump_string_field (di, "note", "artificial");
      if (DECL_CHAIN (t) && !dump_flag (di, TDF_SLIM, NULL))
	dump_child ("chain", DECL_CHAIN (t));
    }
  else if (code_class == tcc_type)
    {
      /* All types have qualifiers.  */
      int quals = lang_hooks.tree_dump.type_quals (t);

      if (quals != TYPE_UNQUALIFIED)
	{
	  fprintf (di->stream, "qual: %c%c%c     ",
		   (quals & TYPE_QUAL_CONST) ? 'c' : ' ',
		   (quals & TYPE_QUAL_VOLATILE) ? 'v' : ' ',
		   (quals & TYPE_QUAL_RESTRICT) ? 'r' : ' ');
	  di->column += 14;
	}

      /* All types have associated declarations.  */
      dump_child ("name", TYPE_NAME (t));

      /* All types have a main variant.  */
      if (TYPE_MAIN_VARIANT (t) != t)
	dump_child ("unql", TYPE_MAIN_VARIANT (t));

      /* And sizes.  */
      dump_child ("size", TYPE_SIZE (t));

      /* All types have alignments.  */
      dump_int (di, "algn", TYPE_ALIGN (t));
    }
  else if (code_class == tcc_constant)
    /* All constants can have types.  */
    queue_and_dump_type (di, t);

  /* Give the language-specific code a chance to print something.  If
     it's completely taken care of things, don't bother printing
     anything more ourselves.  */
  if (lang_hooks.tree_dump.dump_tree (di, t))
    goto done;

  /* Now handle the various kinds of nodes.  */
  switch (code)
    {
      int i;

    case IDENTIFIER_NODE:
      dump_string_field (di, "strg", IDENTIFIER_POINTER (t));
      dump_int (di, "lngt", IDENTIFIER_LENGTH (t));
      break;

    case TREE_LIST:
      dump_child ("purp", TREE_PURPOSE (t));
      dump_child ("valu", TREE_VALUE (t));
      dump_child ("chan", TREE_CHAIN (t));
      break;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator it;
	for (i = 0, it = tsi_start (t); !tsi_end_p (it); tsi_next (&it), i++)
	  {
	    char buffer[32];
	    sprintf (buffer, "%u", i);
	    dump_child (buffer, tsi_stmt (it));
	    node_emit_json(tsi_stmt(it));
	  }
      }
      break;

    case TREE_VEC:
      dump_int (di, "lngt", TREE_VEC_LENGTH (t));
      for (i = 0; i < TREE_VEC_LENGTH (t); ++i)
	{
	  char buffer[32];
	  sprintf (buffer, "%u", i);
	  dump_child (buffer, TREE_VEC_ELT (t, i));
	}
      break;

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      dump_int (di, "prec", TYPE_PRECISION (t));
      dump_string_field (di, "sign", TYPE_UNSIGNED (t) ? "unsigned": "signed");
      dump_child ("min", TYPE_MIN_VALUE (t));
      dump_child ("max", TYPE_MAX_VALUE (t));

      if (code == ENUMERAL_TYPE)
	dump_child ("csts", TYPE_VALUES (t));
      break;

    case REAL_TYPE:
      dump_int (di, "prec", TYPE_PRECISION (t));
      break;

    case FIXED_POINT_TYPE:
      dump_int (di, "prec", TYPE_PRECISION (t));
      dump_string_field (di, "sign", TYPE_UNSIGNED (t) ? "unsigned": "signed");
      dump_string_field (di, "saturating",
			 TYPE_SATURATING (t) ? "saturating": "non-saturating");
      break;

    case POINTER_TYPE:
      dump_child ("ptd", TREE_TYPE (t));
      break;

    case REFERENCE_TYPE:
      dump_child ("refd", TREE_TYPE (t));
      break;

    case METHOD_TYPE:
      dump_child ("clas", TYPE_METHOD_BASETYPE (t));
      /* Fall through.  */

    case FUNCTION_TYPE:
      dump_child ("retn", TREE_TYPE (t));
      dump_child ("prms", TYPE_ARG_TYPES (t));
      break;

    case ARRAY_TYPE:
      dump_child ("elts", TREE_TYPE (t));
      dump_child ("domn", TYPE_DOMAIN (t));
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TREE_CODE (t) == RECORD_TYPE)
	dump_string_field (di, "tag", "struct");
      else
	dump_string_field (di, "tag", "union");

      dump_child ("flds", TYPE_FIELDS (t));
      queue_and_dump_index (di, "binf", TYPE_BINFO (t),
			    DUMP_BINFO);
      break;

    case CONST_DECL:
      dump_child ("cnst", DECL_INITIAL (t));
      break;

    case DEBUG_EXPR_DECL:
      dump_int (di, "-uid", DEBUG_TEMP_UID (t));
      /* Fall through.  */

    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case RESULT_DECL:
      if (TREE_CODE (t) == PARM_DECL)
	dump_child ("argt", DECL_ARG_TYPE (t));
      else
	dump_child ("init", DECL_INITIAL (t));
      dump_child ("size", DECL_SIZE (t));
      dump_int (di, "algn", DECL_ALIGN (t));

      if (TREE_CODE (t) == FIELD_DECL)
	{
	  if (DECL_FIELD_OFFSET (t))
	    dump_child ("bpos", bit_position (t));
	}
      else if (VAR_P (t) || TREE_CODE (t) == PARM_DECL)
	{
	  dump_int (di, "used", TREE_USED (t));
	  if (DECL_REGISTER (t))
	    dump_string_field (di, "spec", "register");
	}
      break;

    case FUNCTION_DECL:
      dump_child ("args", DECL_ARGUMENTS (t));
      if (DECL_EXTERNAL (t))
	dump_string_field (di, "body", "undefined");
      if (TREE_PUBLIC (t))
	dump_string_field (di, "link", "extern");
      else
	dump_string_field (di, "link", "static");
      if (DECL_SAVED_TREE (t) && !dump_flag (di, TDF_SLIM, t))
	dump_child ("body", DECL_SAVED_TREE (t));
      break;

    case INTEGER_CST:
      fprintf (di->stream, "int: ");
      print_decs (wi::to_wide (t), di->stream);
      break;

    case STRING_CST:
      fprintf (di->stream, "strg: %-7s ", TREE_STRING_POINTER (t));
      dump_int (di, "lngt", TREE_STRING_LENGTH (t));
      break;

    case REAL_CST:
      dump_real (di, "valu", TREE_REAL_CST_PTR (t));
      break;

    case FIXED_CST:
      dump_fixed (di, "valu", TREE_FIXED_CST_PTR (t));
      break;

    case TRUTH_NOT_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case CLEANUP_POINT_EXPR:
    case VIEW_CONVERT_EXPR:
    case SAVE_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      /* These nodes are unary, but do not have code class `1'.  */
      dump_child ("op 0", TREE_OPERAND (t, 0));
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case INIT_EXPR:
    case MODIFY_EXPR:
    case COMPOUND_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      /* These nodes are binary, but do not have code class `2'.  */
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      break;

    case COMPONENT_REF:
    case BIT_FIELD_REF:
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      dump_child ("op 2", TREE_OPERAND (t, 2));
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      dump_child ("op 2", TREE_OPERAND (t, 2));
      dump_child ("op 3", TREE_OPERAND (t, 3));
      break;

    case COND_EXPR:
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      dump_child ("op 2", TREE_OPERAND (t, 2));
      break;

    case TRY_FINALLY_EXPR:
    case EH_ELSE_EXPR:
      dump_child ("op 0", TREE_OPERAND (t, 0));
      dump_child ("op 1", TREE_OPERAND (t, 1));
      break;

    case CALL_EXPR:
      {
	int i = 0;
	tree arg;
	call_expr_arg_iterator iter;
	dump_child ("fn", CALL_EXPR_FN (t));
	FOR_EACH_CALL_EXPR_ARG (arg, iter, t)
	  {
	    char buffer[32];
	    sprintf (buffer, "%u", i);
	    dump_child (buffer, arg);
	    i++;
	  }
      }
      break;

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT cnt;
	tree index, value;
	dump_int (di, "lngt", CONSTRUCTOR_NELTS (t));
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), cnt, index, value)
	  {
	    dump_child ("idx", index);
	    dump_child ("val", value);
	  }
      }
      break;

    case BIND_EXPR:
      dump_child ("vars", TREE_OPERAND (t, 0));
      dump_child ("body", TREE_OPERAND (t, 1));
      break;

    case LOOP_EXPR:
      dump_child ("body", TREE_OPERAND (t, 0));
      break;

    case EXIT_EXPR:
      dump_child ("cond", TREE_OPERAND (t, 0));
      break;

    case RETURN_EXPR:
      dump_child ("expr", TREE_OPERAND (t, 0));
      break;

    case TARGET_EXPR:
      dump_child ("decl", TREE_OPERAND (t, 0));
      dump_child ("init", TREE_OPERAND (t, 1));
      dump_child ("clnp", TREE_OPERAND (t, 2));
      /* There really are two possible places the initializer can be.
	 After RTL expansion, the second operand is moved to the
	 position of the fourth operand, and the second operand
	 becomes NULL.  */
      dump_child ("init", TREE_OPERAND (t, 3));
      break;

    case CASE_LABEL_EXPR:
      dump_child ("name", CASE_LABEL (t));
      if (CASE_LOW (t))
	{
	  dump_child ("low ", CASE_LOW (t));
	  if (CASE_HIGH (t))
	    dump_child ("high", CASE_HIGH (t));
	}
      break;
    case LABEL_EXPR:
      dump_child ("name", TREE_OPERAND (t,0));
      break;
    case GOTO_EXPR:
      dump_child ("labl", TREE_OPERAND (t, 0));
      break;
    case SWITCH_EXPR:
      dump_child ("cond", TREE_OPERAND (t, 0));
      dump_child ("body", TREE_OPERAND (t, 1));
      break;
    case OMP_CLAUSE:
      {
	int i;
	fprintf (di->stream, "%s\n", omp_clause_code_name[OMP_CLAUSE_CODE (t)]);
	for (i = 0; i < omp_clause_num_ops[OMP_CLAUSE_CODE (t)]; i++)
	  dump_child ("op: ", OMP_CLAUSE_OPERAND (t, i));
      }
      break;
    default:
      /* There are no additional fields to print.  */
      break;
    }

 done:
  if (dump_flag (di, TDF_ADDRESS, NULL))
    dump_pointer (di, "addr", (void *)t);

  /* Terminate the line.  */
  // Is this where a pass ends?
  child->append(dummy);
  di->tree_json->append(child);
  dummy = node_emit_json(t);
  dummy->set_integer("index", index);
  di->tree_json_debug->append(dummy);
}

/* Return nonzero if FLAG has been specified for the dump, and NODE
   is not the root node of the dump.  */

int dump_flag (dump_info_p di, dump_flags_t flag, const_tree node)
{
  return (di->flags & flag) && (node != di->node);
}

/* Dump T, and all its children, on STREAM.  */

void
dump_node (const_tree t, dump_flags_t flags, FILE *stream)
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
  di.tree_json = new json::array ();
  di.tree_json_debug = new json::array ();
/*TEST
  /* Queue up the first node.  */
  queue (&di, t, DUMP_NONE);

  /* Until the queue is empty, keep dumping nodes.  */
  while (di.queue)
    dequeue_and_dump (&di);

  di.tree_json_debug->dump(stream, false);

  /* Now, clean up.  */
  for (dq = di.free_list; dq; dq = next_dq)
    {
      next_dq = dq->next;
      free (dq);
    }
  splay_tree_delete (di.nodes);
}
