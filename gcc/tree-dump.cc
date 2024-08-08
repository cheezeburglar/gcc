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
#include "wide-int-print.h" //for print_hex
#include "real.h" //for real printing
#include "internal-fn.h"

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


/* Helper for emitting function_decl information. Called iff TREE_CODE is FUNCTION_TYPE */

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

/* Helper for emitting _DECL node information
    See the qualms in dump_decl_name of tree-pretty-print.cc -
     maybe ask to see if everything is okay here.*/

/* Adds Identifier information to JSON object. Make sensitive to translate ID? */

void
identifier_node_add_json (tree t, json::object* dummy)
  {
    //ID_to_local flag here later
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
        if (HAS_DECL_ASSEMBLER_NAME_P(t) //flag later
            && DECL_ASSEMBLER_NAME_SET_P(t))
          identifier_node_add_json(DECL_ASSEMBLER_NAME_RAW(t), dummy);
        else if (DECL_NAMELESS(t)
                 && DECL_IGNORED_P(t))
          name = NULL_TREE;
        else 
          identifier_node_add_json(name, dummy);
      }
    //Need account for flags later
    
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
            const char* c = TREE_CODE (t) == CONST_DECL ? "Const_UID" : "Decl_UID";
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
function_name_json (tree t, json::object* dummy)
{
  if (CONVERT_EXPR_P (t))
    decl_node_add_json(t, dummy);
}

/* Here we emit data in the generic tree without traversing the tree. base on tree-pretty-print.cc */

json::object* 
node_emit_json(tree t)
{
  tree op0, op1, type;
  json::object* dummy;
  json::array* holder;
  enum tree_code code;
//  std::string address_buffer;

  dummy = new json::object ();
  holder = new json::array ();

//  if (EXPR_HAS_LOCATION(t))


  if (TREE_CODE(t) == ERROR_MARK)
    dummy->set_bool("error_mark", true);

// What do we want to dump for every node?
//  TODO dump node address

  code = TREE_CODE(t);
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
                //dummy->set_bool("type_decl", true);
	      else // unnamed 
                dummy->set_string("type_name", "unnamed");
	    }
            else if (TREE_CODE (t) == VECTOR_TYPE)
    	      {
              //Handle recursion here later	  
	      holder->append(node_emit_json(TREE_TYPE (t)));
              dummy->set("vector", holder);
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
	const char* _x = (TREE_CODE (t) == POINTER_TYPE ? "*" : "&");
      
          //Do we need to emit pointer type here?
    
        if (TREE_TYPE (t) == NULL)
   	  dummy->set_bool("null type", true);
        //might be able to remove later and replace with the FUNCTIONT_TYPE in this switch statement
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
//	        This needs to be HEX.
                char* buff;
                buff = new char ();
                print_hex(TYPE_UID(t), buff);
   	        _id->set_string("uid", buff);

              }
 
            dummy->set("function decl", function_decl_emit_json(function_node));

//            //Argument iteration
//            if (arg_node && arg_node != void_list_node && arg_node != error_mark_node)
//	      it_args = new json::object();
//
//            //Fix this later.
//            while (arg_node && arg_node != void_list_node && arg_node != error_mark_node)
//   	      {
//	        it_args = node_emit_json(arg_node);
//	        args_holder->append(it_args);
//   	        arg_node = TREE_CHAIN (arg_node);
//   	      }
//   	    dummy->set("type_uid", _id);
//   	    dummy->set("args", args_holder);
          }
        else
          {
   	  //pickup here
   	  unsigned int quals = TYPE_QUALS (t);
   	  const char* type_qual;
   
   	  dummy->set("tree type", node_emit_json(TREE_TYPE(t)));
   	  
          if (quals & TYPE_QUAL_CONST)
   	    type_qual = "const";
   	  if (quals & TYPE_QUAL_VOLATILE)
   	    type_qual = "volatile";
   	  if (quals & TYPE_QUAL_RESTRICT)
   	    type_qual = "restrict";
   	  
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
        //TDF_GIMPLE corresponds to the gimple front end - is this okay? Check later
        if ((TREE_CODE (t) == MEM_REF
             || TREE_CODE (t) == TARGET_MEM_REF))
          {
            dummy->set("__MEM_REF", node_emit_json(TREE_TYPE(t)));

            if (TYPE_ALIGN (TREE_TYPE (t))
                != TYPE_ALIGN (TYPE_MAIN_VARIANT (TREE_TYPE (t))))
              {
                dummy->set_integer("offset", TYPE_ALIGN (TREE_TYPE(t)));
              }
            dummy->set("op0", node_emit_json(TREE_TYPE(TREE_OPERAND (t, 0))));
            if (TREE_TYPE (TREE_OPERAND (t, 0))
                != TREE_TYPE (TREE_OPERAND (t, 1)))
              {
                dummy->set("op1", node_emit_json(TREE_TYPE(TREE_OPERAND (t, 1))));
              }
            // if (! integer_zerop (TREE_OPERAND (node, 1)))
            if (TREE_CODE (t) == TARGET_MEM_REF)
              {
                if (TREE_OPERAND (t, 2))
                  {
                  dummy->set("op2", node_emit_json(TREE_TYPE(TREE_OPERAND (t, 2))));
                  dummy->set("op3", node_emit_json(TREE_TYPE(TREE_OPERAND (t, 3))));
                  }
                if (TREE_OPERAND (t, 4))
                  dummy->set("op4", node_emit_json(TREE_TYPE(TREE_OPERAND (t, 4))));
              }
          }
        else if (TREE_CODE (t) == MEM_REF
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
                dummy->set("FOO_564", node_emit_json(TREE_OPERAND (t, 0)));
              }
            else
              dummy->set("FOO_567", node_emit_json(TREE_OPERAND (TREE_OPERAND (t, 0), 0)));
          }
        else
          {
            tree type = TREE_TYPE(t);
            tree op0 = TREE_OPERAND(t, 0);
            tree op1 = TREE_OPERAND(t, 1);
            tree op1type = TYPE_MAIN_VARIANT (TREE_TYPE (op1));

            tree op0size = TYPE_SIZE(type);
            tree op1size = TYPE_SIZE (TREE_TYPE(op1type));
            dummy->set("type", node_emit_json(type));
            
            holder->append(node_emit_json(op0));
            holder->append(node_emit_json(op1type));
            if (! integer_zerop (op1))
              holder->append(node_emit_json(op1));
            if (TREE_CODE(t) == TARGET_MEM_REF)
              {
                tree temp = TMR_INDEX2 (t);
                if (temp)
                  holder->append(node_emit_json(temp));
                temp = TMR_INDEX (t);
                if (temp)
                  {
                    holder->append(node_emit_json(temp));
                    temp = TMR_STEP(t);
                    if (temp)
                      holder->append(node_emit_json(temp)); //check later - missing 1 append.
                  }
                dummy->set("tmf_address", holder);
              }
          }
        if (MR_DEPENDENCE_CLIQUE (t) != 0) //TDF_ALIAS usually controls
          {
            dummy->set_integer("clique", MR_DEPENDENCE_CLIQUE(t));
            dummy->set_integer("base", MR_DEPENDENCE_BASE(t));
          }
      }
    break;

    case ARRAY_TYPE:
      {
        unsigned int quals = TYPE_QUALS (t);
        tree temp;

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
      dummy->set_bool("RUQ_fallthrough", true);
      }
      break;
    case LANG_TYPE:

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
            unsigned int len;
            char* buff;

            buff = new char ();

            //FIX LATER
            if (wi::neg_p (val, TYPE_SIGN (TREE_TYPE (t))))
              val = -val;
            print_hex_buf_size (val, &len);
            buff = XALLOCAVEC (char, len);
            print_hex(val, buff);

            dummy->set_string("val", buff);
        }
      if (TREE_OVERFLOW (t))
        dummy->set_bool("overflow", true);
      }
      break;

    case POLY_INT_CST:
      dummy->set_string("code", "poly_int_cst");
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
      //TODO handle function_decl
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
      dummy->set_string("tree_code", "component_ref");
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
  
    //TODO
//    case OMP_ARRAY_SECTION:
//      break;

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
            //cst_elt->set_integer("", ix); //fix
            if (field)
              {
                if (is_struct_init)
                  cst_elt->set("field", node_emit_json(field));
                else if (is_array_init 
                         && (TREE_CODE (field) != INTEGER_CST
                             || curidx != wi::to_widest (field)))
                  {
                    json::array* _array_init_json;
                    if (TREE_CODE (field) == RANGE_EXPR) //CHECK LATER
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
              { //This workaround is ugly; refactor or fix later
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
      dummy->set_string("tree_code",
                        TREE_CODE(t) == MODIFY_EXPR ? "modify_expr"
                                                    : "init_expr");
      dummy->set("op0",
                 node_emit_json( TREE_OPERAND (t, 0)));
      dummy->set("op1",
                 node_emit_json( TREE_OPERAND (t, 1)));
      break;

    case TARGET_EXPR:
      dummy->set_string("tree_code", "target_expr");
      dummy->set("slot", node_emit_json (TARGET_EXPR_SLOT(t)));
      dummy->set("initial", node_emit_json (TARGET_EXPR_INITIAL(t)));
      break;

    case DECL_EXPR:
      decl_node_add_json(DECL_EXPR_DECL(t), dummy);
      break;

    case COND_EXPR:
    /* I don't think we account for lowering this to GIMPLE. */
//      if (TREE_TYPE (t) == NULL || TREE_TYPE (t) == void_type_node)
//        {
//          
//        }
      dummy->set_bool("cond_expr", true);
      dummy->set("if",
                 node_emit_json (TREE_OPERAND (t, 0)));
      dummy->set("then",
                 node_emit_json (TREE_OPERAND (t, 1)));
      dummy->set("else",
                 node_emit_json (TREE_OPERAND (t, 2)));
      break;

    case BIND_EXPR:
      if (BIND_EXPR_VARS (t))
        {
          for (op0 = BIND_EXPR_VARS (t); op0; op0 = DECL_CHAIN (op0))
            {
              // TODO c.f. tree-pretty print's impl
              holder->append(node_emit_json (op0));
            }
          dummy->set("bind_expr_vars", holder);
        }
      dummy->set("bind_expr_body", 
                 node_emit_json(BIND_EXPR_BODY(t)));
      break;

    case CALL_EXPR:
      {
        dummy->set_bool("return_slot_optimization", CALL_EXPR_RETURN_SLOT_OPT(t));
        dummy->set_bool("tail_call", CALL_EXPR_TAILCALL(t));
        if (CALL_EXPR_FN (t) != NULL_TREE)
          /* TODO ()*/ ;
        else
          dummy->set_string("", internal_fn_name (CALL_EXPR_IFN (t)));

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
          dummy->set_bool("__builtin_va_arg_pack", true); //check later
        
        op1 = CALL_EXPR_STATIC_CHAIN (t);
        if (op1)
          dummy->set("", node_emit_json(op1));
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
    CASE_CONVERT: //Check this later
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

    case COMPLEX_EXPR: //check later
      holder->append (node_emit_json (TREE_OPERAND (t,0)));
      holder->append (node_emit_json (TREE_OPERAND (t,1)));
      dummy->set("complex_expr", holder);
//      dummy->set("non_lvalue_expr",
//                 node_emit_json (TREE_OPERAND (t,0)));
//      dummy->set("non_lvalue_expr",
//                 node_emit_json (TREE_OPERAND (t,0)));
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
        else //it MIGHT be okay to dump out the null tree in this case?
          dummy->set_bool("return_expr", true);
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
        {}
      else if (CASE_LOW(t))
        {}
      else
        {}
      break;
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
      dummy->set_bool("fallthrough", true);
      break;
    default:
      dummy->set_bool("default fallthrough", true);
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
