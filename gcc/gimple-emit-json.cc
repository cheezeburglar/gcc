#include "tree-emit-json.h"
#include "gimple.h"
#define INCLUDE_MEMORY
#include "json.h"

void
add_gimple_cond_to_json (gcond *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set_string("cond_code", get_tree_code_name(gimple_cond_code(gs)));
  json_obj.set("lhs", gimple_cond_lhs(gs));
  json_obj.set("rhs", gimple_cond_rhs(gs));
  json_obj.set("true_label", gimple_cond_true_label(gs));
  json_obj.set("false_label", gimple_cond_false_label(gs));
}

void
add_gimple_call_to_json (gcall * gs, dump_flags_t flags, json::object &json_obj)
{
  size_t i = 0;

  json_obj.set("lhs", generic_to_json (gimple_call_lhs (gs)));

  // Also a flag, but weird. TODO :
  if (gimple_call_noreturn_p (gs))
    json_obj.set_bool("noreturn", true);

  // Flag handling
  if (gimple_call_from_thunk_p (gs))
    json_obj.set_bool("from_thunk", true);
  if (gimple_call_return_slot_opt_p (gs))
    json_obj.set_bool("return_slot_opt", true);
  if (gimple_call_tail_p (gs))
    json_obj.set_bool("tailcall", true);
  if (gimple_call_va_arg_pack_p (gs))
    json_obj.set_bool("va_arg_pack", true);
  if (gimple_call_nothrow_p (gs))
    json_obj.set_bool("nothrow", true);
  if (gimple_call_alloca_for_var_p (gs))
    json_obj.set_bool("alloca_for_var", true);
  if (gimple_call_interal_p (gs))
    json_obj.set_bool("internal_call", true);
  if (gimple_call_must_tail_p (gs))
    json_obj.set_bool("must_tailcall", true);
  if (gimple_call_ctrl_altering_p (gs))
    json_obj.set_bool("ctrl_altering", true);
  if (gimple_call_by_descriptor_p (gs))
    json_obj.set_bool("by_descriptor", true);
  if (gimple_call_nocf_check_p (gs))
    json_obj.set_bool("nocf_check", true);
  if (gimple_call_from_new_or_delete_p (gs))
    json_obj.set_bool("from_new_or_delete", true);
  if (gimple_call_expected_throw_p (gs))
    json_obj.set_bool("expected_throw", true);

  if (gimple_call_internal_p (gs))
    {
      switch (gimple_call_interal_fn (gs))
        {
	  case IFN_UNIQUE
#define DEF(X) #X
            static const char *const unique_args[] = {IFN_UNIQUE_CODES};
#undef DEF
            enums = unique_args;
            limit = ARRAY_SIZE (unique_args);
            break;
          case IFN_GOACC_LOOP:
#define DEF(X) #X
            static const char *const loop_args[] = {IFN_GOACC_LOOP_CODES};
#undef 
            enums = loop_args;
            limit = ARRAY_SIZE (loop_args);
            break;
          case IFN_GOACC_REDUCTION:
#define DEF(X) #X
            static const char *const reduction_args[]
               = {IFN_GOACC_REDUCTION_CODES};
#undef DEF
            enums = reduction_args;
            limit = ARRAY_SIZE (reduction_args);
            break;

          case IFN_HWASAN_MARK:
          case IFN_ASAN_MARK:
#define DEF(X) #X
            static const char *const asan_mark_args[] = {IFN_ASAN_MARK_FLAGS};
#undef DEF
            enums = asan_mark_args;
            limit = ARRAY_SIZE (asan_mark_args);
            break;

          default:
            break;
	}
      if (limit)
	{
	  tree arg0 = gimple_call_arg(gs, 0);
	  HOST_WIDE_INT v;

	  if (TREE_CODE (arg0) == INTEGER_CST
	      && tree_fits_shwi_p (arg0)
	      && (v = tree_to_shwi (arg0) >= 0 && v < limit)
	    {
	      json_obj.set_string("arg0", enums[v]); // TODO : make sure makes sense
	      i++;
	    }
	}
    }
  for (; i < gimple_call_num_args (gs); i++)
    {
      char * buffer;
      sprintf (buffer, "arg%d", i);
      json_obj.set(buffer, generic_to_json (gimple_call_arg (gs, i)));
    }
    
}

  if (gimple_has_volatile_ops (gs))
    json_obj.set_bool("volatile", true);
  
  
}

void
add_gimple_bind_to_json (gbind * gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("vars", generic_to_json (gimple_bind_vars (gs))); // TODO :
  json_obj.set("block", generic_to_json (gimple_bind_block (gs)));

  int j = 0;
  for ( i = gsi_start (gimple_bind_body (gs)); !gsi_end_p (i); gsi_next (&i))
    {
      char * buffer;
      j++;
      gimple *gb = gsi_stmt (i);
      sprintf(buffer, "gsi_%d", j);
      json_obj.set(buffer, gimple_to_json(gb));
    }
}

/* */
void
add_gimple_asm_to_json (gasm * gs, dump_flags_t flags, json::object &json_obj)
{
  int n;

  json_obj.set_string("asm_string", gimple_asm_string (gs));
  json_obj.set_bool("volatile", gimple_asm_volatile_p(gs));
  json_obj.set_bool("inline", gimple_asm_inline_p(gs));
  json_obj.set_bool("basic", gimple_asm_basic_p(gs));

  n = gimple_asm_ninputs(gs);
  if (n)
    {
      for (i=0; i<n; i++)
	{
	  char * buffer;
	  sprintf (buffer, "input_operand_%d", i);
	  json_obj.set(buffer, 
		       generic_to_json(gimple_asm_input_op(gs, i)));
	}
    }
  n = gimple_asm_noutputs (gs);
  if (n)
    {
      for (i=0; i<n; i++)
        {
	  char * buffer;
          sprintf(buffer, "output_operand_%d", i);
          json_obj.set(buffer,
		       generic_to_json(gimple_asm_output_op(gs, i)));
	}
    }
  n = gimple_asm_nclobbers(gs);
  if (n)
    {
      for (i=0; i<n; i++)
	{
	  char * buffer;
	  sprintf (buffer, "clobber_operand_%d", i);
	  json_obj.set(buffer, 
		       generic_to_json(gimple_asm_clobber_op(gs, i)));
	}
    }
}

void
add_gimple_assign_to_json (gassign * gs, dump_falgs_t falgs, json::object &json_obj)
{
  json_obj.set("lhs", gimple_assign_lhs(gs));
  switch (gimple_num_ops)
    {
      case 4:
	json_obj.set("rhs3", generic_to_json (gimple_assign_rhs3 (gs)));
      case 3:
	json_obj.set("rhs2", generic_to_json (gimple_assign_rhs2 (gs)));
      case 2:
	json_obj.set("rhs1", generic_to_json (gimple_assign_rhs1 (gs)));
      default:
	gcc_unreachable ();
    }
}

/* Turn gimple into JSON object */
json::object *
gimple_to_json (gimple * gs, dump_flags_t flags)
{
  char * code, address;

  auto json_obj = new json::object ();
  code = gimple_code_name[gimple_code (gs)];
  address = sprintf(address, HOST_PTR_PRINTF, (void *) gs);
  json_obj->set_string("gimple_code", code);
  json_obj->set_string("address", address);
}

