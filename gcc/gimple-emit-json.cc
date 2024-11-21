#include "tree-emit-json.h"
#include "gimple.h"
#define INCLUDE_MEMORY
#include "json.h"

json::array *
gimple_seq_to_json (gimple_seq seq, dump_flags_t flags)
{
  gimple_stmt_iterator iter;
  json::array *json_seq;

  auto json_seq = new json::array ();

  for (iter = gsi_start (seq); ~gsi_end_p (iter); gsi_next (&iter))
    {
      json::object *json_obj;
      gimple *gs = gsi_stmt (iter);
      auto json_obj = gimple_to_json (gs);
      json_seq->append(json_obj);
    }
  return json_seq;
}

void
add_gimple_eh_filter_to_json (geh_filter *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("types", generic_to_json (gimple_eh_filter_types (gs)));
  json_obj.set("failure", gimple_seq_to_json (gimple_eh_filter_failure (gs)));
}

void
add_gimple_eh_must_not_throw_to_json (geh_mnt *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("fndecl", generic_to_json (gimple_eh_must_not_throw_fndecl (gs)));
}

void
add_gimple_eh_else_to_json (geh_else *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("n_body", gimple_seq_to_json(gimple_eh_else_n_body (gs)));
  json_obj.set("e_body", gimple_seq_to_json(gimple_eh_else_e_body (gs)));
}

void
add_gimple_eh_dispatch_to_json (geh_dispatch *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set_integer ("region", gimple_eh_dispatch_region (gs));
}

void
add_gimple_resx_to_json (grex *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set_integer("region", gimple_resx_region (gs));
}

void
add_gimple_debug_to_json (gdebug *gs, dump_flags_t flags, json::object &json_obj)
{
  switch (gs->subcode)
    {
      case GIMPLE_DEBUG_BIND:
	{
	  json_obj.set_bool("bind", true);
	  json_obj.set("var", generic_to_json (gimple_debug_bind_get_var (gs)));
	  json_obj.set("value", generic_to_json (gimple_debug_bind_get_value (gs)));
	  break;
	}	
      case GIMPLE_DEBUG_SOURCE_BIND:
	{
	  json_obj.set("source_bind", true);
	  json_obj.set("var", generic_to_json (gimple_debug_source_bind_get_var (gs)));
	  json_obj.set("value", generic_to_json (gimple_debug_source_bind_get_value (gs)));
	  break;
	}	
      case GIMPLE_DEBUG_BEGIN_STMT:
	{
	  json_obj.set("begin_stmt", true);
	  break;
	}	
      case GIMPLE_DEBUG_INLINE_ENTRY:
	{
	  json_obj.set("inline_entry", true);
	  json_obj.set("", generic_to_json ( // TODO :
			      gimple_block (gs)
			      ? block_ultimate_origin (gimple_block (gs))
			      : NULL_TREE));
	  break;
	}
      default:
	gcc_unreachable ();
    }
}

void
add_gimple_assume_to_json (gcatch *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("guard", generic_to_json (gimple_assume_guard(gs)));
  json_obj.set("body", gimple_seq_to_json (gimple_assume_body (gs)));
}

void
add_gimple_transaction_to_json (gcatch *gs, dump_flags_t flags, json::object &json_obj)
{
  unsigned subcode = gimple_transaction_subcode (gs);

  // Subcode handling
  if (subcode & GTMA_IS_OUTER)
    json_obj.set_bool ("gtma_is_outer", true);
  if (subcode & GTMA_IS_RELAXED)
    json_obj.set_bool ("gtma_is_relaxed", true);
  if (subcode & GTMA_HAVE_ABORT)
    json_obj.set_bool ("gtma_have_abort", true);
  if (subcode & GTMA_HAVE_LOAD)
    json_obj.set_bool ("gtma_have_load", true);
  if (subcode & GTMA_HAVE_STORE)
    json_obj.set_bool ("gtma_have_store", true);
  if (subcode & GTMA_MAY_ENTER_IRREVOCABLE)
    json_obj.set_bool ("gtma_may_enter_irrevocable", true);
  if (subcode & GTMA_DOES_GO_IRREVOCABLE)
    json_obj.set_bool ("gtma_does_go_irrevocable", true);
  if (subcode & GTMA_HAS_NO_INSTRUMENTATION)
    json_obj.set_bool ("gtma_has_no_instrumentation", true);

  json_obj.set("label_norm", generic_to_json (gimple_transaction_label_norm (gs)));
  json_obj.set("uninst", generic_to_json (gimple_transaction_uninst (gs)));
  json_obj.set("over", generic_to_json (gimple_transaction_over (gs)));
  json_obj.set("body", gimple_seq_to_json (gimple_transaction_body (gs)));
}

void
add_gimple_phi_to_json (gphi *gs, dump_flags_t flags, json::object &json_obj)
{
  for (size_t i = 0; i < gimple_phi_num_args (gs); i++)
    {
      json::object * json_arg;
      char * buffer;
      sprintf (buffer, "phi_op%u", i);

      json_arg.set("arg_body", generic_to_json (gimple_phi_arg_def (gs, i), flags));

      if (flags & TDF_RAW)
	{
	  xloc phi_loc = expand_location (gimple_phi_arg_location (gs, i));
	  set_xloc_as (json_arg, flags, xloc_phi);
	}
      json_obj.set(buffer, json_arg);
    }
}

void
add_gimple_catch_to_json (gcatch *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("types", generic_to_json (gimple_catch_types (gs)));
  json_obj.set("handler", gimple_seq_to_json (gimple_catch_handler (gs)));
}

void
add_gimple_try_to_json (gtry *gs, dump_flags_t flags, json::object &json_obj)
{
  // Flag handling
  if (gimple_try_kind (gs) == GIMPLE_TRY_CATCH)
    json_obj.set_bool("catch", true);
  else if (gimple_try_kind (gs) == GIMPLE_TRY_FINALLY)
    json_obj.set_bool("finally", true);
  else
    json_obj.set_bool("unknown", true);

  json_obj.set("eval", gimple_seq_to_json (gimple_try_eval (gs)));
  json_obj.set("cleanup", gimple_seq_to_json (gimple_try_cleanup (gs)));
}

void
add_gimple_switch_to_json (gswitch * gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("index", generic_to_json (gimple_switch_index(gs)));
  for (i = 0; i < gimple_switch_num_labels (gs); i++)
    {
      char * buffer;
      sprintf (buffer, "label%u", i);
      json_obj.set(buffer, generic_to_json (gimple_switch_label(gs, i)));
    } // TODO : bb handling?
}

void
add_gimple_return_to_json (greturn * gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("retval", generic_to_json (gimple_return_retval(gs)));
}

void
add_gimple_goto_to_json (ggoto *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("target", gimple_goto_dest (gs));
}

void
add_gimple_label_to_json (glabel *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("label", generic_to_json(gimple_label_label(gs)));
}

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
  if (gimple_has_volatile_ops (gs))
    json_obj.set_bool("volatile", true);

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
add_gimple_assign_to_json (gassign * gs, dump_flags_t falgs, json::object &json_obj)
{
  json_obj.set("lhs", gimple_assign_lhs (gs));
  switch (gimple_num_ops (gs))
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

