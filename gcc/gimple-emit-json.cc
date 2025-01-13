//TODO: figure out dependencies cleanly. brute forcing it rn


#define INCLUDE_STRING
#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "dumpfile.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "tree-emit-json.h"
#include "langhooks.h"
#include "tree-iterator.h"
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
#include "make-unique.h"
#include "context.h" // To access dump streams for tree_json_writer
#include <zlib.h>

#include "tree-emit-json.h"
#define INCLUDE_MEMORY
#include "json.h"
#include "attribs.h"
#include "asan.h"

// TODO : 

json::array *
gimple_seq_to_json (gimple_seq seq, dump_flags_t flags)
{
  gimple_stmt_iterator iter;

  auto json_seq = new json::array ();

  for (iter = gsi_start (seq); ~gsi_end_p (iter); gsi_next (&iter))
    {
      gimple *gs = gsi_stmt (iter);
      auto json_obj = gimple_emit_json (gs, flags); //This probably has to be serialization
      json_seq->append(json_obj);
    }
  return json_seq;
}

// TODO : this ones kinda weird.

static void
add_gimple_error_mark_to_json ()
{

}



static void
add_gimple_predict_to_json ()
{

}

static void
add_gimple_omp_critical_to_json (const gomp_critical *gs, dump_flags_t flags,
				 json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_critical_clauses (gs), flags));
  json_obj.set("name", generic_to_json (gimple_omp_critical_name (gs), flags));
}

static void
add_gimple_omp_scan_to_json (const gomp_scan *gs, dump_flags_t flags,
			     json::object &json_obj)
{
  
}

static void
add_gimple_omp_structured_block_to_json (const gimple *gs, dump_flags_t flags,
					 json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
}

static void
add_gimple_omp_section_to_json (const gomp_sections *gs, dump_flags_t flags,
			        json::object &json_obj)
{
  if (gimple_omp_section_last_p (gs))
    json_obj.set_bool("last", true);
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_sections_clauses (gs), flags));
  json_obj.set("control", generic_to_json (gimple_omp_sections_control (gs), flags));
}

static void
add_gimple_omp_master_to_json (const gimple *gs, dump_flags_t flags,
			       json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
}

static void
add_gimple_omp_dispatch_to_json (const gimple *gs, dump_flags_t flags,
			 json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_dispatch_clauses (gs), flags));
}

static void
add_gimple_omp_scope_to_json (const gimple *gs, dump_flags_t flags,
			      json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_scope_clauses (gs), flags));
}

static void
add_gimple_omp_masked_to_json (const gimple *gs, dump_flags_t flags,
			       json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_masked_clauses (gs), flags));
}

static void
add_gimple_omp_taskgroup_to_json (const gimple *gs, dump_flags_t flags,
				  json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_taskgroup_clauses (gs), flags));
}

static void
add_gimple_omp_sections_swtich_to_json (const gomp_sections *gs, dump_flags_t flags,
					json::object &json_obj)
{}

static void
add_gimple_omp_sections_to_json (const gomp_sections *gs, dump_flags_t flags,
				 json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_sections_control (gs), flags));
  json_obj.set("control", generic_to_json (gimple_omp_sections_control (gs), flags));
}

static void
add_gimple_omp_return_to_json (const gomp_teams *gs, dump_flags_t flags,
			       json::object &json_obj)
{
  if (gimple_omp_return_nowait_p (gs))
    json_obj.set_bool("nowait", true);
  json_obj.set("lhs", generic_to_json (gimple_omp_return_lhs (gs), flags));
}

static void
add_gimple_omp_teams_to_json (const gomp_teams *gs, dump_flags_t flags,
			      json::object &json_obj)
{
  if (gimple_omp_teams_host (gs))
    json_obj.set_bool("host", true);
  json_obj.set("clauses", generic_to_json (gimple_omp_teams_clauses (gs), flags));
  json_obj.set("child_fn", generic_to_json (gimple_omp_teams_child_fn (gs), flags));
  json_obj.set("data_arg", generic_to_json (gimple_omp_teams_data_arg(gs), flags));
  
}

static void
add_gimple_omp_target_to_json (const gomp_target *gs, dump_flags_t flags, 
			       json::object &json_obj)
{
  switch (gimple_omp_target_kind (gs))
    {
    case GF_OMP_TARGET_KIND_REGION:
      json_obj.set_bool ("kind_region", true);
      break;
    case GF_OMP_TARGET_KIND_DATA:
      json_obj.set_bool ("kind_data", true);
      break;
    case GF_OMP_TARGET_KIND_UPDATE:
      json_obj.set_bool ("kind_update", true);
      break;
    case GF_OMP_TARGET_KIND_ENTER_DATA:
      json_obj.set_bool ("kind_enter data", true);
      break;
    case GF_OMP_TARGET_KIND_EXIT_DATA:
      json_obj.set_bool ("kind_exit data", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_KERNELS:
      json_obj.set_bool ("kind_oacc_kernels", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
      json_obj.set_bool ("kind_oacc_parallel", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_SERIAL:
      json_obj.set_bool ("kind_oacc_serial", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_DATA:
      json_obj.set_bool ("kind_oacc_data", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_UPDATE:
      json_obj.set_bool ("kind_oacc_update", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_ENTER_DATA:
      json_obj.set_bool ("kind_oacc_enter_data", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_EXIT_DATA:
      json_obj.set_bool ("kind_oacc_exit_data", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_DECLARE:
      json_obj.set_bool ("kind_oacc_declare", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
      json_obj.set_bool ("kind_oacc_host_data", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_PARALLEL_KERNELS_PARALLELIZED:
      json_obj.set_bool ("kind_oacc_parallel_kernels_parallelized", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_PARALLEL_KERNELS_GANG_SINGLE:
      json_obj.set_bool ("kind_oacc_parallel_kernels_gang_single", true);
      break;
    case GF_OMP_TARGET_KIND_OACC_DATA_KERNELS:
      json_obj.set_bool ("kind_oacc_data_kernels", true);
      break;
    default:
      gcc_unreachable ();
    }
  json_obj.set("clauses", generic_to_json (gimple_omp_target_clauses (gs), flags));
  json_obj.set("child_fn", generic_to_json (gimple_omp_target_child_fn (gs), flags));
  json_obj.set("data_arg", generic_to_json (gimple_omp_target_data_arg(gs), flags));
}

static void
add_gimple_omp_single_to_json (const gomp_single *gs, dump_flags_t flags,
			       json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("clauses", generic_to_json (gimple_omp_single_clauses (gs), flags));
}

static void
add_gimple_omp_continue_to_json (const gomp_continue * gs, dump_flags_t flags,
				 json::object &json_obj)
{
  json_obj.set("def", generic_to_json (gimple_omp_continue_control_def (gs), flags));
  json_obj.set("use", generic_to_json (gimple_omp_continue_control_use (gs), flags));
}


static void
add_gimple_omp_for (const gomp_for * gs, dump_flags_t flags,
		    json::object &json_obj)
{
  auto json_iter = new json::array ();

  switch (gimple_omp_for_kind (gs))
    {
    case GF_OMP_FOR_KIND_FOR:
      json_obj.set_bool("kind_for", true);
    case GF_OMP_FOR_KIND_DISTRIBUTE:
      json_obj.set_bool("kind_distribute", true);
    case GF_OMP_FOR_KIND_TASKLOOP:
      json_obj.set_bool("kind_taskloop", true);
    case GF_OMP_FOR_KIND_OACC_LOOP:
      json_obj.set_bool("kind_oacc_loop", true);
    case GF_OMP_FOR_KIND_SIMD:
      json_obj.set_bool("kind_simd", true);
    default:
      gcc_unreachable();
    }
  json_obj.set_integer("collapse", gimple_omp_for_collapse (gs));
  json_obj.set("clauses", generic_to_json (gimple_omp_for_clauses (gs), flags));
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags)); // TODO : this is statement_omp class
  for (size_t i = 0; i < gimple_omp_for_collapse (gs); i++)
    {
      auto json_gomp_for_iter = new json::object ();
      json_gomp_for_iter->set("index", generic_to_json (gimple_omp_for_index (gs, i), flags));
      json_gomp_for_iter->set("init", generic_to_json (gimple_omp_for_initial (gs, i), flags));
      json_gomp_for_iter->set("final", generic_to_json (gimple_omp_for_final (gs, i), flags));
      json_gomp_for_iter->set_string("cond", get_tree_code_name (gimple_omp_for_cond (gs, i)));
      json_gomp_for_iter->set("incr", generic_to_json (gimple_omp_for_incr (gs, i), flags));
      json_iter->append(json_gomp_for_iter);
    }
  json_obj.set("iter", json_iter);
  json_obj.set("pre_body", gimple_seq_to_json (gimple_omp_for_pre_body (gs), flags));
}

static void
add_gimple_omp_atomic_store (const gomp_atomic_store * gs, dump_flags_t flags,
			     json::object &json_obj)
{
  // Flags
  if (gimple_omp_atomic_need_value_p (gs))
    json_obj.set_bool("need_value", true);
  if (gimple_omp_atomic_weak_p (gs))
    json_obj.set_bool("weak", true);
  json_obj.set("val", generic_to_json (gimple_omp_atomic_store_val (gs), flags));
  json_obj.set("memory_order", 
	       omp_atomic_memory_order_emit_json (
	          gimple_omp_atomic_memory_order (gs)));
}

static void
add_gimple_omp_atomic_load (const gomp_atomic_load * gs, dump_flags_t flags,
			    json::object &json_obj)
{
  // Flags
  if (gimple_omp_atomic_need_value_p (gs))
    json_obj.set_bool("need_value", true);
  if (gimple_omp_atomic_weak_p (gs))
    json_obj.set_bool("weak", true);

  json_obj.set("lhs", generic_to_json (gimple_omp_atomic_load_lhs (gs), flags));
  json_obj.set("rhs", generic_to_json (gimple_omp_atomic_load_rhs (gs), flags));

  // TODO: Memory order handling ???
  json_obj.set("memory_order", 
	       omp_atomic_memory_order_emit_json (gimple_omp_atomic_memory_order (gs)));
  }

static void
add_gimple_omp_task_to_json (const gomp_task * gs, dump_flags_t flags,
			     json::object &json_obj)
{
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("child_fn", generic_to_json (gimple_omp_task_child_fn (gs), flags));
  json_obj.set("data_arg", generic_to_json (gimple_omp_task_data_arg (gs), flags));
  json_obj.set("copy_fn", generic_to_json (gimple_omp_task_copy_fn (gs), flags));
  json_obj.set("arg_size", generic_to_json (gimple_omp_task_arg_size (gs), flags));
}

static void
add_gimple_omp_parallel_to_json (const gomp_parallel *gs, dump_flags_t flags,
				 json::object &json_obj)
{
  // maybe seperate out for gimple_statement_op type class
  // i.e. structure these helpers based out on class hierarchy of GIMPLE
  json_obj.set("body", gimple_seq_to_json (gimple_omp_body (gs), flags));
  json_obj.set("child_fn", generic_to_json (gimple_omp_parallel_child_fn (gs), flags));
  json_obj.set("data_arg", generic_to_json (gimple_omp_parallel_data_arg (gs), flags));

  // Doesn't look like more accessors
}

static void
add_gimple_eh_filter_to_json (const geh_filter *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("types", generic_to_json (gimple_eh_filter_types (gs), flags));
  json_obj.set("failure", gimple_seq_to_json (gimple_eh_filter_failure (gs), flags));
}

static void
add_gimple_eh_must_not_throw_to_json (const geh_mnt *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("fndecl", generic_to_json (gimple_eh_must_not_throw_fndecl (gs), flags));
}

static void
add_gimple_eh_else_to_json (const geh_else *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("n_body", gimple_seq_to_json(gimple_eh_else_n_body (gs), flags));
  json_obj.set("e_body", gimple_seq_to_json(gimple_eh_else_e_body (gs), flags));
}

static void
add_gimple_eh_dispatch_to_json (const geh_dispatch *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set_integer ("dispatch_region", gimple_eh_dispatch_region (gs));
}

static void
add_gimple_resx_to_json (const gresx *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set_integer("resx_region", gimple_resx_region (gs));
}

static void
add_gimple_debug_to_json (const gdebug *gs, dump_flags_t flags, json::object &json_obj)
{
  switch (gs->subcode)
    {
      case GIMPLE_DEBUG_BIND:
	{
	  json_obj.set_bool("debug_bind", true);
	  json_obj.set("var", generic_to_json (gimple_debug_bind_get_var (gs), flags));
	  json_obj.set("value", generic_to_json (gimple_debug_bind_get_value (gs), flags));
	  break;
	}	
      case GIMPLE_DEBUG_SOURCE_BIND:
	{
	  json_obj.set_bool("debug_source_bind", true);
	  json_obj.set("var", generic_to_json (gimple_debug_source_bind_get_var (gs), flags));
	  json_obj.set("value", generic_to_json (gimple_debug_source_bind_get_value (gs), flags));
	  break;
	}	
      case GIMPLE_DEBUG_BEGIN_STMT:
	{
	  json_obj.set_bool("debug_begin_stmt", true);
	  break;
	}	
      case GIMPLE_DEBUG_INLINE_ENTRY:
	{
	  json_obj.set_bool("inline_entry", true);
	  json_obj.set("block", generic_to_json ( // TODO :
			      gimple_block (gs)
			      ? block_ultimate_origin (gimple_block (gs))
			      : NULL_TREE, flags));
	  break;
	}
      default:
	gcc_unreachable ();
    }
}

static void
add_gimple_assume_to_json (const gcatch *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("guard", generic_to_json (gimple_assume_guard(gs), flags));
  json_obj.set("body", gimple_seq_to_json (gimple_assume_body (gs), flags));
}

static void
add_gimple_transaction_to_json (const gtransaction *gs, dump_flags_t flags, json::object &json_obj)
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

  json_obj.set("label_norm", generic_to_json (gimple_transaction_label_norm (gs), flags));
  json_obj.set("label_uninst", generic_to_json (gimple_transaction_label_uninst (gs), flags));
  json_obj.set("label_over", generic_to_json (gimple_transaction_label_over (gs), flags));
  json_obj.set("body", gimple_seq_to_json (gimple_transaction_body (gs), flags));
}

static void
add_gimple_phi_to_json (const gphi *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("result", generic_to_json (gimple_phi_result (gs), flags));

  for (size_t i = 0; i < gimple_phi_num_args (gs); i++)
    {
      // TODO: Verify this is alright later. 
      auto json_arg = new json::object ();
      char * buffer;
      sprintf (buffer, "arg%u", i);

      json_arg->set("arg_def", generic_to_json (gimple_phi_arg_def (gs, i), flags));

      if ((flags & TDF_RAW)
	  && gimple_phi_arg_has_location (gs, i))
	{
	  expanded_location phi_xloc; 
	  phi_xloc = expand_location (gimple_phi_arg_location (gs, i));
	  set_xloc_as (*json_arg, flags, phi_xloc);
	}
      json_obj.set(buffer, json_arg);
    }
}

static void
add_gimple_catch_to_json (const gcatch *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("types", generic_to_json (gimple_catch_types (gs), flags));
  json_obj.set("handler", gimple_seq_to_json (gimple_catch_handler (gs), flags));
}

static void
add_gimple_try_to_json (const gtry *gs, dump_flags_t flags, json::object &json_obj)
{
  // Flag handling
  if (gimple_try_kind (gs) == GIMPLE_TRY_CATCH)
    json_obj.set_bool("catch", true);
  else if (gimple_try_kind (gs) == GIMPLE_TRY_FINALLY)
    json_obj.set_bool("finally", true);
  else
    json_obj.set_bool("unknown", true);

  json_obj.set("eval", gimple_seq_to_json (gimple_try_eval (gs), flags));
  json_obj.set("cleanup", gimple_seq_to_json (gimple_try_cleanup (gs), flags));
}

static void
add_gimple_switch_to_json (const gswitch * gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("index", generic_to_json (gimple_switch_index(gs), flags));
  for (unsigned i = 0; i < gimple_switch_num_labels (gs); i++)
    {
      char * buffer;
      sprintf (buffer, "label%u", i);
      json_obj.set(buffer, generic_to_json (gimple_switch_label(gs, i), flags));
    } // TODO : bb handling?
}

static void
add_gimple_return_to_json (const greturn * gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("retval", generic_to_json (gimple_return_retval(gs), flags));
}

static void
add_gimple_goto_to_json (const ggoto *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("target", generic_to_json (gimple_goto_dest (gs), flags));
}

static void
add_gimple_label_to_json (const glabel *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("label", generic_to_json(gimple_label_label(gs), flags));
}

static void
add_gimple_cond_to_json (const gcond *gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set_string("cond_code", get_tree_code_name(gimple_cond_code(gs)));
  json_obj.set("lhs", generic_to_json (gimple_cond_lhs(gs), flags));
  json_obj.set("rhs", generic_to_json (gimple_cond_rhs(gs), flags));
  json_obj.set("true_label", generic_to_json (gimple_cond_true_label(gs), flags));
  json_obj.set("false_label", generic_to_json (gimple_cond_false_label(gs), flags));
}

static void
add_gimple_call_to_json (const gcall * gs, dump_flags_t flags, json::object &json_obj)
{
  size_t i = 0;

  json_obj.set("lhs", generic_to_json (gimple_call_lhs (gs), flags));

  // Also a flag, but weird. TODO :
  if (gimple_call_noreturn_p (gs))
    json_obj.set_bool("noreturn", true);
  if (gimple_has_volatile_ops (gs))
    json_obj.set_bool("volatile", true);

  // Flag handling
  if (gimple_call_from_thunk_p ((gcall *)gs))
    json_obj.set_bool("from_thunk", true);
  if (gimple_call_return_slot_opt_p (gs))
    json_obj.set_bool("return_slot_opt", true);
  if (gimple_call_tail_p (gs))
    json_obj.set_bool("tailcall", true);
  if (gimple_call_va_arg_pack_p (gs))
    json_obj.set_bool("va_arg_pack", true);
  if (gimple_call_nothrow_p ((gcall *)gs))
    json_obj.set_bool("nothrow", true);
  if (gimple_call_alloca_for_var_p ((gcall *) gs))
    json_obj.set_bool("alloca_for_var", true);
  if (gimple_call_internal_p (gs))
    json_obj.set_bool("internal_call", true);
  if (gimple_call_must_tail_p (gs))
    json_obj.set_bool("must_tailcall", true);
  if (gimple_call_ctrl_altering_p (gs))
    json_obj.set_bool("ctrl_altering", true);
  if (gimple_call_by_descriptor_p ((gcall *) gs))
    json_obj.set_bool("by_descriptor", true);
  if (gimple_call_nocf_check_p (gs))
    json_obj.set_bool("nocf_check", true);
  if (gimple_call_from_new_or_delete ((gcall *) gs))
    json_obj.set_bool("from_new_or_delete", true);
  if (gimple_call_expected_throw_p ((gcall *) gs))
    json_obj.set_bool("expected_throw", true);

  if (gimple_call_internal_p (gs))
    {
      const char *const *enums = NULL;
      unsigned limit = 0;
      switch (gimple_call_internal_fn (gs))
        {
	  case IFN_UNIQUE:
#define DEF(X) #X
            static const char *const unique_args[] = {IFN_UNIQUE_CODES};
#undef DEF
            enums = unique_args;
            limit = ARRAY_SIZE (unique_args);
            break;
          case IFN_GOACC_LOOP:
#define DEF(X) #X
            static const char *const loop_args[] = {IFN_GOACC_LOOP_CODES};
#undef DEF
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
	      && (v = tree_to_shwi (arg0) >= 0 && v < limit))
	    {
	      json_obj.set_string("arg0_enums", enums[v]); // TODO : make sure makes sense
	      i++;
	    }
	}
    }
  for (; i < gimple_call_num_args (gs); i++)
    {
      char * buffer;
      sprintf (buffer, "arg%d", i);
      json_obj.set(buffer, generic_to_json (gimple_call_arg (gs, i), flags));
    }
}

static void
add_gimple_bind_to_json (const gbind * gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("vars", generic_to_json (gimple_bind_vars (gs), flags)); // TODO :
  json_obj.set("block", generic_to_json (gimple_bind_block (gs), flags));
  json_obj.set("bind_body", generic_to_json (gimple_bind_block (gs), flags));
}

/* */
static void
add_gimple_asm_to_json (const gasm * gs, dump_flags_t flags, json::object &json_obj)
{
  int n;

  json_obj.set_string("asm_string", gimple_asm_string (gs));
  json_obj.set_bool("volatile", gimple_asm_volatile_p(gs));
  json_obj.set_bool("inline", gimple_asm_inline_p(gs));
  json_obj.set_bool("basic", gimple_asm_basic_p(gs));

  n = gimple_asm_ninputs(gs);
  if (n)
    {
      auto json_array = new json::array ();
      for (unsigned i = 0; i<n; i++)
	  json_array->append (generic_to_json (gimple_asm_input_op(gs, i), flags));
      json_obj.set("asm_input_ops", json_array);
    }
  n = gimple_asm_noutputs (gs);
  if (n)
    {
      auto json_array = new json::array ();
      for (unsigned i = 0; i<n; i++)
	  json_array->append (generic_to_json (gimple_asm_output_op(gs, i), flags));
      json_obj.set("asm_output_ops", json_array);
    }
  n = gimple_asm_nclobbers(gs);
  if (n)
    {
      auto json_array = new json::array ();
      for (unsigned i = 0; i<n; i++)
	  json_array->append (generic_to_json (gimple_asm_clobber_op(gs, i), flags));
      json_obj.set("asm_output_ops", json_array);
    }
}

static void
add_gimple_assign_to_json (const gassign * gs, dump_flags_t flags, json::object &json_obj)
{
  json_obj.set("lhs", generic_to_json (gimple_assign_lhs (gs), flags));
  switch (gimple_num_ops (gs))
    {
      case 4:
	json_obj.set("rhs3", generic_to_json (gimple_assign_rhs3 (gs), flags));
      case 3:
	json_obj.set("rhs2", generic_to_json (gimple_assign_rhs2 (gs), flags));
      case 2:
	json_obj.set("rhs1", generic_to_json (gimple_assign_rhs1 (gs), flags));
      default:
	gcc_unreachable ();
    }
}

/* Serialize to json briefly. */
static 
json::object *
gimple_to_json_brief (gimple * gs)
{
  // TODO : Instantiate visitor, queue up nodes to be dumped.

  const char *code;
  char *address;
  // TODO : DO ALL THINGS FOR GIMPLE BASE CLASS
  auto json_obj = new json::object ();

  if (!gs)
    return json_obj;

  code = gimple_code_name[gimple_code (gs)];
  sprintf(address, HOST_PTR_PRINTF, (void *) gs);
  json_obj->set_string("address", address);
  json_obj->set_string("gimple_code", code);

  return json_obj;
}

/* Queue gimple and serialize to json briefly. */

static 
json::object *
gimple_to_json_brief (gimple * gs, dump_info_p di)
{
  gimple_to_json_brief (gs);
  queue (gs);
}


/* Turn gimple nodes into JSON object */
static
json::object *
gimple_to_json (gimple * gs, dump_flags_t flags)
{
  // TODO : Instantiate visitor, queue up nodes to be dumped.

  // TODO : DO ALL THINGS FOR GIMPLE BASE CLASS
  auto json_obj = new json::object ();

  if (!gs)
    return json_obj;

  const char *code = gimple_code_name[gimple_code (gs)];
  char * address;
  sprintf(address, HOST_PTR_PRINTF, (void *) gs);
  json_obj->set_string("address", address);
  json_obj->set_string("gimple_code", (char *)code);

  // TODO: hit things that are in all base classes
  json_obj->set_integer("no_warning", gs->no_warning);
  json_obj->set_integer("visited", gs->visited);
  json_obj->set_integer("nontemporal_move", gs->nontemporal_move);
  json_obj->set_integer("plf", gs->plf);
  json_obj->set_integer("modified", gs->modified);
  json_obj->set_integer("has_volatile_ops", gs->has_volatile_ops);
  json_obj->set_integer("uid", gs->uid);

  switch (code)
    {
    case GIMPLE_ASM:
      add_gimple_asm_to_json (as_a <const gasm *> (gs), flags, *json_obj);
      break;
    case GIMPLE_ASSIGN:
      add_gimple_assign_to_json (as_a <const gassign *> (gs), flags, *json_obj);
      break;
    case GIMPLE_ASSUME:
      add_gimple_assume_to_json (as_a <const gasm *> (gs), flags, *json_obj);
      break;
    case GIMPLE_BIND:
      add_gimple_bind_to_json (as_a <const gbind *> (gs), flags, *json_obj);
      break;
    case GIMPLE_CALL:
      add_gimple_call_to_json (as_a <const gcall *> (gs), flags, *json_obj);
      break;
    case GIMPLE_CATCH:
      add_gimple_catch_to_json (as_a <const gasm *> (gs), flags, *json_obj);
      break;
    case GIMPLE_COND:
      add_gimple_cond_to_json (as_a <const gcond *> (gs), flags, *json_obj);
      break;
    case GIMPLE_DEBUG:
      add_gimple_debug_to_json (as_a <const gasm *> (gs), flags, *json_obj);
      break;
    case GIMPLE_EH_DISPATCH:
      add_gimple_eh_dispatch_to_json (as_a <const geh_dispatch *> (gs), flags, *json_obj);
      break;
    case GIMPLE_EH_ELSE:
      add_gimple_eh_else_to_json (as_a <const geh_else *> (gs), flags, *json_obj);
      break;
    case GIMPLE_EH_FILTER:
      add_gimple_eh_filter_to_json (as_a <const geh_filter *> (gs), flags, *json_obj);
      break;
    case GIMPLE_EH_MUST_NOT_THROW:
      add_gimple_eh_must_not_throw_to_json (as_a <const geh_mnt *> (gs), flags, *json_obj);
      break;
    case GIMPLE_ERROR_MARK:
      add_gimple_error_mark_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_GOTO:
      add_gimple_goto_to_json (as_a <const ggoto *> (gs), flags, *json_obj);
      break;
    case GIMPLE_LABEL:
      add_gimple_label_to_json (as_a <const glabel *> (gs), flags, *json_obj);
      break;
    case GIMPLE_NOP:
      add_gimple_nop_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_OMP_ATOMIC_LOAD:
      add_gimple_omp_atomic_load_to_json (as_a <const gomp_atomic_load *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_ATOMIC_STORE:
      add_gimple_omp_atomic_store_to_json (as_a <const gomp_atomic_store *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_CONTINUE:
      add_gimple_omp_continue_to_json (as_a <const gomp_continue *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_CRITICAL:
      add_gimple_omp_critical_to_json (as_a <const gomp_critical *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_DISPATCH:
      add_gimple_omp_dispatch_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_OMP_FOR:
      add_gimple_omp_for_to_json (as_a <const gomp_for *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_MASKED:
      add_gimple_omp_masked_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_OMP_MASTER:
      add_gimple_omp_master_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_OMP_ORDERED:
      add_gimple_omp_ordered_to_json (as_a <const gomp_ordered *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_PARALLEL:
      add_gimple_omp_parallel_to_json (as_a <const gomp_parallel *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_RETURN:
      add_gimple_omp_return_to_json (as_a <const gomp_teams *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_SCAN:
      add_gimple_omp_scan_to_json (as_a <const gomp_scan *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_SCOPE:
      add_gimple_omp_scope_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_OMP_SECTION:
      add_gimple_omp_section_to_json (as_a <const gomp_sections *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_SECTIONS:
      add_gimple_omp_sections_to_json (as_a <const gomp_sections *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_SECTIONS_SWITCH:
      json_obj->set_bool("gomp_sections_switch", true);
      break;
    case GIMPLE_OMP_SINGLE:
      add_gimple_omp_single_to_json (as_a <const gomp_single *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_STRUCTURED_BLOCK:
      add_gimple_omp_structured_block_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_OMP_TARGET:
      add_gimple_omp_target_to_json (as_a <const gomp_target *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_TASK:
      add_gimple_omp_task_to_json (as_a <const gomp_task *> (gs), flags, *json_obj);
      break;
    case GIMPLE_OMP_TASKGROUP:
      add_gimple_omp_taskgroup_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_OMP_TEAMS:
      add_gimple_omp_teams_to_json (as_a <const gomp_teams *> (gs), flags, *json_obj);
      break;
    case GIMPLE_PHI:
      add_gimple_phi_to_json (as_a <const gphi *> (gs), flags, *json_obj);
      break;
    case GIMPLE_PREDICT:
      add_gimple_predict_to_json (gs, flags, *json_obj);
      break;
    case GIMPLE_RESX:
      add_gimple_resx_to_json (as_a <const gresx *> (gs), flags, *json_obj);
      break;
    case GIMPLE_RETURN:
      add_gimple_return_to_json (as_a <const greturn *> (gs), flags, *json_obj);
      break;
    case GIMPLE_SWITCH:
      add_gimple_switch_to_json (as_a <const gswitch *> (gs), flags, *json_obj);
      break;
    case GIMPLE_TRANSACTION:
      add_gimple_transaction_to_json (as_a <const gtransaction *> (gs), flags, *json_obj);
      break;
    case GIMPLE_TRY:
      add_gimple_try_to_json (as_a <const gtry *> (gs), flags, *json_obj);
      break;
    case GIMPLE_WITH_CLEANUP_EXPR:
      add_gimple_with_cleanup_expr_to_json (as_a <const gimple_statement_wce *> (gs), flags, *json_obj);
      break;
    default:
      gcc_unreachable ();
      break;
    }
  return json_obj;
}

static void
queue (dump_info_p di, const gimple * gs)
{
  dump_queue_p dq;
  dump_node_info_p dni;

  /* Obtain a new queue node.  */
  if (di->free_list)
    {
      dq = di->free_list;
      di->free_list = dq->next;
    }
  else
    dq = XNEW (struct dump_queue);

  /* Create a new entry in the splay-tree and insert into queue iff new.
   * Else, end.*/
  dni = XNEW (struct dump_node_info);
  if (!splay_tree_lookup (di->nodes, (splay_tree_key) gs))
  {
    dq->node = splay_tree_insert (di->nodes, (splay_tree_key) gs,
          			(splay_tree_value) dni);
    dq->next = 0;
    if (!di->queue_end)
      di->queue = dq;
    else
      di->queue_end->next = dq;
    di->queue_end = dq;
  }
}

static void
dequeue_and_add (dump_info_p di)
{
  dump_queue_p dq;
  splay_tree_node stn;
  gimple gimp;

  /* Get the next node from the queue.  */
  dq = di->queue;
  stn = dq->node;
  gimp = (gimple) stn->key;

  /* Remove the node from the queue, and put it on the free list.  */
  di->queue = dq->next;
  if (!di->queue)
    di->queue_end = 0;
  dq->next = di->free_list;
  di->free_list = dq;

  /* Convert the node to JSON and store it to be dumped later. */
  auto dummy = gimple_emit_json(gs, di).release();
  di->json_dump->append(dummy);
}

json::array *
serialize_gimple_to_json (gimple *gs, dump_flags_t flags)
{
  struct dump_info di;
  dump_queue_p dq;
  dump_queue_p next_dq;

  di.queue = 0;
  di.queue_end = 0;
  di.free_list = 0;
  di.flags = flags;
  di.node = t;
  di.nodes = splay_tree_new (splay_tree_compare_pointers, 0,
			     splay_tree_delete_pointers);
  di.json_dump = new json::array ();

  /* queue up the first node.  */
  queue (&di, t);

  /* until the queue is empty, keep dumping nodes.  */
  while (di.queue)
    dequeue_and_add (&di);

  /* now, clean up.  */
  for (dq = di.free_list; dq; dq = next_dq)
    {
      next_dq = dq->next;
      free (dq);
    }
  splay_tree_delete (di.nodes);
  return di.json_dump;
}

DEBUG_FUNCTION void
debug_dump_gimple_json (gimple *gs, FILE *stream)
{
  dump_info di;

  di.stream = stream;
  di.queue = 0;
  di.queue_end = 0;
  di.free_list = 0;
  di.flags = TDF_LINENO;
  di.node = gs;
  di.nodes = splay_tree_new (splay_tree_compare_pointers, 0,
			     splay_tree_delete_pointers);
  di.json_dump = new json::array ();

  queue (&di, gs);

  while (di.queue)
    dequeue_and_add (&di);

  di.json_dump->dump(stream, true);

  splay_tree_delete (di.nodes);
}
