//TODO: figure out dependencies cleanly. brute forcing it rn

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
#include "make-unique.h"
#include "context.h" // To access dump streams for tree_json_writer
#include <zlib.h>

#include "rtl.h"
#include "tree-emit-json.h"
#define INCLUDE_MEMORY
#include "json.h"

static void queue (dump_info_rtx_p di, const_rtx rtx);
static void dequeue_and_add (dump_info_rtx_p di);

inline static void
operand_0_to_json (const_rtx rtx, int idx)
{
  
}

inline static json::object *
operand_i_to_json (const_rtx rtx, int idx)
{
  int value = XINT (rtx, idx);
}

// This should maybe only exist IFF we not in generator file?
// Manually needs to be kept in track with rtl.def

inline static json::object *
operand_L_to_json (const_rtx rtx, int idx)
{
////  if (idx == 4 && INSN_P (rtx))
//    {
//      if (INSN_HAS_LOCATION (rtx))
//	{
//	  //expanded_location xloc = insn_location ();
//	}
//      else
//	{
//
//	}
//    }
//  else if (idx == 1 && GET_CODE (rtx) == ASM_INPUT)
//    {
//      
//    }
//  else if (idx == 6 && GET_CODE (rtx) == ASM_OPERANDS)
//    {
//      
//    }
//  else
//    gcc_unreachable ();
}

inline static void
operand_n_to_json (const_rtx rtx, int idx)
{
  GET_NOTE_INSN_NAME (XINT (rtx, idx));
  XINT (rtx, idx);
}

inline static void
operand_w_to_json (const_rtx rtx, int idx)
{
  XWINT (rtx, idx);
}

inline static void
operand_s_to_json (const_rtx rtx, int idx)
{
  XSTR (rtx, idx);
}

inline static void
operand_S_to_json (const_rtx rtx, int idx)
{
  XSTR (rtx, idx);
}

inline static void
operand_T_to_json (const_rtx rtx, int idx)
{
  XTMPL (rtx, idx);
}

inline static
void
operand_e_to_json (const_rtx rtx, int idx)
{
//  return rtx_to_json (XEXP (rtx, idx));
}

inline static void
operand_E_to_json (const_rtx rtx, int idx)
{
  XVEC (rtx, idx);

  for (int i = 0; i < XVECLEN(rtx, idx); i++)
    {
      XVECEXP(rtx, idx, i);
    }
}

inline static void
operand_V_to_json (const_rtx rtx, int idx)
{

}

inline static void
operand_u_to_json (const_rtx rtx, int idx)
{
  XEXP(rtx, idx);
//  sub_uid = INSN_UID (sub);
}

//FIXME:
//It *looks* like this operand type is unused. qualms?

inline static void
operand_b_to_json (const_rtx rtx, int idx)
{

}

inline static void
operand_B_to_json (const_rtx rtx, int idx)
{
  XBBDEF (rtx, idx);
}

// FIXME: 
// Code switching here ??
// DEBUG_IMPLICT_PTR, DEBUG_PARAMETER_REF, VAR_LOCATION ?
inline static void
operand_t_to_json (const_rtx rtx, int idx)
{
  XTREE(rtx, idx);
}

inline static void
operand_r_to_json (const_rtx rtx, int idx, dump_flags_t flags)
{
  int is_insn = INSN_P (rtx);
  unsigned int regno = REGNO (rtx);
  if (flags & TDF_RAW)
    {
    }
  else
    {
    }
}

inline static void
operand_p_to_json (const_rtx rtx, int idx)
{
  SUBREG_BYTE (rtx);
}

// TODO : 
static void
machine_mode_to_json (rtvec rtvec, dump_flags_t flags)
{

}

// TODO : 
static void
helper (rtvec rtvec, dump_flags_t flags)
{

}

static json::object *
rtx_to_json_brief (const_rtx rtx, dump_flags_t flags)
{
  auto json_obj = new json::object ();
  if (rtx)
    {
      char address [20] = {"\0"};
      sprintf(address, HOST_PTR_PRINTF, (void *) rtx);
      rtx_code code = GET_CODE (rtx);

      json_obj->set_string("code", GET_RTX_NAME(code));
      json_obj->set_string("addr", address);
    }
  return json_obj;
}

static json::object *
rtx_to_json_brief (const_rtx rtx, dump_flags_t flags, dump_info_rtx_p di)
{
  queue (di, rtx);
  return rtx_to_json_brief (rtx, flags);
}

static json::object *
rtx_to_json (const_rtx rtx, dump_flags_t flags, dump_info_rtx_p di)
{
  auto json_obj = new json::object ();

  rtx_code code = GET_CODE (rtx);
  int limit = GET_RTX_LENGTH (GET_CODE (rtx));

  // TODO : DIfferent levels of verbosity for representing this?
  machine_mode mm = GET_MODE(rtx);

  if (RTX_FLAG (rtx, in_struct))
  if (RTX_FLAG (rtx, volatil))
  if (RTX_FLAG (rtx, unchanging))
  if (RTX_FLAG (rtx, frame_related))
  if (RTX_FLAG (rtx, jump))
  if (RTX_FLAG (rtx, call))
  if (RTX_FLAG (rtx, return_val))
    {}
  auto json_array = new json::array ();
  const char *format_ptr = GET_RTX_FORMAT (GET_CODE (rtx));
  for (int idx = 0; idx < limit; idx++)
    {
      switch(format_ptr[idx])
	{
//        case '*':
//	  break;
//	case '0':
//	  json_array->append(operand_0_to_json (rtx, idx));
//	  break;
//	case 'i':
//	  json_array->append(operand_i_to_json (rtx, idx));
//	  break;
//	case 'n':
//	  json_array->append(operand_n_to_json (rtx, idx));
//	  break;
//	case 'w':
//	  json_array->append(operand_w_to_json (rtx, idx));
//	  break;
//	case 's':
//	  json_array->append(operand_s_to_json (rtx, idx));
//	  break;
//	case 'S':
//	  json_array->append(operand_S_to_json (rtx, idx));
//	  break;
//	case 'T':
//	  json_array->append(operand_T_to_json (rtx, idx));
//	  break;
//	case 'e':
//	  json_array->append(operand_e_to_json (rtx, idx, di));
//	  break;
//	case 'E':
//	  json_array->append(operand_E_to_json (rtx, idx, di));
//	  break;
//	case 'V':
//	  json_array->append(operand_V_to_json (rtx, idx, di));
//	  break;
//	case 'u':
//	  json_array->append(operand_u_to_json (rtx, idx));
//	  break;
//	case 'b':
//	  json_array->append(operand_b_to_json (rtx, idx));
//	  break;
//	case 'B':
//	  json_array->append(operand_B_to_json (rtx, idx));
//	  break;
//	case 't':
//	  json_array->append(operand_t_to_json (rtx, idx));
//	  break;
//	case 'r':
//	  json_array->append(operand_r_to_json (rtx, idx));
//	  break;
//	case 'p':
//	  json_array->append(operand_p_to_json (rtx, idx));
//	  break;
//	default:
//	  gcc_unreachable ();
	}
    }

  json_obj->set("operands", json_array);

  RTX_CODE_SIZE(code);
  return json_obj;

//  GET_RTX_CLASS(code);
//  GET_RTX_LENGTH (code);
//  GET_RTX_FORMAT (code);
//  GET_RTX_NAME (rtx);

//  // TODO: Do we want to traverse the entire linked list?
//  if (RTX_NEXT(rtx))
//
//  // TODO: What/where are the flags?
//  if (RTX_FLAG (rtx, in_struct))
//  if (RTX_FLAG (rtx, volatil))
//  if (RTX_FLAG (rtx, unchanging))
//  if (RTX_FLAG (rtx, frame_related))
//  if (RTX_FLAG (rtx, jump))
//  if (RTX_FLAG (rtx, call))
//  if (RTX_FLAG (rtx, return_val))
//
//  //TODO: Handle These
//  CALL_P(rtx);
//  INSN_P(rtx);

  // TODO: What are these accessing? Operand N?
//  XINT(RTX, N)	(RTL_CHECK2 (RTX, N, 'i', 'n').rt_int)
//  XUINT(RTX, N)   (RTL_CHECK2 (RTX, N, 'i', 'n').rt_uint)
//  XSTR(RTX, N)	(RTL_CHECK2 (RTX, N, 's', 'S').rt_str)
//  XEXP(RTX, N)	(RTL_CHECK2 (RTX, N, 'e', 'u').rt_rtx)
//  XVEC(RTX, N)	(RTL_CHECK2 (RTX, N, 'E', 'V').rt_rtvec)
//  XMODE(RTX, N)	(RTL_CHECK1 (RTX, N, 'M').rt_type)
//  XTREE(RTX, N)   (RTL_CHECK1 (RTX, N, 't').rt_tree)
//  XBBDEF(RTX, N)	(RTL_CHECK1 (RTX, N, 'B').rt_bb)
//  XTMPL(RTX, N)	(RTL_CHECK1 (RTX, N, 'T').rt_str)
//  XCFI(RTX, N)	(RTL_CHECK1 (RTX, N, 'C').rt_cfi)
  
//  REG_NOTE_KIND (rtx);
}

static void
queue (dump_info_rtx_p di, const_rtx rtx)
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
  if (!splay_tree_lookup (di->nodes, (splay_tree_key) rtx))
  {
    dq->node = splay_tree_insert (di->nodes, (splay_tree_key) rtx,
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
dequeue_and_add (dump_info_rtx_p di)
{
  dump_queue_p dq;
  splay_tree_node stn;
  rtx * rt;

  /* Get the next node from the queue.  */
  dq = di->queue;
  stn = dq->node;
  rt = (rtx *) stn->key;

  /* Remove the node from the queue, and put it on the free list.  */
  di->queue = dq->next;
  if (!di->queue)
    di->queue_end = 0;
  dq->next = di->free_list;
  di->free_list = dq;

  /* Convert the node to JSON and store it to be dumped later. */
  auto dummy = rtx_to_json(*rt, di->flags, di);
  di->json_dump->append(dummy);
}

json::array *
serialize_rtx_to_json (const_rtx rtx, dump_flags_t flags)
{
  struct dump_info_rtx di;
  dump_queue_p dq;
  dump_queue_p next_dq;

  di.queue = 0;
  di.queue_end = 0;
  di.free_list = 0;
  di.flags = flags;
  di.node = rtx;
  di.nodes = splay_tree_new (splay_tree_compare_pointers, 0,
			     splay_tree_delete_pointers);
  di.json_dump = new json::array ();

// AHH

  /* queue up the first node.  */
  queue (&di, rtx);

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
debug_dump_rtl_json (const_rtx rtx, FILE *stream)
{
  dump_info_rtx di;

  di.stream = stream;
  di.queue = 0;
  di.queue_end = 0;
  di.free_list = 0;
  di.flags = TDF_LINENO;
  di.node = rtx;
  di.nodes = splay_tree_new (splay_tree_compare_pointers, 0,
			     splay_tree_delete_pointers);
  di.json_dump = new json::array ();
  
  queue (&di, rtx);

  while (di.queue)
    dequeue_and_add (&di);

  di.json_dump->dump(stream, true);
  
  splay_tree_delete (di.nodes);
}
