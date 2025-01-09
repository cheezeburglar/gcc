#include "rtl.h"
#include "tree-emit-json.h"
#define INCLUDE_MEMORY
#include "json.h"


inline static void
operand_0_to_json (const_rtx rtx, int idx)
{
  
}

inline static void
operand_i_to_json (const_rtx rtx, int idx)
{
  int value = XINT (rtx, idx);
  return json::integer_number(value);
}

// This should maybe only exist IFF we not in generator file?
// Manually needs to be kept in track with rtl.def

inline static void
operand_L_to_json (const_rtx rtx, int idx)
{
  if (idx == 4 && INSN_P (rtx))
    {
      if (INSN_HAS_LOCATION (in_insn))
	{
	  // TODO :
	  expanded_location xloc = insn_location ();
	}
      else
	{

	}
    }
  else if (idx == 1 && GET_CODE (rtx) == ASM_INPUT)
    {
      
    }
  else if (idx == 6 && GET_CODE (rtx) == ASM_OPERANDS)
    {
      
    }
  else
    gcc_unreachable ();
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
  XTMPL (in_rtx, idx);
}

inline static
json::object *
operand_e_to_json (const_rtx rtx, int idx)
{
  return rtx_to_json (XEXP (rtx, idx));
}

inline static void
operand_E_to_json (const_rtx rtx, int idx)
{
  XVEC (rtx, idx);

  for (int i = 0; i < VECLEN(rtx, idx); i++)
    {
      XVECEXP(rtx, idx, i)
    }

}

inline static void
operand_V_to_json (const_rtx rtx, int idx)
{

}

inline static void
operand_u_to_json (const_rtx rtx, int idx)
{
  rtx sub = XEXP (rtx, idx);
  sub_uid = INSN_UID (sub);
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
operand_r_to_json (const_rtx rtx, int idx)
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

json::object *
rtx_to_json (const_rtx rtx, dump_flags_t flags)
{
  json_obj = new json::object ();
  add_rtx_to_json (rtx, flags, json_obj);
  return json_obj;
}

static void
add_rtx_to_json (const_rtx rtx, dump_flags_t flags, json::object json_obj)
{
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

  // TODO: Iterate over this to get operands instead?
  auto json_array = new json_array ();
  char *format_ptr = GET_RTX_FORMAT (GET_CODE (rtx));
  int idx = 0;
  while (*format_ptr)
    {
      switch(*format_ptr)
	{
        case "*":
	  break;
	case "0":
	  json_array->append(operand_0_to_json (rtx, idx));
	  break;
	case "i":
	  json_array->append(operand_i_to_json (rtx, idx));
	case "n":
	  json_array->append(operand_n_to_json (rtx, idx));
	  break;
	case "w":
	  json_array->append(operand_w_to_json (rtx, idx));
	  break;
	case "s":
	  json_array->append(operand_s_to_json (rtx, idx));
	  break;
	case "S":
	  json_array->append(operand_S_to_json (rtx, idx));
	  break;
	case "T":
	  json_array->append(operand_T_to_json (rtx, idx));
	  break;
	case "e":
	  json_array->append(operand_e_to_json (rtx, idx));
	  break;
	case "E":
	  json_array->append(operand_E_to_json (rtx, idx));
	  break;
	case "V":
	  json_array->append(operand_V_to_json (rtx, idx));
	  break;
	case "u":
	  json_array->append(operand_u_to_json (rtx, idx));
	  break;
	case "b":
	  json_array->append(operand_b_to_json (rtx, idx));
	  break;
	case "B":
	  json_array->append(operand_B_to_json (rtx, idx));
	  break;
	case "t":
	  json_array->append(operand_t_to_json (rtx, idx));
	  break;
	case "r":
	  json_array->append(operand_r_to_json (rtx, idx));
	  break;
	case "p":
	  json_array->append(operand_p_to_json (rtx, idx));
	  break;
	default:
	  gcc_unreachable ();
	}
      format_ptr++;
    }
  RTX_CODE_SIZE(code)

  GET_RTX_CLASS(code);
  GET_RTX_LENGTH (code);
  GET_RTX_FORMAT (code);
  GET_RTX_NAME (rtx);

  // TODO: Do we want to traverse the entire linked list?
  if (RTX_NEXT(rtx))

  // TODO: What/where are the flags?
  if (RTX_FLAG (rtx, in_struct))
  if (RTX_FLAG (rtx, volatil))
  if (RTX_FLAG (rtx, unchanging))
  if (RTX_FLAG (rtx, frame_related))
  if (RTX_FLAG (rtx, jump))
  if (RTX_FLAG (rtx, call))
  if (RTX_FLAG (rtx, return_val))

  //TODO: Handle These
  CALL_P(rtx);
  INSN_P(rtx);

  // TODO: What are these accessing? Operand N?
  XINT(RTX, N)	(RTL_CHECK2 (RTX, N, 'i', 'n').rt_int)
  XUINT(RTX, N)   (RTL_CHECK2 (RTX, N, 'i', 'n').rt_uint)
  XSTR(RTX, N)	(RTL_CHECK2 (RTX, N, 's', 'S').rt_str)
  XEXP(RTX, N)	(RTL_CHECK2 (RTX, N, 'e', 'u').rt_rtx)
  XVEC(RTX, N)	(RTL_CHECK2 (RTX, N, 'E', 'V').rt_rtvec)
  XMODE(RTX, N)	(RTL_CHECK1 (RTX, N, 'M').rt_type)
  XTREE(RTX, N)   (RTL_CHECK1 (RTX, N, 't').rt_tree)
  XBBDEF(RTX, N)	(RTL_CHECK1 (RTX, N, 'B').rt_bb)
  XTMPL(RTX, N)	(RTL_CHECK1 (RTX, N, 'T').rt_str)
  XCFI(RTX, N)	(RTL_CHECK1 (RTX, N, 'C').rt_cfi)
  
  // TODO: We probably don't need this,
  // and should instead iterate over the
  // operand codes of our given
  // RTX.
  switch (GET_CODE (rtx_first))
    {
      case ABS:
      case ABSENCE_SET:
      case ADDR_DIFF_VEC:
      case ADDRESS:
      case ADDR_VEC:
      case AND:
      case ASHIFT:
      case ASHIFTRT:
      case ASM_INPUT:
      case ASM_OPERANDS:
      case ATTR:
      case ATTR_FLAG:
      case AUTOMATA_OPTION:
      case BARRIER:
      case BITREVERSE:
      case BSWAP:
      case CALL:
      case CALL_INSN:
      case CLOBBER:
      case CLRSB:
      case CLZ:
      case CODE_LABEL:
      case COMPARE:
      case CONCAT:
      case CONCATN:
      case COND:
      case COND_EXEC:
      case CONST:
      case CONST_DOUBLE:
      case CONST_FIXED:
      case CONST_INT:
      case CONST_POLY_INT:
      case CONST_STRING:
      case CONST_VECTOR:
      case CONST_WIDE_INT:
      case COPYSIGN:
      case CTZ:
      case DEBUG_EXPR:
      case DEBUG_IMPLICIT_PTR:
      case DEBUG_INSN:
      case DEBUG_MARKER:
      case DEBUG_PARAMETER_REF:
      case DEFINE_ADDRESS_CONSTRAINT:
      case DEFINE_ASM_ATTRIBUTES:
      case DEFINE_ATTR:
      case DEFINE_AUTOMATON:
      case DEFINE_BYPASS:
      case DEFINE_COND_EXEC:
      case DEFINE_CONSTRAINT:
      case DEFINE_CPU_UNIT:
      case DEFINE_DELAY:
      case DEFINE_ENUM_ATTR:
      case DEFINE_EXPAND:
      case DEFINE_INSN_AND_REWRITE:
      case DEFINE_INSN_AND_SPLIT:
      case DEFINE_INSN:
      case DEFINE_INSN_RESERVATION:
      case DEFINE_MEMORY_CONSTRAINT:
      case DEFINE_PEEPHOLE2:
      case DEFINE_PEEPHOLE:
      case DEFINE_PREDICATE:
      case DEFINE_QUERY_CPU_UNIT:
      case DEFINE_REGISTER_CONSTRAINT:
      case DEFINE_RELAXED_MEMORY_CONSTRAINT:
      case DEFINE_RESERVATION:
      case DEFINE_SPECIAL_MEMORY_CONSTRAINT:
      case DEFINE_SPECIAL_PREDICATE:
      case DEFINE_SPLIT:
      case DEFINE_SUBST_ATTR:
      case DEFINE_SUBST:
      case DIV:
      case EH_RETURN:
      case ENTRY_VALUE:
      case EQ_ATTR_ALT:
      case EQ_ATTR:
      case EQ:
      case EXCLUSION_SET:
      case EXPR_LIST:
      case FFS:
      case FINAL_ABSENCE_SET:
      case FINAL_PRESENCE_SET:
      case FIX:
      case FLOAT_EXTEND:
      case FLOAT:
      case FLOAT_TRUNCATE:
      case FMA:
      case FRACT_CONVERT:
      case GE:
      case GEU:
      case GT:
      case GTU:
      case HIGH:
      case IF_THEN_ELSE:
      case INSN:
      case INSN_LIST:
      case INT_LIST:
      case IOR:
      case JUMP_INSN:
      case JUMP_TABLE_DATA:
      case LABEL_REF:
      case LE:
      case LEU:
      case LO_SUM:
      case LSHIFTRT:
      case LTGT:
      case LT:
      case LTU:
      case MATCH_CODE:
      case MATCH_DUP:
      case MATCH_OP_DUP:
      case MATCH_OPERAND:
      case MATCH_OPERATOR:
      case MATCH_PARALLEL:
      case MATCH_PAR_DUP:
      case MATCH_SCRATCH:
      case MATCH_TEST:
      case MEM:
      case MINUS:
      case MOD:
      case MULT:
      case NEG:
      case NE:
      case NOTE:
      case NOT:
      case ORDERED:
      case PARALLEL:
      case PARITY:
      case PC:
      case PLUS:
      case POPCOUNT:
      case POST_DEC:
      case POST_INC:
      case POST_MODIFY:
      case PRE_DEC:
      case PREFETCH:
      case PRE_INC:
      case PRE_MODIFY:
      case PRESENCE_SET:
      case REG:
      case RETURN:
      case ROTATE:
      case ROTATERT:
      case SAT_FRACT:
      case SCRATCH:
      case SEQUENCE:
      case SET_ATTR_ALTERNATIVE:
      case SET_ATTR:
      case SET:
      case SIGN_EXTEND:
      case SIGN_EXTRACT:
      case SIMPLE_RETURN:
      case SMAX:
      case SMIN:
      case SMUL_HIGHPART:
      case SQRT:
      case SS_ABS:
      case SS_ASHIFT:
      case SS_DIV:
      case SS_MINUS:
      case SS_MULT:
      case SS_NEG:
      case SS_PLUS:
      case SS_TRUNCATE:
      case STRICT_LOW_PART:
      case SUBREG:
      case SYMBOL_REF:
      case TRAP_IF:
      case TRUNCATE:
      case UDIV:
      case UMAX:
      case UMIN:
      case UMOD:
      case UMUL_HIGHPART:
      case UNEQ:
      case UNGE:
      case UNGT:
      case UNKNOWN:
      case UNLE:
      case UNLT:
      case UNORDERED:
      case UNSIGNED_FIX:
      case UNSIGNED_FLOAT:
      case UNSIGNED_FRACT_CONVERT:
      case UNSIGNED_SAT_FRACT:
      case UNSPEC:
      case UNSPEC_VOLATILE:
      case US_ASHIFT:
      case US_DIV:
      case USE:
      case US_MINUS:
      case US_MULT:
      case US_NEG:
      case US_PLUS:
      case US_TRUNCATE:
      case VALUE:
      case VAR_LOCATION:
      case VEC_CONCAT:
      case VEC_DUPLICATE:
      case VEC_MERGE:
      case VEC_SELECT:
      case VEC_SERIES:
      case XOR:
      case ZERO_EXTEND:
      case ZERO_EXTRACT:
      default:
	gcc_unreachable();
	break;
    }
  //EXPR LIST ONLY
  REG_NOTE_KIND (rtx);
}


