
#ifndef GCC_GIMPLE_JSON_H
#define GCC_GIMPLE_JSON_H

#include "dumpfile.h"
#include "json.h"

static void gimple_to_json (gimple * gs, dump_flags_t flags);
json::array * serialize_gimple_to_json (gimple *gs, dump_flags_t flags);

#endif
