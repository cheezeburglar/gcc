#include "cfgexpand.h"
#include "tree-pass.h"

// Sizes are kept as poly_int64
// Alignments are unsigned ints
//

// For a single function
class stack_safety
{
public:
  struct ss_info; // TODO :What should this contain information wise?
private:
  function * cfun;
  // TODO: Scalar evoloution information?
public:
  stack_safety ();
  stack_safety (function * cfun);
  ~stack_safety ();

  bool is_safe ();
}; // end stack_safety

class stack_lifetime
{
  struct bb_lifetime_info {
    int foo;
  };

public:
//  class stack_lifetime_debug;
  
  class stack_liverange {
  // TODO : analogous to BitVector of LLVM? maybe bitmap or something

  public:
    void add_range () {};
    bool overlap const (stack_liverange &other) { return false; };
    void join (stack_liverange &other) {};
  };

  enum LivenessClass {
    surely,
    maybely, // TODO: This is fucked up
  };

private:
  const *function cfun; // TODO: Make sure ok

  using liveness_map = hash_map<basic_block, bb_lifetime_info>;
  liveness_map bb_liveness;

  unsinged allocs;
  // GCC analogue of AllocInst?

public:
stack_liverange get_liverange ();

}; // end stack_lifetime

// Should this be RTL level?

class pass_stack_safety : public gimple_opt_pass;
{

};

class pass_stack_lifetime : public gimple_opt_pass;
{

};
