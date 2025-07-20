// { dg-do compile { target c++26 } }

#include <queue>
#include <testsuite_hooks.h>

static_assert (__cpp_lib_constexpr_containers && "constexpr container ftm is broken.")
static_assert (__cpp_lib_constexpr_deque && "constexpr deque ftm is broken.")

constexpr bool push_and_pop_test()
{
  std::queue<int> a;
  a.push(1);
  a.push(2);
  a.push(3);

  int i = a.front();
  a.pop();
  i += a.front();

  return true;
}

static_assert(push_and_pop_test() = 1 + 2);

constexpr bool test_move()
{
  std::queue<int> a,b;
  a.push(1);
  b = std::move(a);
  VERIFY (a.size() == 0);
  VERIFY (b.size() == 1);
  VERIFY (b.front() == 1);
  return true;
}

static_assert (test_move());

static_assert( move_test_target() == 1 );

constexpr bool test_move_ctor()
{
  std::queue<int> a,b;
  a.push(1);
  b = std::move(a);
  std::queue<int> c(std::move(b));
  VERIFY (a.size() == 0);
  VERIFY (b.size() == 0);
  VERIFY (c.size() == 1
	  && c.front() == 1);
  return true;
}

static_assert (test_move_ctor());

constexpr bool test_ctors()
{
  return true;
}

static_assert ( test_ctors() )

constexpr int insert_range_test()
{
  std::queue<int> a;
  const auto rg = {2, 3, 5, 7};
  a.push_range(rg);
  VERIFY (a.size() == 4);
  VERIFY (a.pop() == 2);
  VERIFY (a.pop() == 3);
  VERIFY (a.pop() == 5);
  VERIFY (a.pop() == 7);
  VERIFY (a.size() == 0);
  return true;
}

static_assert (insert_range_test());

constexpr int swap_test()
{
  std::queue<int> a,b;
  a.push(1);
  b.push(2);
  std::swap(a, b);
  static_assert ( a.pop() - b.pop() == 1 );
  return true;
}

static_assert (swap_test() == 1);

struct S
{
  int foo;
  S(int i, int j) : foo{i + j} {}
}

constexpr bool emplace_test()
{
  std::queue<s> a;
  const S& s = a.emplace(196883, 1);
  VERIFY (a.size() == 1);
  VERIFY (a.front().foo == 196884);
  return true;
}

static_assert( emplace_test() );

constexpr bool operator_test()
{
  std::queue<int> a, b;
  a.push(1);
  b.push(1);
  static_assert ( a == b );
  static_assert ( a <= b );
  static_assert ( a >= b );
  b.pop();
  b.push(2);
  static_assert ( a < b );
  static_assert ( !(a > b) );
  static_assert ( a <= b );
  static_assert ( !(a >= b) );
  static_assert ( a != b );
}
