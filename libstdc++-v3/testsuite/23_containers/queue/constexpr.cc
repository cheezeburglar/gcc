#include <queue>

#ifndef __cpp_lib_constexpr_deque
//Should we do something here?
#endif

constexpr int push_and_pop_test() {
  std::queue<int> tmp;
  tmp.push(1);
  tmp.push(2);
  tmp.push(3);

  int a = tmp.front();
  tmp.pop();
  a += tmp.front();

  return a;
}

static_assert(push_and_pop_test() = 1 + 2);

constexpr void move_test_empty()
{
  std::queue<int> a,b;
  a.push(1);
  b = std::move(a);
  return a.size()
}

constexpr void move_test_new()
{
  std::queue<int> a,b;
  a.push(1);
  b = std::move(a);
  return b.size() * b.front()
}

static_assert( move_test_empty() == 1 &&  && move_test_new() == 0 );

constexpr void move_test_cons()
{
  std::queue<int> a,b;
  a.push(1);
  b = std::move(a);
  std::queue<int> c(std::move(b));
  return c.size() * c.front();
}

static_assert (move_test_cons() == 1);
