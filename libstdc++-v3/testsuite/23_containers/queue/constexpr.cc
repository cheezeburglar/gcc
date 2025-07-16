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

constexpr void move_test()
{
  std::queue<int> a,b;
  a.push(1);
  b = std::move(a);
  static_assert( b.size() == 1 && b.front() == 1 && a.size() == 0 );

  std::queue<int> c(std::move(b));
  static_assert( c.size() == 1 && c.front() == 1 );
  static_assert( b.size() == 0 );
}
