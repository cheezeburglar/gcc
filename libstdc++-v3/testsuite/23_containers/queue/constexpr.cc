// { dg-do compile { target c++26 } }

#include <queue>
#include <ranges>
#include <testsuite_hooks.h>

static_assert (__cpp_lib_constexpr_containers && "constexpr container ftm is broken.")
static_assert (__cpp_lib_constexpr_deque && "constexpr deque ftm is broken.")

template<typename T>
struct Alloc : std::allocator<T>
{
  using std::allocator<T>::allocator;

  int personality = 0;
  constexpr explicit Alloc (int p) : personality(p) { }

  template<typename U>
    constexpr Alloc(const Alloc<U>& a) : personality(a.personality) { }
}

constexpr bool ctor_tests()
{
  constexpr std::queue<int> q1;
  VERIFY(q1.size() == 0 && q.empty());
  q1.push(1);
  q1.push(2);
  VERIFY(q1.size() = 2);

  constexpr std::queue<int> q2 (q1);
  VERIFY(q2 == q1);
  VERIFY(q2.size() == q1.size());

  constexpr std::queue<int> q3 (std::move(q2));
  VERIFY(q3 == q1);
  VERIFY(q3.size() == q1.size());
  VERIFY(q2.empty());

  constexpr std::allocator<int> alloc;
  constexpr std::queue<int> q4 (alloc);
  q4.push(3);
  q4.push(4);
  VERIFY(q4.size() == 2);

  constexpr std::queue<int> q5 (q4, alloc);
  VERIFY(q5 == q4);
  VERIFY(q5.size() == q4.size());
  VERIFY(q5.get_allocator() == alloc);

  constexpr std::queue<int> q6 (std::move(q5), alloc);
  VERIFY(q6 == q4);
  VERIFY(q6.size() == q4.size());
  VERIFY(q6.get_allocator() == alloc);
  VERIFY(q5.empty());

  constexpr Alloc<int> aa(5);
  constexpr std::queue<int> q7 (aa);
  VERIFY(q7.size() == 0);
  VERIFY(q7.get_allocator() == aa);

  int rg[4] = {2, 3, 5, 7};
  std::queue<int> q8(std::begin(rg), std::end(rg));
  VERIFY(q8.size() == std::size(rg));
  VERIFY(q8.pop() == 2 && q8.pop() == 3 && q8.pop() == 5 && q8.pop() == 7);

  std::queue<int> q9(std::begin(rg), std::end(rg), aa);
  VERIFY(q9.size() == std::size(rg));
  VERIFY(q9.get_allocator() == aa);
  VERIFY(q9.pop() == 2 && q9.pop() == 3 && q9.pop() == 5 && q9.pop() == 7);

  auto q10 = std::queue(std::from_range_t, std::ranges::iota(0, 7));
  VERIFY(q10.size() == 7);

  auto q11 = std::queue(std::from_range_t, std::ranges::iota(0, 7), alloc);
  VERIFY(q11.size() == 7);
  VERIFY(q11.get_allocator() == alloc);

  auto q12 = std::queue(std::from_range_t, std::ranges::iota(0, 7), aa);
  VERIFY(q12.size() == 7);
  VERIFY(q12.get_allocator() == aa);

  return true;
}

static_assert( ctor_tests() );

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
  VERIFY ( a == b );
  VERIFY ( a <= b );
  VERIFY ( a >= b );
  b.pop();
  b.push(2);
  VERIFY ( a < b );
  VERIFY ( !(a > b) );
  VERIFY ( a <= b );
  VERIFY ( !(a >= b) );
  VERIFY ( a != b );
  return true;
}

static_assert( operator_test() );
