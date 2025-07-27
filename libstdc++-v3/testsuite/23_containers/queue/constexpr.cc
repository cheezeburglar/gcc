// { dg-do compile { target c++26 } }

#include <queue>

#ifndef __cpp_lib_constexpr_containers
# error "Feature test macro for constexpr_containers is missing in <queue>"
#elif __cpp_lib_constexpr_containers != 202502L
# error "Feature test macro for constexpr containers has wrong value in <queue>"
#endif

#ifndef __cpp_lib_constexpr_queue
#error "Feature test macro for constexpr queue is missing in <queue>"
#elif __cpp_lib_constexpr_queue != 202502L
# error "Feature test macro for constexpr queue has wrong value in <queue>"
#endif

#include <ranges>
#include <functional>
#include <vector>
#include <testsuite_hooks.h>

template<typename T>
struct Alloc : std::allocator<T>
{
  using std::allocator<T>::allocator;

  int personality = 0;
  constexpr explicit Alloc (int p) : personality(p) { }

  template<typename U>
    constexpr Alloc(const Alloc<U>& a) : personality(a.personality) { }
}

namespace queue_tests {

constexpr bool ctor_tests()
{
  constexpr std::queue<int> q1;
  VERIFY(q1.size() == 0 && q1.empty());
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
  q4.push(1);
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
  a.pop();

  return true;
}

static_assert( push_and_pop_test() );

constexpr bool front_and_back_test()
{
  std::queue<int> a;
  a.push(2);
  a.push(4);
  static_assert ( a.front() == 2 && a.back() == 4);
}

static_assert( front_and_back_test() );

constexpr int push_range_test()
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

static_assert (push_range_test());

constexpr int swap_test()
{
  std::queue<int> a,b;
  a.push(1);
  b.push(2);
  std::swap(a, b);
  static_assert ( a.pop() - b.pop() == 1 );
  return true;
}

static_assert (swap_test());

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

} // end queue_tests

namespace priority_queue_tests {


constexpr bool ctor_tests()
{
  std::vector<int> v0 {0, 1, 2, 3};

  constexpr std::priority_queue<int> pq1;
  VERIFY(pq1.size() == 0 && pq1.empty());

  constexpr std::priority_queue<int> pq2 {std::less<int>()};
  VERIFY(pq2.size() == 0 && pq2.empty());

  constexpr std::priority_queue<int> pq3 {std::less<int>(), v0};
  VERIFY(pq3.size() == 4 && pq3.top() == 3);

  constexpr std::priority_queue<int> pq4 {std::less<int>(), std::move(v0)};
  VERIFY(pq4.size() == 4 && pq4.top() == 3);

  constexpr std::priority_queue<int> pq5 (pq3);
  VERIFY(pq5 == pq3);
  VERIFY(pq5.size() == pq3.size());

  constexpr std::priority_queue<int> pq6 (std::move(pq3));
  VERIFY(pq6 == pq5);
  VERIFY(pq6.size() == pq5.size());

  int rg[4] = {2, 3, 5, 7};
  std::vector<int> v1 {};

  std::priority_queue<int> pq10(std::begin(rg), std::end(rg),
			       std::less<int>());
  VERIFY(pq10.size() == std::size(rg));
  VERIFY(pq10.pop() == 2 && pq10.pop() == 3 && pq10.pop() == 5 && pq10.pop() == 7);

  std::priority_queue<int> pq11(std::begin(rg), std::end(rg),
			       std::less<int>(), v1);
  verify(pq11.size() == std::size(rg));
  verify(pq11.pop() == 2 && pq11.pop() == 3 && pq11.pop() == 5 && pq11.pop() == 7);

  std::priority_queue<int> pq12(std::begin(rg), std::end(rg),
			       std::less<int>(), std::move(v1));
  verify(pq12.size() == std::size(rg));
  verify(pq12.pop() == 2 && pq12.pop() == 3 && pq12.pop() == 5 && pq12.pop() == 7);

  auto pq13 = std::priority_queue(std::from_range_t, std::ranges::iota(0, 7), std::less<int>());
  VERIFY(pq13.size() == 7);

  return true;
}

static_assert( ctor_tests() );

constexpr bool alloc_aware_ctor_tests()
{
  constexpr std::allocator<int> alloc;
  int rg[4] = {2, 3, 5, 7};
  std::vector<int> v0 {};
  constexpr std::priority_queue<int> pq14 (alloc);
  VERIFY(pq14.get_allocator() == alloc);

  constexpr std::priority_queue<int> pq15 (std::less<int>(), alloc);
  VERIFY(pq15.get_allocator() == alloc);

  constexpr std::priority_queue<int> pq16 (std::less<int>(), v0, alloc);
  VERIFY(pq16.get_allocator() == alloc);

  constexpr std::priority_queue<int> pq17 (std::less<int>(), std::move(v0), alloc);
  VERIFY(pq17.get_allocator() == alloc);

  constexpr std::priority_queue<int> pq18 (pq17, alloc);
  VERIFY(pq18 == pq17);
  VERIFY(pq18.size() == pq17.size());
  VERIFY(pq18.get_allocator() == alloc);

  constexpr std::priority_queue<int> pq19 (std::move(pq17), alloc);
  VERIFY(pq19 == pq18);
  VERIFY(pq19.size() == pq18.size());
  VERIFY(pq19.get_allocator() == alloc);

  std::vector<int> v1 {};

  std::priority_queue<int> pq20(std::begin(rg), std::end(rg),
			       std::less<int>(), alloc);
  VERIFY(pq20.size() == std::size(rg));
  VERIFY(pq20.pop() == 2 && pq20.pop() == 3 && pq20.pop() == 5 && pq20.pop() == 7);

  std::priority_queue<int> pq21(std::begin(rg), std::end(rg),
			       std::less<int>(), v1, alloc);
  VERIFY(pq21.size() == std::size(rg));
  VERIFY(pq21.pop() == 2 && pq21.pop() == 3 && pq21.pop() == 5 && pq21.pop() == 7);

  std::priority_queue<int> pq22(std::begin(rg), std::end(rg),
			       std::less<int>(), std::move(v1), alloc);
  VERIFY(pq22.size() == std::size(rg));
  VERIFY(pq22.pop() == 2 && pq22.pop() == 3 && pq22.pop() == 5 && pq22.pop() == 7);

  auto pq23 = std::priority_queue(std::from_range_t, std::ranges::iota(0, 7), alloc);
  VERIFY(pq23.size() == 7);
  VERIFY(pq23.get_alloc() == alloc);

  auto pq24 = std::priority_queue(std::from_range_t, std::ranges::iota(0, 7), std::less<int>(), alloc);
  VERIFY(pq24.size() == 7);
  VERIFY(pq24.get_allocator() == aa);

  return true;
}

static_assert( alloc_aware_ctor_tests() );

constexpr bool push_and_pop_test()
{
  std::priority_queue<int> a;
  a.push(1);
  a.pop();

  return true;
}

static_assert( push_and_pop_test() );

constexpr bool top_test ()
{
  std::priority_queue<int> a;
  a.push(2);
  a.push(4);
  VERIFY (a.top() == 4 && a.pop() == 4);
}

static_assert( top_test() );

constexpr int push_range_test()
{
  std::priority_queue<int> a;
  const auto rg = {2, 3, 5, 7};
  a.push_range(rg);
  VERIFY (a.size() == 4);
  VERIFY (a.pop() == 7);
  VERIFY (a.pop() == 5);
  VERIFY (a.pop() == 3);
  VERIFY (a.pop() == 2);
  VERIFY (a.size() == 0);
  return true;
}

static_assert( push_range_test() );

constexpr int swap_test()
{
  std::priority_queue<int> a,b;
  a.push(2);
  b.push(4);
  std::swap(a, b);
  static_assert ( a.pop() - b.pop() == 2 );
  return true;
}

static_assert (swap_test());

struct S
{
  int foo;
  S(int i, int j) : foo{i + j} {}
  friend bool operator< (S const& x, S const& y) { return x.id < y.id; }
}

constexpr bool emplace_test()
{
  std::priority_queue<S> a;
  const S& s1 = a.emplace(0, 0);
  const S& s2 = a.emplace(1, 0);
  VERIFY (a.size() == 2);
  VERIFY (a.top() == s2);
  a.pop();
  VERIFY (a.top() == s1);
  return true;
}

static_assert( emplace_test() );

constexpr bool operator_test()
{
  std::priority_queue<int> a, b;
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

} // end priority_queue_test
