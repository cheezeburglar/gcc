// { dg-do compile { target c++26 } }

#include <queue>

#ifndef __cpp_lib_constexpr_queue
#error "Feature test macro for constexpr queue is missing in <queue>"
#elif __cpp_lib_constexpr_queue != 202502L
# error "Feature test macro for constexpr queue has wrong value in <queue>"
#endif

#include <ranges>
#include <functional>
#include <vector>
#include <numeric>
#include <testsuite_hooks.h>

template<typename T>
struct Alloc : std::allocator<T>
{
  using std::allocator<T>::allocator;

  int personality = 0;
  constexpr explicit Alloc (int p) : personality(p) { }

  template<typename U>
    constexpr Alloc(const Alloc<U>& a) : personality(a.personality) { }
};

namespace queue_tests {

constexpr bool ctor_tests()
{
  std::queue<int> q1;
  VERIFY(q1.size() == 0 && q1.empty());
  q1.push(1);
  q1.push(2);
  VERIFY(q1.size() == 2);

  std::queue<int> q2 (q1);
  VERIFY(q2.size() == q1.size());
  VERIFY(q2.front() == q1.front());
  VERIFY(q2.back() == q1.back());

  std::queue<int> q3 (std::move(q2));
  VERIFY(q3.size() == q1.size());
  VERIFY(q3.front() == q1.front());
  VERIFY(q3.back() == q1.back());
  VERIFY(q2.empty());

  std::allocator<int> alloc;
  std::queue<int> q4 (alloc);
  q4.push(1);
  q4.push(2);
  VERIFY(q4.size() == 2);

  std::queue<int> q5 (q4, alloc);
  VERIFY(q5 == q4);
  VERIFY(q5.size() == q4.size());
  VERIFY(q5.front() == q4.front());
  VERIFY(q5.back() == q4.back());

  std::queue<int> q6 (std::move(q5), alloc);
  VERIFY(q6 == q4);
  VERIFY(q6.size() == q4.size());
  VERIFY(q5.empty());

  Alloc<int> aa(5);
  std::queue<int> q7 (aa);
  VERIFY(q7.size() == 0);

  int rg[4] = {2, 3, 5, 7};
  std::queue<int> q8(std::begin(rg), std::end(rg));
  VERIFY(q8.size() == std::size(rg));
  VERIFY(q8.front() == 2);
  q8.pop();
  VERIFY(q8.front() == 3);
  q8.pop();
  VERIFY(q8.front() == 5);
  q8.pop();
  VERIFY(q8.front() == 7);
  q8.pop();

  std::queue<int> q9(std::begin(rg), std::end(rg), aa);
  VERIFY(q9.size() == std::size(rg));
  VERIFY(q9.front() == 2);
  q9.pop();
  VERIFY(q9.front() == 3);
  q9.pop();
  VERIFY(q9.front() == 5);
  q9.pop();
  VERIFY(q9.front() == 7);
  q9.pop();

  auto q10 = std::queue<int>(std::from_range, std::ranges::views::iota(0, 7));
  VERIFY(q10.size() == 7);

  auto q11 = std::queue<int>(std::from_range, std::ranges::views::iota(0, 7), alloc);
  VERIFY(q11.size() == 7);

  auto q12 = std::queue<int>(std::from_range, std::ranges::views::iota(0, 7), aa);
  VERIFY(q12.size() == 7);

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
  return true;
}

static_assert( front_and_back_test() );

constexpr int push_range_test()
{
  std::queue<int> a;
  const auto rg = {2, 3, 5, 7};
  a.push_range(rg);
  VERIFY (a.size() == 4);
  VERIFY (a.front() == 2);
  a.pop();
  VERIFY (a.front() == 3);
  a.pop();
  VERIFY (a.front() == 5);
  a.pop();
  VERIFY (a.front() == 7);
  a.pop();
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
  VERIFY( a.front() == 2 );
  VERIFY( b.front() == 1 );
  return true;
}

static_assert (swap_test());

struct S
{
  int foo;
  constexpr S(int i, int j) : foo{i + j} {}
};

constexpr bool emplace_test()
{
  std::queue<S> a;
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
  VERIFY(pq5.top() == pq3.top());
  VERIFY(pq5.size() == pq3.size());

  constexpr std::priority_queue<int> pq6 (std::move(pq3));
  VERIFY(pq6.top() == pq5.top());
  VERIFY(pq6.size() == pq5.size());

  int rg[4] = {2, 3, 5, 7};
  constexpr std::vector<int> v1 {};

  std::priority_queue<int> pq10(std::begin(rg), std::end(rg),
			       std::less<int>());
  VERIFY(pq10.size() == std::size(rg));
  VERIFY(pq10.top() == 7);
  pq10.pop();
  VERIFY(pq10.top() == 5);
  pq10.pop();
  VERIFY(pq10.top() == 3);
  pq10.pop();
  VERIFY(pq10.top() == 2);
  pq10.pop();

  std::priority_queue<int> pq11(std::begin(rg), std::end(rg),
			       std::less<int>(), v1);
  VERIFY(pq11.size() == std::size(rg));
  VERIFY(pq11.top() == 7);
  pq11.pop();
  VERIFY(pq11.top() == 5);
  pq11.pop();
  VERIFY(pq11.top() == 3);
  pq11.pop();
  VERIFY(pq11.top() == 2);
  pq11.pop();

  std::priority_queue<int> pq12(std::begin(rg), std::end(rg),
			       std::less<int>(), std::move(v1));
  VERIFY(pq11.size() == std::size(rg));
  VERIFY(pq11.top() == 7);
  pq11.pop();
  VERIFY(pq11.top() == 5);
  pq11.pop();
  VERIFY(pq11.top() == 3);
  pq11.pop();
  VERIFY(pq11.top() == 2);
  pq11.pop();

  auto pq13 = std::priority_queue<int>(std::from_range, std::ranges::views::iota(0, 7), std::less<int>());
  VERIFY(pq13.size() == 7);

  return true;
}

static_assert( ctor_tests() );

constexpr bool alloc_aware_ctor_tests()
{
  std::allocator<int> alloc;
  auto rg = {0, 1};
  constexpr std::vector<int> v0 {};
  std::priority_queue<int> pq14 (alloc);

  std::priority_queue<int> pq15 (std::less<int>(), alloc);
  pq15.push(0);
  pq15.push(1);
//  VERIFY(pq15.top() == 0);

  std::priority_queue<int> pq16 (std::less<int>(), v0, alloc);
  pq16.push(0);
  pq16.push(1);
  VERIFY(pq16.top() == 0);

  std::priority_queue<int> pq17 (std::less<int>(), std::move(v0), alloc);
  pq17.push(0);
  pq17.push(1);
  VERIFY(pq17.top() == 0);

  std::priority_queue<int> pq18 (pq17, alloc);
  VERIFY(pq18.size() == pq17.size());
  VERIFY(pq18.top() == 0);

  std::priority_queue<int> pq19 (std::move(pq17), alloc);
  VERIFY(pq19.size() == pq18.size());
  VERIFY(pq18.top() == 0);

  constexpr std::vector<int> v1 {};

  std::priority_queue<int> pq20(std::begin(rg), std::end(rg),
			       std::less<int>(), alloc);
  VERIFY(pq20.top() == 0);
  VERIFY(pq20.size() == std::size(rg));

  std::priority_queue<int> pq21(std::begin(rg), std::end(rg),
			       std::less<int>(), v1, alloc);
  VERIFY(pq21.top() == 0);
  VERIFY(pq21.size() == std::size(rg));

  std::priority_queue<int> pq22(std::begin(rg), std::end(rg),
			       std::less<int>(), std::move(v1), alloc);
  VERIFY(pq21.top() == 0);
  VERIFY(pq22.size() == std::size(rg));

  auto pq23 = std::priority_queue<int>(std::from_range, std::ranges::views::iota(0, 7), alloc);
  VERIFY(pq23.size() == 7);

  auto pq24 = std::priority_queue<int>(std::from_range, std::ranges::views::iota(0, 7), std::less<int>(), alloc);
  VERIFY(pq24.size() == 7);

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
  VERIFY (a.top() == 4);

  return true;
}

static_assert( top_test() );

constexpr int push_range_test()
{
  std::priority_queue<int> pq;
  const auto rg = {2, 3, 5, 7};
  pq.push_range(rg);
  VERIFY (pq.size() == 4);
  VERIFY (pq.top() == 7);
  pq.pop();
  VERIFY (pq.top() == 5);
  pq.pop();
  VERIFY (pq.top() == 3);
  pq.pop();
  VERIFY (pq.top() == 2);
  pq.pop();
  VERIFY (pq.size() == 0);
  return true;
}

static_assert( push_range_test() );

constexpr int swap_test()
{
  std::priority_queue<int> a,b;
  a.push(2);
  b.push(4);
  std::swap(a, b);
  static_assert ( a.top() == 4 );
  static_assert ( b.top() == 2 );
  return true;
}

static_assert (swap_test());

constexpr bool emplace_test()
{

  struct S
  {
    int foo;
    constexpr S(int i, int j) : foo{i + j} {}
    constexpr friend bool operator< (S const &x, S const &y) { return x.foo < y.foo; }
  };
  std::priority_queue<S> pq;
  pq.emplace(0, 0);
  pq.emplace(1, 0);
  VERIFY (pq.size() == 2);
  VERIFY (pq.top().foo == 1);
  pq.pop();
  VERIFY (pq.top().foo == 0);
  return true;
}

static_assert( emplace_test() );
} // end priority_queue_test
