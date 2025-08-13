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

constexpr bool ctor_tests()
{
  auto rg = {2, 3, 5, 7};
  std::vector<int> v0 {std::from_range, rg};

  std::priority_queue<int> pq1;
  VERIFY( pq1.size() == 0 && pq1.empty() );

  std::priority_queue<int> pq2 {std::less<int>()};
  VERIFY( pq2.size() == 0 && pq2.empty() );

  std::priority_queue<int> pq3 {std::less<int>(), v0};
  VERIFY( pq3.size() == 4 && pq3.top() == 7 );

  std::priority_queue<int> pq4 {std::less<int>(), std::move(v0)};
  VERIFY( pq4.size() == 4 && pq4.top() == 7 );

  std::priority_queue<int> pq5 (pq3);
  VERIFY( pq5.top() == pq3.top() );
  VERIFY( pq5.size() == pq3.size() );

  std::priority_queue<int> pq6 (std::move(pq3));
  VERIFY( pq6.top() == pq5.top() );
  VERIFY( pq6.size() == pq5.size() );

  std::vector<int> v1 {};

  std::priority_queue<int> pq10(std::begin(rg), std::end(rg),
			       std::less<int>());
  VERIFY( pq10.size() == std::size(rg) );
  VERIFY( pq10.top() == 7 );
  pq10.pop();
  VERIFY( pq10.top() == 5 );
  pq10.pop();
  VERIFY( pq10.top() == 3 );
  pq10.pop();
  VERIFY( pq10.top() == 2 );
  pq10.pop();

  std::priority_queue<int> pq11(std::begin(rg), std::end(rg),
			       std::less<int>(), v1);
  VERIFY( pq11.size() == std::size(rg));
  VERIFY( pq11.top() == 7 );
  pq11.pop();
  VERIFY( pq11.top() == 5 );
  pq11.pop();
  VERIFY( pq11.top() == 3 );
  pq11.pop();
  VERIFY( pq11.top() == 2 );
  pq11.pop();

  std::priority_queue<int> pq12(std::begin(rg), std::end(rg),
			       std::less<int>(), std::move(v1));
  VERIFY( pq12.size() == std::size(rg));
  VERIFY( pq12.top() == 7 );
  pq12.pop();
  VERIFY( pq12.top() == 5 );
  pq12.pop();
  VERIFY( pq12.top() == 3 );
  pq12.pop();
  VERIFY( pq12.top() == 2 );
  pq12.pop();

  auto pq13 = std::priority_queue<int>(std::from_range, std::ranges::views::iota(0, 7), std::less<int>());
  VERIFY( pq13.size() == 7 );

  return true;
}

static_assert( ctor_tests() );

constexpr bool alloc_aware_ctor_tests()
{
  std::allocator<int> alloc;
  auto rg = {1, 0};
  constexpr std::vector<int> v0 {};
  std::priority_queue<int> pq14 (alloc);

  std::priority_queue<int> pq15 (std::less<int>(), alloc);
  pq15.push(0);
  pq15.push(1);
  VERIFY( pq15.top() == 1 );

  std::priority_queue<int> pq16 (std::less<int>(), v0, alloc);
  pq16.push(0);
  pq16.push(1);
  VERIFY( pq16.top() == 1 );

  std::priority_queue<int> pq17 (std::less<int>(), std::move(v0), alloc);
  pq17.push(0);
  pq17.push(1);
  VERIFY( pq17.top() == 1 );

  std::priority_queue<int> pq18 (pq17, alloc);
  VERIFY( pq18.size() == pq17.size());
  VERIFY( pq18.top() == 1 );

  std::priority_queue<int> pq19 (std::move(pq17), alloc);
  VERIFY( pq19.size() == pq18.size() );
  VERIFY( pq18.top() == 1 );

  constexpr std::vector<int> v1 {};

  std::priority_queue<int> pq20(std::begin(rg), std::end(rg),
			       std::less<int>(), alloc);
  VERIFY( pq20.top() == 1 );
  VERIFY( pq20.size() == std::size(rg) );

  std::priority_queue<int> pq21(std::begin(rg), std::end(rg),
			       std::less<int>(), v1, alloc);
  VERIFY( pq21.top() == 1 );
  VERIFY( pq21.size() == std::size(rg) );

  std::priority_queue<int> pq22(std::begin(rg), std::end(rg),
			       std::less<int>(), std::move(v1), alloc);
  VERIFY( pq21.top() == 1 );
  VERIFY( pq22.size() == std::size(rg) );

  auto pq23 = std::priority_queue<int>(std::from_range, std::ranges::views::iota(0, 7), alloc);
  VERIFY( pq23.size() == 7 );

  auto pq24 = std::priority_queue<int>(std::from_range, std::ranges::views::iota(0, 7), std::less<int>(), alloc);
  VERIFY( pq24.size() == 7 );

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
  VERIFY( a.top() == 4 );

  return true;
}

static_assert( top_test() );

constexpr int push_range_test()
{
  std::priority_queue<int> pq;
  const auto rg = {2, 3, 5, 7};
  pq.push_range(rg);
  VERIFY( pq.size() == 4);
  VERIFY( pq.top() == 7);
  pq.pop();
  VERIFY( pq.top() == 5);
  pq.pop();
  VERIFY( pq.top() == 3);
  pq.pop();
  VERIFY( pq.top() == 2);
  pq.pop();
  VERIFY( pq.size() == 0);
  return true;
}

static_assert( push_range_test() );

constexpr int swap_test()
{
  std::priority_queue<int> a,b;
  a.push(2);
  b.push(4);
  std::swap(a, b);
  VERIFY( a.top() == 4 );
  VERIFY( b.top() == 2 );
  return true;
}

static_assert (swap_test());

struct S
{
  int foo;
  constexpr S(int i, int j) : foo{i + j} {}
  constexpr friend bool operator< (S const &x, S const &y) { return x.foo < y.foo; }
};

constexpr bool emplace_test()
{
  std::priority_queue<S> pq;
  pq.emplace(0, 0);
  pq.emplace(1, 0);
  VERIFY( pq.size() == 2 );
  VERIFY( pq.top().foo == 1 );
  pq.pop();
  VERIFY ( pq.top().foo == 0 );
  return true;
}

static_assert( emplace_test() );
