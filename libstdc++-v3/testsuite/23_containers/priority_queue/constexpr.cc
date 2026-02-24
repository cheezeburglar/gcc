// { dg-do compile { target c++26 } }

#include <queue>

#ifndef __cpp_lib_constexpr_queue
# error "Feature test macro for __cpp_lib_constexpr_queue is missing in <queue>"
#elif __cpp_lib_constexpr_queue != 202502L
# error "Feature test macro for __cpp_lib_constexpr_queue has wrong value in <queue>"
#endif

#include <algorithm>
#include <ranges>
#include <functional>
#include <vector>
#include <numeric>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

#include <iostream>

struct Gt {
  template<typename T, typename U>
  constexpr bool operator()(T const& l, U const & r) {
    return l > r;
  }
};

template<typename Cont, typename Cmp = std::less<int>>
constexpr void ctor_tests(Cmp cmp = Cmp())
{
  using V = typename Cont::value_type;
  typename Cont::allocator_type alloc;


  V rg[] {2, 3, 5, 7};
  Cont v0 {std::from_range, rg};
  Cont v1 = v0;
  Cont v2 = v0;
  auto top_range = * std::ranges::max_element(v0, cmp);

  auto eq = [&] (std::priority_queue<V, Cont, Cmp>& l, std::span<V> r) {
    if (l.size() != r.size())
      return false;

    std::vector<V> s(r.begin(), r.end());
    std::ranges::sort(s, cmp);
    for (auto const& v : s | std::views::reverse) {
      if (v != l.top())
        return false;
      l.pop();
    }
    return true;
  };

  std::priority_queue<V, Cont, Cmp> pq1;
  VERIFY( pq1.size() == 0 && pq1.empty() );

  std::priority_queue<V, Cont, Cmp> pq2 {cmp};
  VERIFY( pq2.size() == 0 && pq2.empty() );

  std::priority_queue<V, Cont, Cmp> pq3 {cmp, v0};
  VERIFY( pq3.size() == 4 && pq3.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq4 {cmp, std::move(v0)};
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq5 (pq3);
  VERIFY( pq5.top() == pq3.top() );
  VERIFY( pq5.size() == pq3.size() );

  std::priority_queue<V, Cont, Cmp> pq6 (std::move(pq3));
  VERIFY( pq6.top() == pq5.top() );
  VERIFY( pq6.size() == pq5.size() );

  std::priority_queue<V, Cont, Cmp> pq10(std::begin(rg), std::end(rg),
					 cmp);
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq11(std::begin(rg), std::end(rg),
					 cmp, v0);
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq12(std::begin(rg), std::end(rg),
					 cmp, std::move(v0));
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq14 (alloc);
  VERIFY( pq14.empty() );

  std::priority_queue<V, Cont, Cmp> pq15 (cmp, alloc);
  VERIFY( pq15.empty() );

  std::priority_queue<V, Cont, Cmp> pq16 (cmp, v2, alloc);
  VERIFY( eq(pq16, {rg, 4}) );

  std::priority_queue<V, Cont, Cmp> pq17 (cmp, std::move(v2), alloc);
//  VERIFY( eq(pq17, {{2, 3, 5, 7}, 4}) );

  std::priority_queue<V, Cont, Cmp> pq18 (pq12, alloc);
  VERIFY( pq18.size() == pq12.size());

  std::priority_queue<V, Cont, Cmp> pq19 (std::move(pq12), alloc);
  VERIFY( pq19.size() == pq18.size() );
}


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

template<typename Range, typename Cont, typename Comp = std::less<int>>
constexpr void push_range_test(Comp comp = Comp())
{
  using T = std::ranges::range_value_t<Range>;
  using V = typename Cont::value_type;
  T rg[] {2, 3, 5, 7};

  std::vector<T> s(std::from_range, rg);
  std::ranges::sort(s, comp);

  std::priority_queue<V, Cont, Comp> pq;
  pq.push_range(Range(rg, rg+4));

  for (auto const& v : s | std::views::reverse) {
    VERIFY(v == pq.top());
    pq.pop();
  }
}

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

template<typename Container, typename Cmp>
constexpr void constructor_tests()
{

}

constexpr bool do_tests()
{
//  constructor_tests<>();
//  swap_test<>();
  emplace_test();
  return true;
}

template<typename Range, typename Cont, typename Cmp = std::less<int>>
constexpr bool range_tests(Cmp cmp = Cmp())
{
  using V = typename Cont::value_type;
  typename Cont::allocator_type alloc;

  using T = std::ranges::range_value_t<Range>;
  T rg[] {2, 3, 5, 7, 11, 13};

  auto top_range = std::ranges::max_element(rg, cmp);

  auto eq = [&](std::priority_queue<V, Cont, Cmp>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;

    std::vector<T> s(r.begin(), r.end());
    std::ranges::sort(s, cmp);
    for (auto const& v : s | std::views::reverse) {
      if (v != l.top())
        return false;
      l.pop();
    }
    return true;
  };

  Cont v0 {std::from_range, rg};

  std::priority_queue<V, Cont, Cmp> pq1;
  VERIFY( pq1.size() == 0 && pq1.empty() );

  std::priority_queue<V, Cont, Cmp> pq2 {cmp};
  VERIFY( pq2.size() == 0 && pq2.empty() );

  std::priority_queue<V, Cont, Cmp> pq3 {cmp, v0};
  VERIFY( pq3.size() == 6 && pq3.top() == *top_range );

  std::priority_queue<V, Cont, Cmp> pq4 {cmp, std::move(v0)};
  VERIFY( pq4.size() == 6 && pq4.top() == *top_range );

  std::priority_queue<V, Cont, Cmp> pq5 (pq3);
  VERIFY( pq5.top() == pq3.top() );
  VERIFY( pq5.size() == pq3.size() );

  std::priority_queue<V, Cont, Cmp> pq6 (std::move(pq3));
  VERIFY( pq6.top() == pq5.top() );
  VERIFY( pq6.size() == pq5.size() );

  Cont v1 {v0};

  std::priority_queue<V, Cont, Cmp> pq10(std::begin(rg), std::end(rg),
					 cmp);
  VERIFY( eq(pq10, {rg, 6}) );

  std::priority_queue<V, Cont, Cmp> pq11(std::begin(rg), std::end(rg),
					 cmp, v1);
  VERIFY( eq(pq11, {rg, 6}) );

  std::priority_queue<V, Cont, Cmp> pq12(std::begin(rg), std::end(rg),
					 cmp, std::move(v1));
  VERIFY( eq(pq12, {rg, 6}) );

  std::priority_queue<V, Cont, Cmp> pq13 (std::from_range, Range(rg, rg+4),
					  cmp);
  VERIFY( eq(pq13, {rg, 4}) );

  std::priority_queue<V, Cont, Cmp> pq14 (alloc);
  VERIFY( pq14.empty() );

  std::priority_queue<V, Cont, Cmp> pq15 (cmp, alloc);
  VERIFY( pq15.empty() );

  Cont v2 (std::from_range, Range(rg, rg+2), alloc);

  std::priority_queue<V, Cont, Cmp> pq16 (cmp, v2, alloc);
  VERIFY( eq(pq16, {rg, 2}) );

  std::priority_queue<V, Cont, Cmp> pq17 (cmp, std::move(v2), alloc);
  VERIFY( eq(pq17, {rg, 2}) );

  std::priority_queue<V, Cont, Cmp> pq18 (pq13, alloc);
  VERIFY( pq18.size() == pq13.size());

  std::priority_queue<V, Cont, Cmp> pq19 (std::move(pq13), alloc);
  VERIFY( pq19.size() == pq18.size() );

  Cont v3 {};

  std::priority_queue<V, Cont, Cmp> pq20(std::begin(rg), std::end(rg),
					 cmp, alloc);
  VERIFY( eq(pq20, {rg, 6}) );

  std::priority_queue<V, Cont, Cmp> pq21(std::begin(rg), std::end(rg),
			        cmp, v3, alloc);
  VERIFY( eq(pq21, {rg, 6}) );

  std::priority_queue<V, Cont, Cmp> pq22(std::begin(rg), std::end(rg),
					 cmp, std::move(v3), alloc);
  VERIFY( eq(pq22, {rg, 6}) );

  auto pq23 = std::priority_queue<V, Cont, Cmp>(std::from_range, Range(rg, rg+6), alloc);
  VERIFY( eq(pq23, {rg, 6}) );

  auto pq24 = std::priority_queue<V, Cont, Cmp>(std::from_range, Range(rg, rg+6), cmp, alloc);
  VERIFY( eq(pq24, {rg, 6}) );

  std::priority_queue<V, Cont, Cmp> pq;
  pq.push_range(Range(rg, rg+6));
  VERIFY( eq(pq, {rg, 6}) );

  return true;
}

template<typename Range, typename Cont>
constexpr void do_ranges_tests_b()
{
  range_tests<Range,
	      Cont>();
  range_tests<Range,
	      Cont,
	      Gt>();
}

template<typename Cont>
constexpr void do_ranges_tests_a() {
  using T = typename Cont::value_type;
  using namespace __gnu_test;
  do_ranges_tests_b<test_forward_range<T>,
	     Cont>();
  do_ranges_tests_b<test_forward_sized_range<T>,
	     Cont>();
  do_ranges_tests_b<test_sized_range_sized_sent<T, forward_iterator_wrapper>,
	     Cont>();

  do_ranges_tests_b<test_input_range<T>,
	     Cont>();
  do_ranges_tests_b<test_input_sized_range<T>,
	     Cont>();
  do_ranges_tests_b<test_sized_range_sized_sent<T, forward_iterator_wrapper>,
	      Cont>();

  do_ranges_tests_b<test_range<T, input_iterator_wrapper_nocopy>,
	     Cont>();
  do_ranges_tests_b<test_sized_range<T, input_iterator_wrapper_nocopy>,
	     Cont>();
  do_ranges_tests_b<test_sized_range_sized_sent<T, input_iterator_wrapper_nocopy>,
	     Cont>();
}

constexpr void do_ranges_tests()
{
  using namespace __gnu_test;
  do_ranges_tests_a<std::vector<int>>();
  do_ranges_tests_a<std::vector<int, SimpleAllocator<int>>>();
  do_ranges_tests_a<std::deque<int>>();
  do_ranges_tests_a<std::deque<int, SimpleAllocator<int>>>();
}

template <typename Cont>
constexpr void do_ctor_tests_a()
{
  ctor_tests<Cont>();
  ctor_tests<Cont, Gt>();
}

constexpr void do_ctor_tests()
{
  using namespace __gnu_test;
  do_ctor_tests_a<std::vector<int>>();
  do_ctor_tests_a<std::vector<int, SimpleAllocator<int>>>();
  do_ctor_tests_a<std::deque<int>>();
  do_ctor_tests_a<std::deque<int, SimpleAllocator<int>>>();
}

constexpr bool do_all_tests()
{
  do_ctor_tests();
  push_and_pop_test();
  top_test();
  swap_test();
  emplace_test();

  do_ranges_tests();

  return true;
}

static_assert (do_all_tests());

int main()
{
  static_assert( do_all_tests() );
  do_all_tests();
}
