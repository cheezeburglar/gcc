// { dg-do compile { target c++26 } }

#include <queue>

#ifndef __cpp_lib_constexpr_queue
# error "Feature test macro for __cpp_lib_constexpr_queue is missing in <queue>"
#elif __cpp_lib_constexpr_queue != 202502L
# error "Feature test macro for __cpp_lib_constexpr_queue has wrong value in <queue>"
#endif

#include <ranges>
#include <functional>
#include <vector>
#include <numeric>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

template<typename Container>
constexpr void ctor_tests()
{
  using Tp = typename Container::value_type;
  typename Container::allocator_type Alloc;

  Container c0 {};
  auto alloc = c0.get_allocator();

  std::queue<Tp, Container> q1 (c0);
  VERIFY( q1.size() == 0 && q1.empty() );
  q1.push(1);
  q1.push(2);
  VERIFY( q1.size() == 2) ;

  Container c1 {1, 2};
  std::queue<Tp, Container> q_0 {c1};
  VERIFY ( q_0 == q1 );
  std::queue<Tp, Container> q_1 {std::move(c1)};
  VERIFY ( q_1 == q1 );


  std::queue<Tp, Container> q2 (q1);
  VERIFY( q2.size() == q1.size() );
  VERIFY( q2.front() == q1.front() );
  VERIFY( q2.back() == q1.back() );

  std::queue<Tp, Container> q3 (std::move(q2));
  VERIFY( q3.size() == q1.size() );
  VERIFY( q3.front() == q1.front() );
  VERIFY( q3.back() == q1.back() );
  VERIFY( q2.empty() );

  std::queue<Tp, Container, Alloc> q4 (alloc);
  q4.push(1);
  q4.push(2);
  VERIFY( q4.size() == 2 );

  std::queue<Tp, Container, Alloc> q5 (q4, alloc);
  VERIFY( q5 == q4 );
  VERIFY( q5.size() == q4.size() );
  VERIFY( q5.front() == q4.front() );
  VERIFY( q5.back() == q4.back() );

  std::queue<Tp, Container, Alloc> q6 (std::move(q5), alloc);
  VERIFY( q6 == q4 );
  VERIFY( q6.size() == q4.size() );
  VERIFY( q5.empty() );

  std::queue<Tp, Container, Alloc> q7 (alloc);
  VERIFY( q7.size() == 0 );

  Tp rg[4] = {2, 3, 5, 7};
  std::queue<Tp, Container> q8(std::begin(rg), std::end(rg));
  VERIFY( q8.size() == std::size(rg));
  VERIFY( q8.front() == 2 );
  q8.pop();
  VERIFY( q8.front() == 3 );
  q8.pop();
  VERIFY( q8.front() == 5 );
  q8.pop();
  VERIFY( q8.front() == 7 );
  q8.pop();

  std::queue<Tp, Container, Alloc> q9(std::begin(rg), std::end(rg), alloc);
  VERIFY( q9.size() == std::size(rg));
  VERIFY( q9.front() == 2 );
  q9.pop();
  VERIFY( q9.front() == 3 );
  q9.pop();
  VERIFY( q9.front() == 5 );
  q9.pop();
  VERIFY( q9.front() == 7 );
  q9.pop();

  auto rg0 = {2, 3, 5, 7};
  auto test_q0 = rg0 | std::ranges::to<std::queue<Container>>();
  auto q10 = std::queue<Tp, Container>(std::from_range,
				       std::ranges::views::iota(0, 7));
  VERIFY( q10.size() == 7 );

  auto q11 = std::queue<Tp, Container, Alloc>(std::from_range,
					      std::ranges::views::iota(0, 7),
					      alloc);
  VERIFY( q11.size() == 7 );
}

template<typename Range, typename Alloc>
constexpr void
do_ranges_tests_a()
{
  using Tp = std::ranges::range_value_t<Range>;
  Alloc alloc;
  Tp a[] {2, 3, 5, 7};

  auto eq = [&] (std::queue<Tp> l, std::queue<Tp> r) {
    if (l.size() != r.size())
      return false;

    while (!l.empty()) {
      if (l.front() != r.front())
	return false;
      l.pop();
      r.pop();
    }
    return true;
  };

//  auto q0 = Range(a, a+2) | std::ranges::to<std::queue>();
  auto q1 = std::queue<Tp>(std::from_range, Range(a, a+4));

  std::queue<Tp> q2;
  q2.push_range(Range(a, a+4));
  VERIFY( eq (q1, q2) );
}

template<typename Alloc>
constexpr void
do_ranges_tests()
{
  using namespace __gnu_test;
  using Tp = std::allocator_traits<Alloc>::value_type;

  do_ranges_tests_a<test_forward_range<Tp>,
			     Alloc>();
  do_ranges_tests_a<test_forward_sized_range<Tp>,
			     Alloc>();
  do_ranges_tests_a<test_sized_range_sized_sent
		 <Tp, forward_iterator_wrapper>,
	       Alloc>();

  do_ranges_tests_a<test_input_range<Tp>, Alloc>();
  do_ranges_tests_a<test_input_sized_range<Tp>, Alloc>();
  do_ranges_tests_a<test_sized_range_sized_sent<Tp, forward_iterator_wrapper>,
	       Alloc>();

  do_ranges_tests_a<test_range<Tp, input_iterator_wrapper_nocopy>,
	       Alloc>();
  do_ranges_tests_a<test_sized_range<Tp, input_iterator_wrapper_nocopy>,
	       Alloc>();
  do_ranges_tests_a<test_sized_range_sized_sent<Tp, input_iterator_wrapper_nocopy>,
	       Alloc>();
}

constexpr void push_and_pop_test()
{
  std::queue<int> a;
  a.push(1);
  a.pop();
}

constexpr void push_range_test()
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
}

constexpr void swap_test()
{
  std::queue<int> a,b;
  a.push(1);
  b.push(2);
  std::swap(a, b);
  VERIFY( a.front() == 2 );
  VERIFY( b.front() == 1 );
}

struct S
{
  int foo;
  constexpr S(int i, int j) : foo{i + j} {}
};

constexpr void emplace_test()
{
  std::queue<S> a;
  const S& s = a.emplace(196883, 1);
  VERIFY ( a.size() == 1 );
  VERIFY ( a.front().foo == 196884 );
}

constexpr void element_access_tests()
{
  std::queue<int> a;
  a.push(2);
  a.push(4);
  VERIFY ( a.front() == 2 && a.back() == 4);
}

constexpr void operator_test()
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
}

constexpr bool
do_tests()
{
  using namespace __gnu_test;

  auto do_modifier_tests = []() {
    push_and_pop_test();
    push_range_test();
    swap_test();
    emplace_test();
  };

  // TODO: Check also list when made constexpr.
  auto do_ctor_tests = []() {
    ctor_tests<std::deque<int>>();
    ctor_tests<std::deque<int, SimpleAllocator<int>>>();
  }

  do_ctor_tests();
  do_modifier_tests();
  element_access_tests();
  operator_test();

  // Additional code coverage
  do_ranges_tests<std::allocator<int>>();
  do_ranges_tests<SimpleAllocator<int>>();
  return true;
}

int main() {
  do_tests();
  static_assert( do_tests() );
}
