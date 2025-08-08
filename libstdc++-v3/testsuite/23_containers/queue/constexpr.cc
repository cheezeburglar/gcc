// { dg-do compile { target c++26 } }

#include <queue>

#ifndef __cpp_lib_constexpr_queue
#error "Feature test macro for constexpr queue is missing in <queue>"
#elif __cpp_lib_constexpr_queue != 202502L
# error "Feature test macro for constexpr queue has wrong value in <queue>"
#endif

#include <ranges>
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
  std::queue<int> q1;
  VERIFY( q1.size() == 0 && q1.empty() );
  q1.push(1);
  q1.push(2);
  VERIFY( q1.size() == 2) ;

  std::queue<int> q2 (q1);
  VERIFY( q2.size() == q1.size() );
  VERIFY( q2.front() == q1.front() );
  VERIFY( q2.back() == q1.back() );

  std::queue<int> q3 (std::move(q2));
  VERIFY( q3.size() == q1.size() );
  VERIFY( q3.front() == q1.front() );
  VERIFY( q3.back() == q1.back() );
  VERIFY( q2.empty() );

  std::allocator<int> alloc;
  std::queue<int> q4 (alloc);
  q4.push(1);
  q4.push(2);
  VERIFY( q4.size() == 2 );

  std::queue<int> q5 (q4, alloc);
  VERIFY( q5 == q4 );
  VERIFY( q5.size() == q4.size() );
  VERIFY( q5.front() == q4.front() );
  VERIFY( q5.back() == q4.back() );

  std::queue<int> q6 (std::move(q5), alloc);
  VERIFY( q6 == q4 );
  VERIFY( q6.size() == q4.size() );
  VERIFY( q5.empty() );

  Alloc<int> aa(5);
  std::queue<int> q7 (aa);
  VERIFY( q7.size() == 0 );

  int rg[4] = {2, 3, 5, 7};
  std::queue<int> q8(std::begin(rg), std::end(rg));
  VERIFY( q8.size() == std::size(rg));
  VERIFY( q8.front() == 2 );
  q8.pop();
  VERIFY( q8.front() == 3 );
  q8.pop();
  VERIFY( q8.front() == 5 );
  q8.pop();
  VERIFY( q8.front() == 7 );
  q8.pop();

  std::queue<int> q9(std::begin(rg), std::end(rg), aa);
  VERIFY( q9.size() == std::size(rg));
  VERIFY( q9.front() == 2 );
  q9.pop();
  VERIFY( q9.front() == 3 );
  q9.pop();
  VERIFY( q9.front() == 5 );
  q9.pop();
  VERIFY( q9.front() == 7 );
  q9.pop();

  auto q10 = std::queue<int>(std::from_range, std::ranges::views::iota(0, 7));
  VERIFY( q10.size() == 7 );

  auto q11 = std::queue<int>(std::from_range, std::ranges::views::iota(0, 7), alloc);
  VERIFY( q11.size() == 7 );

  auto q12 = std::queue<int>(std::from_range, std::ranges::views::iota(0, 7), aa);
  VERIFY( q12.size() == 7 );

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
  VERIFY ( a.front() == 2 && a.back() == 4);
  return true;
}

static_assert( front_and_back_test() );

constexpr int push_range_test()
{
  std::queue<int> a;
  const auto rg = {2, 3, 5, 7};
  a.push_range(rg);
  VERIFY( a.size() == 4 );
  VERIFY( a.front() == 2 );
  a.pop();
  VERIFY( a.front() == 3 );
  a.pop();
  VERIFY( a.front() == 5 );
  a.pop();
  VERIFY( a.front() == 7 );
  a.pop();
  VERIFY( a.size() == 0 );
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
  VERIFY ( a.size() == 1 );
  VERIFY ( a.front().foo == 196884 );
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
