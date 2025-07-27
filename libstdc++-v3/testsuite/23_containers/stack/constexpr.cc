// { dg-do compile { target c++26 } }

// TODO: need to also add tests where theunderlying container is deque, list, and vector. maybe

#include <stack>

#ifndef __cpp_lib_constexpr_containers
# error "Feature test macro for constexpr_containers is missing in <stack>"
#elif __cpp_lib_constexpr_containers != 202502L
# error "Feature test macro for constexpr containers has wrong value in <stack>"
#endif

#ifndef __cpp_lib_constexpr_stack
#error "Feature test macro for constexpr stack is missing in <stack>"
#elif __cpp_lib_constexpr_stack != 202502L
# error "Feature test macro for constexpr stack has wrong value in <stack>"
#endif

#include <ranges>
#include <vector>
#include <testsuite_hooks.h>

template<typename T>
struct test_alloc : std::allocator<T>
{
  using std::allocator<T>::allocator;

  int personality = 0;
  constexpr explicit Alloc (int p) : personality(p) { }

  template<typename U>
    constexpr Alloc(const Alloc<U>& a) : personality(a.personality) { }
}

constexpr bool ctor_tests()
{
  constexpr std::stack<int> s1 ();
  VERIFY (s1.size() == 0);
  s1.push(1);
  VERIFY (s1.size() == 1);

  std::vector<int> v0 {2, 3, 5, 7};

  constexpr std::stack<int> s2 (v0);
  VERIFY (s2.size() == 4);
  VERIFY (s2.size() == s2.capacity());

  constexpr std::stack<int> s3 (std::move(v0));
  VERIFY (s3.size() == 4);
  VERIFY (s3.size() == s3.capacity());
  VERIFY (s3 == s2);

  constexpr std::stack<int> s4 (s1);
  VERIFY (s4 == s1);

  constexpr std::stack<int> s5 (std::move(s1));
  VERIFY (s3 == s2);

  auto il = {9, 11, 13, 17};
  constexpr std::stack<int> s6 (il.begin(), il.end());
  VERIFY (s6.size() == 4);
  VERIFY (s6.pop() == 9 && s6.pop() == 11 && s6.pop() == 13 && s6.pop() == 17);

  constexpr std::stack<int> s7 (std::from_range_t, std::ranges::iota(0, 7));
  VERIFY (s7.size() == 7);
}

constexpr bool alloc_aware_ctor_tests()
{
  test_alloc<int> alloc;

  constexpr std::stack<int> s8 ();
  VERIFY (s8.size() == 0);
  s8.push(1);
  VERIFY (s8.size() == 1);

  std::vector<int> v0 {2, 3, 5, 7};

  constexpr std::stack<int> s9 (v0, alloc);
  VERIFY (s9.size() == 4);
  VERIFY (s9.size() == s9.capacity());

  constexpr std::stack<int> s10 (std::move(v0), alloc);
  VERIFY (s10.size() == 4);
  VERIFY (s10.size() == s9.capacity());
  VERIFY (s10 == s9);

  constexpr std::stack<int> s11 (s8);
  VERIFY (s11 == s8);

  constexpr std::stack<int> s12 (std::move(s8), alloc);
  VERIFY (s12 == s8);

  auto il = {11, 13, 17, 19};
  constexpr std::stack<int> s13 (il.begin(), il.end(), alloc);
  VERIFY (s13.size() == 4);
  VERIFY (s13.pop() == 11 && s13.pop() == 13 && s13.pop() == 17 && s13.pop() == 19);

  constexpr std::stack<int> s14 (std::from_range_t, std::ranges::iota(0, 7), alloc);
  VERIFY (s14.size() == 7);

}

constexpr bool capacity_tests()
{
  constexpr std::stack<int> s0 ();
  VERIFY(s0.empty());
  s0.push(1);
  VERIFY(!s0.empty());
  VERIFY(s0.size() == 1);
  VERIFY(s0.pop() == 1);
  VERIFY(s0.size() == 0);

}

struct S
{
  int foo;
  S(int i, int j) : foo{i+j} {}
}

constexpr bool modifier_tests()
{
  constexpr std::stack<int> s0 ();
  s0.push(1);
  VERIFY (s0.pop() == 1);
  int x = 5;
  s0.push(std::move(x));
  VERIFY (s0.top() == 5);

  auto rg = {2, 3, 5, 7};
  s0.push_range(rg);
  VERIFY (s0.pop() = 7 && s0.pop() == 5 && s0.pop() == 3 && s0.pop() == 2);

  std::stack<S> s1;
  const S& struct0 = s1.emplace(0, 0);
  const S& struct1 = s1.emplace(0, 1);
  VERIFY(s1.pop().foo == 0);
  VERIFY(s1.pop().foo == 1);
}

constexpr bool nonmember_tests()
{
  std::stack<int> s0, s1;
  s0.push(2);
  s1.push(4);
  std::swap(s0, s1);
  VERIFY (s0.pop() - s1.pop() == 2);
}
