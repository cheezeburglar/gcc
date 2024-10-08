// { dg-options "-std=c++17 -fchar8_t" }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cstdint>
#include <cassert>

int operator"" _foo(const char*)                  { return 0; }
int operator"" _foo(unsigned long long int)       { return 1; }
int operator"" _foo(long double)                  { return 2; }
int operator"" _foo(char)                         { return 3; }
int operator"" _foo(wchar_t)                      { return 4; }
int operator"" _foo(char8_t)                      { return 5; }
int operator"" _foo(char16_t)                     { return 6; }
int operator"" _foo(char32_t)                     { return 7; }
int operator"" _foo(const char*, std::size_t)     { return 8; }
int operator"" _foo(const wchar_t*, std::size_t)  { return 9; }
int operator"" _foo(const char8_t*, std::size_t)  { return 10; }
int operator"" _foo(const char16_t*, std::size_t) { return 11; }
int operator"" _foo(const char32_t*, std::size_t) { return 12; }
template<char...> int operator"" _foo2()          { return 20; }
int operator"" _foo2(unsigned long long int)      { return 21; }

int
main()
{
  assert(123_foo == 1);
  assert(0.123_foo == 2);
  assert('c'_foo == 3);
  assert(L'c'_foo == 4);
  assert(u8'c'_foo == 5);
  assert(u'c'_foo == 6);
  assert(U'c'_foo == 7);
  assert("abc"_foo == 8);
  assert(L"abc"_foo == 9);
  assert(u8"abc"_foo == 10);
  assert(u"abc"_foo == 11);
  assert(U"abc"_foo == 12);
  assert(123_foo2 == 21);
}
