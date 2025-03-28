// { dg-options "-O2" }
// { dg-do run { target c++11 } }
// { dg-require-normal-mode "" }
// { dg-require-effective-target cxx11_abi }

// Copyright (C) 2015-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <list>
#include <iterator>
#include <testsuite_hooks.h>

__attribute__((__noinline__, __noclone__))
void testm(std::list<short>& l)
{
  bool b = std::distance(l.begin(), l.end()) == l.size();
  VERIFY( __builtin_constant_p(b) );
  VERIFY( b );
}

__attribute__((__noinline__, __noclone__))
void testc(const std::list<short>& l)
{
  bool b = std::distance(l.begin(), l.end()) == l.size();
  VERIFY( __builtin_constant_p(b) );
  VERIFY( b );
}

int main()
{
#if ! __NO_INLINE__
  std::list<short> l;
  testm(l);
  testc(l);
#endif
}
