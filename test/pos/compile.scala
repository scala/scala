//############################################################################
// Compile Time Bugs & Test Cases
//############################################################################
// $Id$

import java.lang.System; // to avoid name clash with .NET's library

//############################################################################
// Test 0

class Test0Foo[X];

object Test0Test {
  type Gen[A] = Test0Foo[A];
  class Tic(g: Test0Test.Gen[Int]);
  class Tac(g:           Gen[Int]);
}

//############################################################################
