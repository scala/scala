//############################################################################
// Compile Time Bugs & Test Cases
//############################################################################

import java.lang.System; // to avoid name clash with .NET's library

//############################################################################
// Test 0

class Test2_2(i: Int) {
  {
    val t = {
      val x = {
        val y = {
          val z = i;
          z;
        };
      };
    };
    val x = {
      val y = {
        val z = i;
        z;
      };
    };
    val y = {
      val z = i;
      z;
    };
    val z2_2 = i;
    0
  }
}

