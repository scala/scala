//############################################################################
// Compile Time Bugs & Test Cases
//############################################################################

import java.lang.System; // to avoid name clash with .NET's library

//############################################################################
// Test 0

/*
class Test0Foo[X];

object Test0Test {
  type Gen[A] = Test0Foo[A];
  class Tic(g: Test0Test.Gen[Int]);
  class Tac(g:           Gen[Int]);
}

//############################################################################
// Test 1 - Single types in lambda lift

object Test1 {
  def main(args: Array[String]): Unit = {
    List[args.type](args);
  }
  def foo[X]: Any = {
    def bar(x: X) = List(x);
    0
  }
}

//############################################################################
// Test 2 - Local variables owned by other local variables

class Test2_1(i: Int) {
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
  val z2_1 = i;
}

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

class Test2_3() {

  def this(i: Int) = {
    this();
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
    val z2_3 = i;
  }

  def test(i: Int): Int = {
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
    val z_test = i;
    0
  }

}
*/
//############################################################################
// Test 3 - Super Calls with Mixins

class Test3Foo;

trait Test3A[T] {
  def fun: T = fun;
}

class Test3B extends Test3A[Test3Foo];

trait Test3M extends Test3A[Test3Foo] {
  override def fun: Test3Foo = super.fun;
}

class Test3C extends Test3B with Test3M;

//############################################################################
