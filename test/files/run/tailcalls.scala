//############################################################################
// Tail Calls
//############################################################################
// $Id$

//############################################################################
// Calibration

class Calibrator {
  def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
}

//############################################################################
// Tail calls in different contexts

class Class {
  def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
}

class SubClass extends Class {
  override def f(n: Int, v: Int): Int = v;
}

sealed class Sealed {
  def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
}

class SubSealed extends Sealed {
  override def f(n: Int, v: Int): Int = v;
}

final class Final {
  def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
}

object Object {
  def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
}

//############################################################################
// Tail calls in nested objects/classes

object O {
  final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
  object O {
    final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
    object O {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    class C {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    val c: C = new C;
  }
  class C {
    final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
    object O {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    class C {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    val c: C = new C;
  }
  val c: C = new C;
}

class C {
  final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
  object O {
    final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
    object O {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    class C {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    val c: C = new C;
  }
  class C {
    final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
    object O {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    class C {
      final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      object O {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      class C {
        final def f(n: Int, v: Int): Int = if (n == 0) v else f(n - 1, v - 1);
      }
      val c: C = new C;
    }
    val c: C = new C;
  }
  val c: C = new C;
}

//############################################################################
// Tail calls with different signatures

class TailCall[S](s: S) {
  def getS: S = s;

  final def f1(n: Int, v: Int): Int =
    if (n == 0) v else f1(n - 1, v - 1);
  final def f2[T](n: Int, v: Int): Int =
    if (n == 0) v else f2[T](n - 1, v - 1);
  final def f3[T](n: Int, v: Int, ls: List[T]): Int =
    if (n == 0) v else f3(n - 1, v - 1, ls);

  final def g1(x: Int, y: Int): Int = {
    def aux(n: Int, v: Int): Int =
      if (n == 0) v else aux(n - 1, v - 1);
    aux(x, y);
  }
  final def g2[T](x: Int, y: Int): Int = {
    def aux[U](n: Int, v: Int): Int =
      if (n == 0) v else aux[U](n - 1, v - 1);
    aux[T](x, y);
  }
  final def g3[T](x: Int, y: Int, zs: List[T]): Int = {
    def aux[U](n: Int, v: Int, ls: List[Pair[T,U]]): Int =
      if (n == 0) v else aux(n - 1, v - 1, ls);
    aux(x, y, Nil);
  }

  def h1(n: Int, v: Int): Int = hP(n, v);
  private def hP(n: Int, v: Int): Int = if (n == 0) v else hP(n - 1, v - 1);

  // !!! test return in non-tail-call position
  // !!! test non-same-instance calls
  // !!! test non-same-type calls

}

//############################################################################
// Test code

object Test {
  import java.lang.System;

  def check_success(name: String, def closure: Int, expected: Int): Unit = {
    System.out.print("test " + name);
    try {
      val actual: Int = closure;
      if (actual == expected) {
        System.out.print(" was successful");
      } else {
        System.out.print(" failed: expected "+ expected +", found "+ actual);
      }
    } catch {
      case exception: Throwable => {
        System.out.print(" raised exception " + exception);
      }
    }
    System.out.println();
  }

  def calibrate: Int = {
    val calibrator = new Calibrator();
    var stop = false;
    var n = 1;
    while (!stop) {
      try {
        calibrator.f(n, n);
        if (n >= Integer.MAX_VALUE / 2) throw new Error("calibration failure");
        n = 2 * n;
      } catch {
        case exception: StackOverflowError => stop = true
      }
    }
    4 * n;
  }

  def main(args: Array[String]): Unit = {
    // compute min and max iteration number
    val min = 16;
    val max = calibrate;

    // test tail calls in different contexts
    val Final     = new Final();
    val Class     = new Class();
    val SubClass  = new SubClass();
    val Sealed    = new Sealed();
    val SubSealed = new SubSealed();
    check_success("Object   .f", Object   .f(max, max), 0);
    check_success("Final    .f", Final    .f(max, max), 0);
    check_success("Class    .f", Class    .f(max, max), 0);
    check_success("SubClass .f", SubClass .f(max, max), max);
    check_success("Sealed   .f", Sealed   .f(max, max), 0);
    check_success("SubSealed.f", SubSealed.f(max, max), max);
    System.out.println();

    // test tail calls in nested classes/objects
    val c: C = new C;
    check_success("O      .f", O      .f(max, max), 0);
    check_success("c      .f", c      .f(max, max), 0);
    check_success("O.O    .f", O.O    .f(max, max), 0);
    check_success("O.c    .f", O.c    .f(max, max), 0);
    check_success("c.O    .f", c.O    .f(max, max), 0);
    check_success("c.c    .f", c.c    .f(max, max), 0);
    check_success("O.O.O  .f", O.O.O  .f(max, max), 0);
    check_success("O.O.c  .f", O.O.c  .f(max, max), 0);
    check_success("O.c.O  .f", O.c.O  .f(max, max), 0);
    check_success("O.c.c  .f", O.c.c  .f(max, max), 0);
    check_success("c.O.O  .f", c.O.O  .f(max, max), 0);
    check_success("c.O.c  .f", c.O.c  .f(max, max), 0);
    check_success("c.c.O  .f", c.c.O  .f(max, max), 0);
    check_success("c.c.c  .f", c.c.c  .f(max, max), 0);
    check_success("O.O.O.O.f", O.O.O.O.f(max, max), 0);
    check_success("O.O.O.c.f", O.O.O.c.f(max, max), 0);
    check_success("O.O.c.O.f", O.O.c.O.f(max, max), 0);
    check_success("O.O.c.c.f", O.O.c.c.f(max, max), 0);
    check_success("O.c.O.O.f", O.c.O.O.f(max, max), 0);
    check_success("O.c.O.c.f", O.c.O.c.f(max, max), 0);
    check_success("O.c.c.O.f", O.c.c.O.f(max, max), 0);
    check_success("O.c.c.c.f", O.c.c.c.f(max, max), 0);
    check_success("c.O.O.O.f", c.O.O.O.f(max, max), 0);
    check_success("c.O.O.c.f", c.O.O.c.f(max, max), 0);
    check_success("c.O.c.O.f", c.O.c.O.f(max, max), 0);
    check_success("c.O.c.c.f", c.O.c.c.f(max, max), 0);
    check_success("c.c.O.O.f", c.c.O.O.f(max, max), 0);
    check_success("c.c.O.c.f", c.c.O.c.f(max, max), 0);
    check_success("c.c.c.O.f", c.c.c.O.f(max, max), 0);
    check_success("c.c.c.c.f", c.c.c.c.f(max, max), 0);
    System.out.println();

    // test tail calls with different signatures
    val TailCall = new TailCall("S");
    check_success("TailCall.f1", TailCall.f1(max, max     ), 0);
    check_success("TailCall.f2", TailCall.f2(max, max     ), 0);
    check_success("TailCall.f3", TailCall.f3(max, max, Nil), 0);
    check_success("TailCall.g1", TailCall.g1(max, max     ), 0);
    check_success("TailCall.g2", TailCall.g2(max, max     ), 0);
    check_success("TailCall.g3", TailCall.g3(max, max, Nil), 0);
    check_success("TailCall.h1", TailCall.h1(max, max     ), 0);
    System.out.println();
  }
}

//############################################################################
