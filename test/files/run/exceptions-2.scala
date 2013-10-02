/*
 * Try exception handling and finally blocks.
 */

trait Tree extends Exception;

case class Node(a: Tree, b: Tree) extends Tree;
case class Leaf(x: Int) extends Tree;


object NoExcep {
  def a = true;
  def b = false;
  def c = true;

  def method1(t: Tree) = try {
    Console.println(t);
  } catch {
    case Node(Leaf(_), Leaf(_)) => a;
    case Leaf(_) => b;
  }

  def method2 = try {
    Console.println("Hello, world");
  } catch {
    case _: Error => Console.println("File error");
    case t: Throwable => Console.println("Unknown error");
  }

  def method3 = try {
    try {
      Console.println("method3");
    } catch {
      case Node(Leaf(_), Leaf(_)) => Console.println("First one");
      case Leaf(_) => Console.println("Second one");
    }
  } catch {
    case _: Error => Console.println("File error");
    case t: Exception => Console.println("Unknown error");
  }

  def method4 = try {
    Console.println("..");
  } catch {
    case _: Throwable => sys.error("..");
  }
}

object Test {
  def nested1: Unit = try {
    try {
      sys.error("nnnnoooo");
    } finally {
      Console.println("Innermost finally");
    }
  } finally {
    Console.println("Outermost finally");
  }

  def nested2 =  try {
    try {
      sys.error("nnnnoooo");
    } finally {
      Console.println("Innermost finally");
    }
    Console.println("Intermediary step");
  } finally {
    Console.println("Outermost finally");
  }

  def mixed =
    try {
      if (10 > 0)
        throw Leaf(10);
      Console.println("nooo oneeee can priiiint meee");
    } catch {
      case Leaf(a) => Console.println(a);
      case _: Exception => Console.println("Exception occurred");
    } finally {
      Console.println("Finally!");
    }

  def method2: Unit = {
    try {
      if (10 > 0)
        throw Leaf(10);
      Console.println("nooo oneeee can priiiint meee");
    } catch {
      case Leaf(a) => Console.println(a);
      case _: Exception => Console.println("Exception occurred");
    }

    try {
      val a: Leaf = null;
      println(a.x);
    } catch {
      case Leaf(a) => Console.println(a);
      case _: NullPointerException => Console.println("Exception occurred");
    }
  }

  def method3: Unit = try {
    try {
      val a: Leaf = null;
      println(a.x);
    } catch {
      case Leaf(a) => Console.println(a);
    }
  } catch {
    case npe: NullPointerException =>
      Console.println("Caught an NPE");
  }

  def withValue1: Unit = {
    val x = try {
      10
    } finally {
      Console.println("Oh, oh");
    };
    Console.println(x);
  }

  def withValue2: Unit = {
    val x = try {
      null
    } finally {
      Console.println("droped a null");
    };
    Console.println(x);
  }

  def tryFinallyTry: Unit = {
    try {
      ()
    } finally {
      try {
        sys.error("a");
      } catch {
        case _: Throwable => Console.println("Silently ignore exception in finally");
      }
    }
  }

  def valInFinally: Unit =
    try {
    } finally {
      val fin = "Abc";
      Console.println(fin);
    }

  def tryAndValInFinally: Unit =
    try {
    } finally {
      val fin = "Abc";
      try {
        Console.println(fin);
      } catch { case _: Throwable => () }
    }

  def returnInBody: Unit = try {
    try {
      Console.println("Normal execution...");
      return
      Console.println("non reachable code");
    } finally {
      Console.println("inner finally");
    }
  } finally {
    Console.println("Outer finally");
  }

  def returnInBodySynch: Unit = try {
    synchronized {
      try {
        Console.println("Synchronized normal execution...");
        return
        Console.println("non reachable code");
      } finally {
        Console.println("inner finally");
      }
    }
  } finally {
    Console.println("Outer finally");
  }


  def returnInBodyAndInFinally: Unit = try {
    try {
      Console.println("Normal execution...");
      return
      Console.println("non reachable code");
    } finally {
      Console.println("inner finally");
      return
    }
  } finally {
    Console.println("Outer finally");
    return
  }

  def returnInBodyAndInFinally2: Unit = try {
    try {
      Console.println("Normal execution...");
      return
      Console.println("non reachable code");
    } finally {
      try {
        Console.println("inner finally");
        return
      } finally {
        Console.println("finally inside finally");
      }
    }
  } finally {
    Console.println("Outer finally");
    return
  }

  /** bug #1020, no crash at compile time */
  def tryCatchInFinally: Unit = {
    try {
      Console.println("Try")
    } catch {
      case e:java.io.IOException =>
        throw e
    } finally {
      val x = 10
      // Always make sure result sets and statements are closed,
      // and the connection is returned to the pool
      if (x != 10) {
        try { Console.println("Fin"); } catch { case e:java.io.IOException => ;  }
      }
    }
  }

  def tryThrowFinally: Unit = {
    try {
      print("A")
      throw new Exception
    } catch {
      case e : Exception =>
        print("B")
      throw e
    } finally {
      println("C")
    }
  }

  def execute(f: => Unit) = try {
    f;
  } catch {
    case _: Throwable => ()
  }


  def returnWithFinallyClean: Int = try {
    try {
      Console.println("Normal execution...");
      return 10
      Console.println("non reachable code");
      11
    } finally {
      Console.println("inner finally");
    }
  } finally {
    Console.println("Outer finally");
    try { 1 } catch { case e: java.io.IOException => () }
  }

  /** Test that empty finally clauses containing while are correctly emitted.
   */
  class Issue {
    var b = 0
    try {
      //    println("abc")
    } finally {
      while (b == -1) {b = 0}
    }
  }

  /* Tests that class Issue passes verification. */
  def whileInFinally = {
    new Issue
  }



  def main(args: Array[String]): Unit = {
    Console.println("nested1: ");
    execute(nested1);

    Console.println("nested2: ");
    execute(nested2);

    Console.println("mixed: ");
    execute(mixed);

    Console.println("withValue1:");
    execute(withValue1);

    Console.println("withValue2:");
    execute(withValue2);

    Console.println("method2:");
    execute(method2);

    Console.println("method3:");
    execute(method3);

    Console.println("tryFinallyTry:");
    execute(tryFinallyTry);

    Console.println("valInFinally:");
    execute(valInFinally);
    Console.println("tryAndValInFinally");
    execute(tryAndValInFinally);

    Console.println("=================");

    Console.println("NoExcep.method2:");
    execute(NoExcep.method2);

    Console.println("NoExcep.method3:");
    execute(NoExcep.method3);

    Console.println("NoExcep.method4:");
    execute(NoExcep.method4);

    Console.println("Return inside body:");
    execute(returnInBody);

    Console.println("Return inside synchronized body:");
    execute(returnInBodySynch);

    Console.println("Return inside body and return in finally:");
    execute(returnInBodyAndInFinally);

    Console.println("Return inside body and return in finally inside finally:");
    execute(returnInBodyAndInFinally2);

    Console.println("Throw in catch and finally:");
    execute(tryThrowFinally);

    Console.println("Return with finally clause that cleans the stack")
    returnWithFinallyClean

    whileInFinally
  }
}
