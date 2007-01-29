/*
 * Try exception handling and finally blocks.
 *
 * $Id$
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
    case _ => error("..");
  }
}

object Test {
  def nested1: Unit = try {
    try {
      error("nnnnoooo");
    } finally {
      Console.println("Innermost finally");
    }
  } finally {
    Console.println("Outermost finally");
  }

  def nested2 =  try {
    try {
      error("nnnnoooo");
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
      a.x;
    } catch {
      case Leaf(a) => Console.println(a);
      case _: NullPointerException => Console.println("Exception occurred");
    }
  }

  def method3: Unit = try {
    try {
      val a: Leaf = null;
      a.x;
    } catch {
      case Leaf(a) => Console.println(a);
    }
  } catch {
    case npe: NullPointerException =>
      Console.println("Cought an NPE");
  }

  def withValue1: Unit = {
    val x = try {
      10
    } finally {
      Console.println("Oh, oh");
    };
    Console.println(x);
  }

  def tryFinallyTry: Unit = {
    try {
      ()
    } finally {
      try {
        error("a");
      } catch {
        case _ => Console.println("Silently ignore exception in finally");
      }
    }
  }

  def valInFinally: Unit =
    try {
    } finally {
      val fin = "Abc";
      Console.println(fin);
    };

  def tryAndValInFinally: Unit =
    try {
    } finally {
      val fin = "Abc";
      try {
        Console.println(fin);
      } catch { case _ => () }
    };

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




  def execute(f: => Unit) = try {
    f;
  } catch {
    case _ => ();
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
  }
}
