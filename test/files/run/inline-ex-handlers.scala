import scala.tools.partest.IcodeTest

object Test extends IcodeTest {
  override def printIcodeAfterPhase = "inlineExceptionHandlers"
}

import scala.util.Random._

/** There should be no inlining taking place in this class */
object TestInlineHandlersNoInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersNoInline")
    var result = -1

    try {
      if (nextInt % 2 == 0)
      throw new IllegalArgumentException("something")
      result = 1
    } catch {
      case e: StackOverflowError =>
      println("Stack overflow")
    }

    result
  }
}

/** Just a simple inlining should take place in this class */
object TestInlineHandlersSimpleInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersSimpleInline")
    var result = -1

    try {
      if (nextInt % 2 == 0)
      throw new IllegalArgumentException("something")
      result = 1
    } catch {
      case e: IllegalArgumentException =>
      println("IllegalArgumentException")
    }

    result
  }
}

/** Inlining should take place because the handler is taking a superclass of the exception thrown */
object TestInlineHandlersSubclassInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersSubclassInline")
    var result = -1

    try {
      if (nextInt % 2 == 0)
      throw new IllegalArgumentException("something")
      result = 1
    } catch {
      case e: RuntimeException =>
      println("RuntimeException")
    }

    result
  }
}

/** For this class, the finally handler should be inlined */
object TestInlineHandlersFinallyInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersFinallyInline")
    var result = -1

    try {
      if (nextInt % 2 == 0)
      throw new IllegalArgumentException("something")
      result = 1
    } catch {
      case e: Exception => throw e
    } finally {
      println("finally")
      result = (result - 1) / 2
    }

    result
  }
}


case class MyException(message: String) extends RuntimeException(message)

/** For this class, we test inlining for a case class error */
object TestInlineHandlersCaseClassExceptionInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersCaseClassExceptionInline")
    var result = -1

    try {
      if (nextInt % 2 == 0)
      throw new MyException("something")
      result = 1
    } catch {
      case MyException(message) => println(message)
    }

    result
  }
}


/** For this class, inline should take place in the inner handler */
object TestInlineHandlersNestedHandlerInnerInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersNestedHandlersInnerInline")
    var result = -1

    try {
      try {
        if (nextInt % 2 == 0)
        throw new MyException("something")
        result = 1
      } catch {
        case MyException(message) => println(message)
      }
    } catch {
      case e: IllegalArgumentException => println("IllegalArgumentException")
    }

    result
  }
}


/** For this class, inline should take place in the outer handler */
object TestInlineHandlersNestedHandlerOuterInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersNestedHandlersOuterInline")
    var result = -1

    try {
      try {
        if (nextInt % 2 == 0)
        throw new MyException("something")
        result = 1
      } catch {
        case e: IllegalArgumentException => println("IllegalArgumentException")
      }
    } catch {
      case MyException(message) => println(message)
    }

    result
  }
}


/** For this class, inline should take place in the all handlers (inner, outer and finally) */
object TestInlineHandlersNestedHandlerAllInline {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersNestedHandlersOuterInline")
    var result = -1

    try {
      try {
        if (nextInt % 2 == 0)
        throw new MyException("something")
        result = 1
      } catch {
        case MyException(message) =>
        println(message)
        throw MyException(message)
      }
    } catch {
      case MyException(message) =>
      println(message)
      throw MyException(message)
    } finally {
      println("finally")
      result = (result - 1) / 2
    }

    result
  }
}


/** This class is meant to test whether the inline handler is copied only once for multiple inlines */
object TestInlineHandlersSingleCopy {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersSingleCopy")
    var result = -1

    try {

      if (nextInt % 2 == 0)
      throw new MyException("something")

      println("A side effect in the middle")
      result = 3 // another one

      if (nextInt % 3 == 2)
      throw new MyException("something else")
      result = 1
    } catch {
      case MyException(message) =>
      println(message)
    }

    result
  }
}

/** This should test the special exception handler for synchronized blocks */
object TestInlineHandlersSynchronized {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersSynchronized")
    var result = "hello"

    // any exception thrown here will be caught by a default handler that does MONTIOR_EXIT on result :)
    result.synchronized {
      throw MyException(result)
    }

    result.length
  }
}

/** This should test the special exception handler for synchronized blocks with stack */
object TestInlineHandlersSynchronizedWithStack {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersSynchronizedWithStack")
    var result = "hello"

    // any exception thrown here will be caught by a default handler that does MONTIOR_EXIT on result :)
    result = "abc" + result.synchronized {
      throw MyException(result)
    }

    result.length
  }
}

/** This test should trigger a bug in the dead code elimination phase - it actually crashes ICodeCheckers
object TestInlineHandlersSynchronizedWithStackDoubleThrow {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersSynchronizedWithStackDoubleThrow")
    var result = "a"

    // any exception thrown here will be caught by a default handler that does MONTIOR_EXIT on result :)
    result += result.synchronized { throw MyException(result) }
    result += result.synchronized { throw MyException(result) }

    result.length
  }
}
*/

/** This test should check the preciseness of the inliner: it should not do any inlining here
* as it is not able to discern between the different exceptions
*/
object TestInlineHandlersPreciseness {

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersCorrectHandler")

    try {
      val exception: Throwable =
      if (scala.util.Random.nextInt % 2 == 0)
      new IllegalArgumentException("even")
      else
      new StackOverflowError("odd")
      throw exception
    } catch {
      case e: IllegalArgumentException =>
      println("Correct, IllegalArgumentException")
      case e: StackOverflowError =>
      println("Correct, StackOverflowException")
      case t: Throwable =>
      println("WROOOONG, not Throwable!")
    }
  }
}

/** This check should verify that the double no-local exception handler is duplicated correctly */
object TestInlineHandlersDoubleNoLocal {

  val a1: String = "a"
  val a2: String = "b"

  def main(args: Array[String]): Unit = {
    println("TestInlineHandlersDoubleNoLocal")

    try {
      a1.synchronized {
        a2. synchronized {
          throw new MyException("crash")
        }
      }
    } catch {
      case t: Throwable => println("Caught crash: " + t.toString)
    }

    /*        try {
      val exception: Throwable =
      if (scala.util.Random.nextInt % 2 == 0)
      new IllegalArgumentException("even")
      else
      new StackOverflowError("odd")
      throw exception
    } catch {
      case e: IllegalArgumentException =>
      println("Correct, IllegalArgumentException")
      case e: StackOverflowError =>
      println("Correct, StackOverflowException")
      case t: Throwable =>
      println("WROOOONG, not Throwable!")
    }*/
  }
}
