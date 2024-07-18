
//> using options -Werror -Xsource:3 -Xlint:universal-methods

// Dotty has top-level defs, so the reference is linted based on context.
// For Scala 2, check result of looking up the identifier.
// Universal members are not imported from root contexts (in particular, Predef).
// Use an explicit import to exercise the warning.

class Test(val x: Any) extends AnyVal {
  import Predef.*

  def test1 =
    synchronized { // error
      println("hello")
    }

  /* correctly errors in Scala 2
  def test2 =
    this.synchronized { // not an error (should be?)
      println("hello")
    }
  */

  // surprise, ~not~ a universal member
  def test16 =
    wait() // error

  // OK because Any, so this is kosher
  def `maybe hashcode` = hashCode

  // it does know about notify
  def `maybe notify`(): Unit = notify()
  def `maybe notifyAll`(): Unit = notifyAll()

}

// Can't work these tests inside value class.
//
class ObjectHolder {

  object MyLib

  /* ambiguous
  def test3 = {
    import MyLib.*
    synchronized { // error
      println("hello")
    }
  }
  */

  def test4 =
    1.synchronized { // warn
      println("hello")
    }

  object Test4 {
    synchronized { // not an error
      println("hello")
    }
  }

  object Test5 {
    def test5 =
      synchronized { // not an error
        println("hello")
      }
  }

  object Test6 {
    import MyLib.*
    synchronized { // not an error
      println("hello")
    }
  }

  object Test7 {
    import MyLib.*
    def test7 =
      synchronized { // not an error
        println("hello")
      }
  }

  /*
  object Test7b {
    def test8 =
      import MyLib.*
      synchronized { // already an error: Reference to synchronized is ambiguous.
        println("hello")
      }
  }
  */

  class Test8 {
    synchronized { // not an error
      println("hello")
    }
  }

  class Test9 {
    def test5 =
      synchronized { // not an error
        println("hello")
      }
  }

  class Test10 {
    import MyLib.*
    synchronized { // not an error
      println("hello")
    }
  }

  class Test11 {
    import MyLib.*
    def test7 =
      synchronized { // not an error
        println("hello")
      }
  }

  trait Test12 {
    synchronized { // not an error
      println("hello")
    }
  }

  trait Test13 {
    def test5 =
      synchronized { // not an error
        println("hello")
      }
  }

  trait Test14 {
    import MyLib.*
    synchronized { // not an error
      println("hello")
    }
  }

  trait Test15 {
    import MyLib.*
    def test7 =
      synchronized { // not an error
        println("hello")
      }
  }

  def test16 =
    wait() // error

  def test17 =
    this.wait() // not an error (should be?)

  /* ambiguous
  def test18 = {
    import MyLib.*
    wait() // error
  }
  */

  def test19 =
    1.wait() // not an error (should be?)

  /* ambiguous
  def test20 =
    wait(10) // error
  */

  def test21 =
    this.wait(10) // not an error (should be?)

  /* ambiguous
  def test22 = {
    import MyLib.*
    wait(10) // error
  }
  */

  def test23 =
    1.wait(10) // not an error (should be?)

  def test24 =
    hashCode() // error

  def test25 =
    this.hashCode() // not an error (should be?)

  /* ambiguous
  def test26 = {
    import MyLib.*
    hashCode() // error
  }
  */

  def test27 =
    1.hashCode()// not an error (should be? probably not)
}
