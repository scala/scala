
import scala.tools.partest.SessionTest

object Test extends SessionTest {

  override def stripMargins = false

  def session =
"""
scala> object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
<console>:11: error: double definition:
def f[A](a: => A): Int at line 11 and
def f[A](a: => Either[Exception,A]): Int at line 11
have same type after erasure: (a: Function0)Int
       object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
                                              ^

scala> object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
<console>:11: error: double definition:
def f[A](a: => A): Int at line 11 and
def f[A](a: => Either[Exception,A]): Int at line 11
have same type after erasure: (a: Function0)Int
       object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
                                              ^

scala> object Y {
     |   def f[A](a: =>  A) = 1
     |   def f[A](a: => Either[Exception, A]) = 2
     | }
<console>:13: error: double definition:
def f[A](a: => A): Int at line 12 and
def f[A](a: => Either[Exception,A]): Int at line 13
have same type after erasure: (a: Function0)Int
         def f[A](a: => Either[Exception, A]) = 2
             ^

scala> :pa
// Entering paste mode (ctrl-D to finish)

object Y {
  def f[A](a: =>  A) = 1
  def f[A](a: => Either[Exception, A]) = 2
}

// Exiting paste mode, now interpreting.

<pastie>:13: error: double definition:
def f[A](a: => A): Int at line 12 and
def f[A](a: => Either[Exception,A]): Int at line 13
have same type after erasure: (a: Function0)Int
         def f[A](a: => Either[Exception, A]) = 2
             ^

scala> :quit"""
}

