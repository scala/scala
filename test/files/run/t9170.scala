
import scala.tools.partest.SessionTest

object Test extends SessionTest {

  override def stripMargins = false

  def session =
"""Type in expressions to have them evaluated.
Type :help for more information.

scala> object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
<console>:7: error: double definition:
def f[A](a: => A): Int at line 7 and
def f[A](a: => Either[Exception,A]): Int at line 7
have same type after erasure: (a: Function0)Int
       object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
                                              ^

scala> object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
<console>:7: error: double definition:
def f[A](a: => A): Int at line 7 and
def f[A](a: => Either[Exception,A]): Int at line 7
have same type after erasure: (a: Function0)Int
       object Y { def f[A](a: => A) = 1 ; def f[A](a: => Either[Exception, A]) = 2 }
                                              ^

scala> object Y {
     |   def f[A](a: =>  A) = 1
     |   def f[A](a: => Either[Exception, A]) = 2
     | }
<console>:9: error: double definition:
def f[A](a: => A): Int at line 8 and
def f[A](a: => Either[Exception,A]): Int at line 9
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

<console>:9: error: double definition:
def f[A](a: => A): Int at line 8 and
def f[A](a: => Either[Exception,A]): Int at line 9
have same type after erasure: (a: Function0)Int
         def f[A](a: => Either[Exception, A]) = 2
             ^

scala> :quit"""
}

