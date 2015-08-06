
import scala.tools.partest.SessionTest

object Test extends SessionTest {

  def session =
"""
scala> val x: Any = 42
x: Any = 42

scala> x + " works"
res0: String = 42 works

scala> import Predef.{ any2stringadd => _, _ }
import Predef.{any2stringadd=>_, _}

scala> x + " works"
<console>:14: error: value + is not a member of Any
       x + " works"
         ^

scala> import Predef._
import Predef._

scala> x + " works"
res2: String = 42 works

scala> object Predef { def f = 42 }
defined object Predef

scala> import Predef._
import Predef._

scala> f
<console>:14: error: not found: value f
       f
       ^

scala> Predef.f
res4: Int = 42

scala> :quit"""
}
