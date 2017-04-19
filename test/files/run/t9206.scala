
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  def session =
    s"""|
        |scala> val i: Int = "foo"
        |<console>:10: error: type mismatch;
        | found   : String("foo")
        | required: Int
        |       val i: Int = "foo"
        |                    ^
        |
        |scala> { val j = 42 ; val i: Int = "foo" + j }
        |<console>:11: error: type mismatch;
        | found   : String
        | required: Int
        |       { val j = 42 ; val i: Int = "foo" + j }
        |                                         ^
        |
        |scala> :quit"""
}
