
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  //override def prompt = "XXX> "
//Welcome to Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_40).
  def session =
    s"""|
        |scala> val i: Int = "foo"
        |<console>:11: error: type mismatch;
        | found   : String("foo")
        | required: Int
        |       val i: Int = "foo"
        |                    ^
        |
        |scala> { val j = 42 ; val i: Int = "foo" + j }
        |<console>:12: error: type mismatch;
        | found   : String
        | required: Int
        |       { val j = 42 ; val i: Int = "foo" + j }
        |                                         ^
        |
        |scala> :quit"""
        /*
        |XXX> XXX> def f = 42
        |
        |// Detected repl transcript paste: ctrl-D to finish.
        |
        |// Replaying 1 commands from transcript.
        |
        |XXX> def f = 42
        |f: Int
        |
        |XXX> :quit"""
        */

}
