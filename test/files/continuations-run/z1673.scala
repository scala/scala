import scala.util.continuations._

class MatchRepro {
  def s: String @cps[Any] = shift { k => k("foo") }

  def p = {
    val k = s
    s match { case lit0 => }
  }

  def q = {
    val k = s
    k match { case lit1 => }
  }

  def r = {
    s match { case "FOO" => }
  }

  def t = {
    val k = s
    k match { case "FOO" => }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val m = new MatchRepro
    ()
  }
}
