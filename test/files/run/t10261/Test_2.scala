import scala.util.Try

object C extends Companion[C] {
  def parse(v: String) = if (v.nonEmpty) Some(new C(v)) else None
}

case class C(value: String)

object Test {
  def main(args: Array[String]): Unit = {
    assert(Try{C("")}.isFailure, "Empty value should fail to parse") // check that parse is used to validate input
    assert(C("a").value == "a", "Unexpected value")
  }
}
