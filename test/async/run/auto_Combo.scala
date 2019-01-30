import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { assert(test == "case 3: blerg3")
  @async
  def test: Any = {
    object Extractor1 {
      @autoawait def unapply(a: String) = Some((a + 1, a + 2))
    }
    object Extractor2 {
      @autoawait def unapply(a: String) = Some(a + 3)
    }
    @autoawait def id(a: String) = a

    println("Test.test")
    val r1 = Predef.identity("blerg") match {
      case x if " ".isEmpty                                   => "case 2: " + x
      case Extractor1(Extractor2(x), y: String) if x == "xxx" => "case 1: " + x + ":" + y
        x match {
          case Extractor1(Extractor2(x), y: String) =>
          case _                                    =>
        }
      case Extractor2(x)                                      => "case 3: " + x
    }
    r1
  }
}
