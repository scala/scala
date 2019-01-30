import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { assert(test)
  @async
  def test: Boolean = {
    object Extractor {
      @autoawait def unapply(a: String) = Some((a, a))
    }
    "" match {
      case _ if "".isEmpty =>
        "" match {
          case Extractor(a, b) => a == b
        }
    }
  }
}
