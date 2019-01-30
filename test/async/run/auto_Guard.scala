import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { assert(test == "okay")
  @async
  def test: Any = {
    @autoawait def id[A](a: A) = a

    "" match {
      case _ if id(false) => ???;
      case _              => "okay"
    }
  }
}
