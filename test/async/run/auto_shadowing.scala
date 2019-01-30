import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { test

  trait Foo

  trait Bar extends Foo

  @autoawait def boundary = ""
  @async
  def test: Unit = {
    (new Bar {}: Any) match {
      case foo: Bar =>
        boundary
        0 match {
          case _ => foo; ()
        }
        ()
    }
    ()
  }
}
