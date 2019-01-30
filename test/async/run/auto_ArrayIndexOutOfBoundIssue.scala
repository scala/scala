import scala.tools.nsc.transform.async.user.{async, autoawait}

sealed trait Result

case object A extends Result

case object B extends Result

case object C extends Result

object Test extends App { test
  protected def doStuff(res: Result) = {
    class C {
      @autoawait def needCheck = false

      @async def m = {
        if (needCheck) "NO"
        else {
          res match {
            case A => 1
            case _ => 2
          }
        }
      }
    }
  }


  @async
  def test() = doStuff(B)
}
