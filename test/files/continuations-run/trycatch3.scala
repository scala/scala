import scala.util.continuations._

trait Result
case class ValueResult(value: Int) extends Result
case class ErrorResult(err: Exception) extends Result

class Foo {
  def func: Int @cpsParam[Any, Any] = shiftUnit[Int, Any, Any](5)
}

object Test extends App {
  val foo = new Foo

  def f: Result @cpsParam[Any, Any] = {
    try {
      //ValueResult(foo.func)
      throw new Exception("" + foo.func)
    }
    catch {
      case ex: Exception => ErrorResult(ex)
    }
  }

  reset {
    f match {
      case ValueResult(x)  => println(x)
      case ErrorResult(ex) => println(ex.getMessage())
    }
  }
}
