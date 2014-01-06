
import scala.language.{ postfixOps }
object Test {
  def shortName(x: AnyRef) = x.getClass.getName split '.' last
  type Handler[+T] = PartialFunction[Throwable, T]

  val standardHandler: Handler[String] = {
    case x: java.util.NoSuchElementException    => shortName(x)
    case x: java.lang.IllegalArgumentException  => shortName(x)
  }

  def fn[T: Handler](body: => T): T = {
    try body
    catch implicitly[Handler[T]]
  }

  def f1 = {
    implicit val myHandler = standardHandler
    println(fn(Nil.head))
    println(fn(null.toString))
  }
  def f2 = {
    implicit val myHandler: Handler[String] = standardHandler orElse {
      case x => "DEBUG: " + shortName(x)
    }
    println(fn(Nil.head))
    println(fn(null.toString))
  }

  def main(args: Array[String]): Unit = {
    try f1
    catch { case x: Throwable => println(shortName(x) + " slipped by.") }

    f2
  }
}
