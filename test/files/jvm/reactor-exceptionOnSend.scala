import scala.actors.Reactor
import scala.actors.Actor._

case class MyException(text: String) extends Exception(text)

object A extends Reactor[Any] {
  override def exceptionHandler = {
    case MyException(text) =>
      println("receiver handles exception")
  }

  def guard(): Boolean =
    if (state == 0) {
      state = 1
      throw MyException("illegal state")
    } else
      true

  var state = 0

  def act() {
    try {
    loop {
      react {
        case 'hello if guard() =>
          println("process")
          exit()
      }
    }
    } catch {
      case e: Throwable if (!e.isInstanceOf[scala.util.control.ControlThrowable] &&
                            !e.isInstanceOf[MyException]) =>
        e.printStackTrace()
    }
  }
}

object B extends Reactor[Any] {
  def act() {
    try {
    A.start()
    A ! 'hello
    A ! 'hello
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}

object Test {
  def main(args: Array[String]) {
    B.start()
  }
}
