

@deprecated("Suppress warnings", since="2.11")
object Test {
import scala.actors._
import scala.actors.Actor._

class MyActor extends ReplyReactor {
  def act() {
    try {
    loop {
      react {
        case 'hello =>
          sender ! 'hello
        case 'stop =>
          exit()
      }
    }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}

  def main(args: Array[String]) {
    val a = new MyActor
    a.start()

    val b = new Reactor[Any] {
      def act() {
        try {
        react {
          case r: MyActor =>
            var i = 0
            loop {
              i += 1
              val msg = r !? (500, 'hello)
              if (i % 200000 == 0)
                println(msg)
              if (i >= 1000000) {
                r ! 'stop
                exit()
              }
            }
        }
        } catch {
          case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
            e.printStackTrace()
        }
      }
    }
    b.start()

    b ! a
  }
}
