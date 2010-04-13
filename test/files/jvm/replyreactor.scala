import scala.actors.ReplyReactor

object Test {
  def main(args: Array[String]) {
    val a = new ReplyReactor {
      def act() {
        try {
        react {
          case 'hello =>
            sender ! 'hello
        }
        } catch {
          case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
            e.printStackTrace()
        }
      }
    }
    a.start()

    val b = new ReplyReactor {
      def act() {
        try {
        react {
          case r: ReplyReactor =>
            r ! 'hello
            react {
              case any =>
                println(any)
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
