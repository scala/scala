import scala.actors.ReplyReactor

object Test {
  def main(args: Array[String]) {
    val a = new ReplyReactor {
      def act() {
        react {
          case 'hello =>
            sender ! 'hello
        }
      }
    }
    a.start()

    val b = new ReplyReactor {
      def act() {
        react {
          case r: ReplyReactor =>
            r ! 'hello
            react {
              case any =>
                println(any)
            }
        }
      }
    }
    b.start()

    b ! a
  }
}
