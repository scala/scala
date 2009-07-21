import scala.actors.{Reactor, ReplyReactor}

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

    val b = new Reactor {
      def act() {
        react {
          case r: Reactor =>
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
