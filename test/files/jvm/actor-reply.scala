import scala.actors.Actor

object Test {
  def main(args: Array[String]) {
    val a = new Actor {
      def act() {
        react {
          case 'hello =>
            sender ! 'hello
        }
      }
    }
    a.start()

    val b = new Actor {
      def act() {
        react {
          case r: Actor =>
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
