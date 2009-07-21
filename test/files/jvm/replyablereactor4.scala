import scala.actors._
import scala.actors.Actor._

class MyActor extends ReplyReactor with ReplyableReactor {
  def act() {
    loop {
      react {
        case 'hello =>
          sender ! 'hello
        case 'stop =>
          exit()
      }
    }
  }
}

object Test {
  def main(args: Array[String]) {
    val a = new MyActor
    a.start()

    val b = new Reactor {
      def act() {
        react {
          case r: MyActor =>
            var i = 0
            loop {
              i += 1
              val msg = r !? (500, 'hello)
              if (i % 10000 == 0)
                println(msg)
              if (i >= 50000) {
                r ! 'stop
                exit()
              }
            }
        }
      }
    }
    b.start()

    b ! a
  }
}
