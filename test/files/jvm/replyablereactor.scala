import scala.actors.ReplyReactor

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

object Test {
  def main(args: Array[String]) {
    val a = new MyActor
    a.start()

    val b = new ReplyReactor {
      def act() {
        try {
        react {
          case r: MyActor =>
            var i = 0
            loop {
              i += 1
              val ft = r !! 'hello
              ft.inputChannel.react {
                case msg =>
                  if (i % 10000 == 0)
                    println(msg)
                  if (i >= 50000) {
                    r ! 'stop
                    exit()
                  }
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
