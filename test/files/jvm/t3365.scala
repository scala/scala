import scala.actors.{ReplyReactor, Channel, Actor, Future}

case class ChannelMsg(chan: Channel[Any])

class MyActor extends Actor {
  def act() {
    try {
    val chan = new Channel[Any](this)
    loop {
      react {
        case other: ReplyReactor =>
          other ! ChannelMsg(chan)
          loop {
            chan.react {
              case 'hello =>
                reply('hello)
              case 'stop =>
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

object Test {
  def main(args: Array[String]) {
    val a = new MyActor
    a.start()

    val b = new Actor {
      def act() {
        try {
        react {
          case ChannelMsg(c) =>
            var i = 0
            loop {
              i += 1
              val ft: Future[Any] = c !! 'hello
              ft.inputChannel.react {
                case msg =>
                  if (i % 10000 == 0)
                    println(msg)
                  if (i >= 50000) {
                    c ! 'stop
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

    a ! b
  }
}
