import scala.actors.{Actor, TIMEOUT, Exit}
import scala.actors.Actor._

object Test {

  val NUM = 2000

  def main(args: Array[String]) {
    var b: Actor = null
    var c: Actor = null

    val a = actor {
      for (_ <- 0 until NUM)
        receive {
          case 'hello if sender == b => // do nothing
        }
      b ! 'ok
      for (_ <- 0 until NUM)
        receiveWithin (1000) {
          case 'bye if sender == b => // do nothing
          case TIMEOUT => b ! 'fail
        }
      b ! 'ok
    }

    b = actor {
      self.trapExit = true
      link(a)

      for (_ <- 0 until NUM)
        a ! 'hello

      val proceed = receive {
        case Exit(from, reason) => println("FAIL"); false
        case 'ok                => println("OK"); true
        case other              => println(other); false
      }

      if (proceed) {
        for (_ <- 0 until NUM)
          a ! 'bye
        receive {
          case Exit(from, reason) => println("FAIL")
          case 'ok                => println("OK")
          case other              => println(other)
        }
      }
    }
  }

}
