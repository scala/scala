
import scala.actors.Actor._
import scala.actors.TIMEOUT

object Test extends Application {
  case class Timing(time: Long)

  actor {
    val a = actor {
      react {
        case 'doTiming =>
          val s = sender
          reactWithin(500) {
            case TIMEOUT =>
              s ! Timing(System.currentTimeMillis)
          }
      }
    }

    val start = System.currentTimeMillis
    (a !? 'doTiming) match {
      case Timing(end) =>
        val delay = end - start

        if (delay > 100 && delay < 900)
          println("OK")
        else {
          println("EXPECTED: 100 < x < 900")
          println("ACTUAL:   "+delay)
        }
    }
  }
}
