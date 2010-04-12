import scala.actors.{Actor, TIMEOUT}

object A extends Actor {
  def act() {
    receive {
      case 'done =>
        var cnt = 0
        while (cnt < 500) {
          cnt += 1
          receiveWithin (0) {
            case 'msg =>
              if (cnt % 100 == 0)
                println("'msg")
            case TIMEOUT =>
              // should not happen
              println("FAIL1")
          }
        }
        cnt = 0
        while (cnt < 500) {
          cnt += 1
          receiveWithin (0) {
            case 'msg =>
              // should not happen
              println("FAIL2")
            case TIMEOUT =>
              if (cnt % 100 == 0)
                println("TIMEOUT")
          }
        }
        B ! 'next
        receive { case 'done => }
        cnt = 0
        while (cnt < 501) {
          cnt += 1
          receiveWithin (500) {
            case 'msg2 =>
              if (cnt % 100 == 0)
                println("'msg2")
            case TIMEOUT =>
              println("TIMEOUT")
          }
        }
    }
  }
}

object B extends Actor {
  def act() {
    A.start()
    for (_ <- 1 to 500) {
      A ! 'msg
    }
    A ! 'done
    receive {
      case 'next =>
        for (_ <- 1 to 500) {
          A ! 'msg2
        }
        A ! 'done
    }
  }
}

object Test {
  def main(args:Array[String]) {
    B.start()
  }
}
