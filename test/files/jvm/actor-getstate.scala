import scala.actors.{Reactor, Actor, TIMEOUT}

object Test {

  def assert(cond: => Boolean, hint: String) {
    if (!cond)
      println("FAIL ["+hint+"]")
  }

  def main(args: Array[String]) {
    val a = new Reactor[Any] {
      def act() {
        assert(getState == Actor.State.Runnable, "runnable1")
        react {
          case 'go =>
            println("OK")
        }
      }
    }
    assert(a.getState == Actor.State.New, "new1")

    a.start()
    Thread.sleep(200)
    assert(a.getState == Actor.State.Suspended, "suspend1")

    a ! 'go
    Thread.sleep(200)
    assert(a.getState == Actor.State.Terminated, "terminated1")

    val b = new Actor {
      def act() {
        assert(getState == Actor.State.Runnable, "runnable2")
        react {
          case 'go =>
            reactWithin(100000) {
              case TIMEOUT =>
              case 'go =>
                receive {
                  case 'go =>
                }
                receiveWithin(100000) {
                  case TIMEOUT =>
                  case 'go =>
                    println("OK")
                }
            }
        }
      }
    }
    assert(b.getState == Actor.State.New, "new2")

    b.start()
    Thread.sleep(200)
    assert(b.getState == Actor.State.Suspended, "suspend2")

    b ! 'go
    Thread.sleep(200)
    assert(b.getState == Actor.State.TimedSuspended, "timedsuspend2")

    b ! 'go
    Thread.sleep(200)
    assert(b.getState == Actor.State.Blocked, "blocked2")

    b ! 'go
    Thread.sleep(200)
    assert(b.getState == Actor.State.TimedBlocked, "timedblocked2")

    b ! 'go
    Thread.sleep(200)
    assert(b.getState == Actor.State.Terminated, "terminated2")
  }

}
