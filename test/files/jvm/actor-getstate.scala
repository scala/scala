import scala.actors.{Reactor, Actor, TIMEOUT}

object Test {

  def assert(cond: => Boolean) {
    if (!cond)
      println("FAIL")
  }

  def main(args: Array[String]) {
    val a = new Reactor[Any] {
      def act() {
        assert(getState == Actor.State.Runnable)
        react {
          case 'go =>
            println("OK")
        }
      }
    }
    assert(a.getState == Actor.State.New)

    a.start()
    Thread.sleep(100)
    assert(a.getState == Actor.State.Suspended)

    a ! 'go
    Thread.sleep(100)
    assert(a.getState == Actor.State.Terminated)

    val b = new Actor {
      def act() {
        assert(getState == Actor.State.Runnable)
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
    assert(b.getState == Actor.State.New)

    b.start()
    Thread.sleep(100)
    assert(b.getState == Actor.State.Suspended)

    b ! 'go
    Thread.sleep(100)
    assert(b.getState == Actor.State.TimedSuspended)

    b ! 'go
    Thread.sleep(100)
    assert(b.getState == Actor.State.Blocked)

    b ! 'go
    Thread.sleep(100)
    assert(b.getState == Actor.State.TimedBlocked)

    b ! 'go
    Thread.sleep(100)
    assert(b.getState == Actor.State.Terminated)
  }

}
