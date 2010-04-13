import scala.actors.{Reactor, Actor, TIMEOUT}
import Actor._

object Test {

  def assert(cond: => Boolean, hint: String) {
    if (!cond)
      println("FAIL ["+hint+"]")
  }

  def expectActorState(a: Reactor[T] forSome { type T }, s: Actor.State.Value) {
    var done = false
    var i = 0
    while (!done) {
      i = i + 1
      if (i == 10) { // only wait for 2 seconds total
        println("FAIL ["+a+": expected "+s+"]")
        done = true
      }

      Thread.sleep(200)
      if (a.getState == s) // success
        done = true
    }
  }

  def main(args: Array[String]) {
    actor {
      val a = new Reactor[Any] {
        def act() {
          assert(getState == Actor.State.Runnable, "runnable1")
          react {
            case 'go =>
              println("OK")
          }
        }
      }
      expectActorState(a, Actor.State.New)

      a.start()
      expectActorState(a, Actor.State.Suspended)

      a ! 'go
      expectActorState(a, Actor.State.Terminated)

      val b = new Actor {
        def act() {
          assert(getState == Actor.State.Runnable, "runnable2: "+getState)
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
      expectActorState(b, Actor.State.New)

      b.start()
      expectActorState(b, Actor.State.Suspended)

      b ! 'go
      expectActorState(b, Actor.State.TimedSuspended)

      b ! 'go
      expectActorState(b, Actor.State.Blocked)

      b ! 'go
      expectActorState(b, Actor.State.TimedBlocked)

      b ! 'go
      expectActorState(b, Actor.State.Terminated)
    }
  }

}
