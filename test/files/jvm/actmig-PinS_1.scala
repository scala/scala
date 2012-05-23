import scala.actors._

object SillyActor {
  val ref = MigrationSystem.actorOf(Props(() => new SillyActor, "akka.actor.default-stash-dispatcher"))
}

/* PinS, Listing 32.1: A simple actor
 */
class SillyActor extends Actor {
  def act() {
    for (i <- 1 to 5) {
      println("I'm acting!")
      Thread.sleep(10)
    }
  }
}

object SeriousActor {
  val ref = MigrationSystem.actorOf(Props(() => new SeriousActor, "akka.actor.default-stash-dispatcher"))
}

class SeriousActor extends Actor {
  def act() {
    for (i <- 1 to 5) {
      println("To be or not to be.")
      //Thread.sleep(1000)
      Thread.sleep(10)
    }
  }
}

/* PinS, Listing 32.3: An actor that calls react
 */
object NameResolver extends Actor {
  import java.net.{ InetAddress, UnknownHostException }

  def act() {
    react {
      case (name: String, actor: Actor) =>
        actor ! getIp(name)
        act()
      case "EXIT" =>
        println("Name resolver exiting.")
      // quit
      case msg =>
        println("Unhandled message: " + msg)
        act()
    }
  }

  def getIp(name: String): Option[InetAddress] = {
    try {
      Some(InetAddress.getByName(name))
    } catch {
      case _: UnknownHostException => None
    }
  }

}

object Test extends App {

  /* PinS, Listing 32.2: An actor that calls receive
   */
  def makeEchoActor(): ActorRef = MigrationSystem.actorOf(Props(() => new Actor {
    def act() {
      while (true) {
        receive {
          case 'stop =>
            exit()
          case msg =>
            println("received message: " + msg)
        }
      }
    }
  }, "akka.actor.default-stash-dispatcher"))

  /* PinS, page 696
   */
  def makeIntActor(): ActorRef = MigrationSystem.actorOf(Props(() => new Actor {
    def act() {
      receive {
        case x: Int => // I only want Ints
          println("Got an Int: " + x)
      }
    }
  }, "akka.actor.default-stash-dispatcher"))

  MigrationSystem.actorOf(Props(() => new Actor {
    def act() {
      trapExit = true
      link(SillyActor.ref)
      react {
        case Exit(_: SillyActor, _) =>
          link(SeriousActor.ref)
          react {
            case Exit(_: SeriousActor, _) =>
              // PinS, page 694
              val seriousActor2 = MigrationSystem.actorOf(Props(() =>
                new Actor {
                  def act() {
                    for (i <- 1 to 5) {
                      println("That is the question.")
                      //Thread.sleep(1000)
                      Thread.sleep(10)
                    }
                  }
                }
              , "akka.actor.default-stash-dispatcher"))

              Thread.sleep(200)
              val echoActor = makeEchoActor()
              link(echoActor)
              echoActor ! "hi there"
              echoActor ! 15
              echoActor ! 'stop

              react {
                case Exit(_, _) =>
                  val intActor = makeIntActor()
                  intActor ! "hello"
                  intActor ! math.Pi
                  // only the following send leads to output
                  intActor ! 12
              }
          }
      }
    }
  }, "akka.actor.default-stash-dispatcher"))
}
