import scala.actors.{ MigrationSystem, StashingActor, ActorRef, Props, Exit }
import scala.concurrent.util.duration._
import scala.concurrent.{ Promise, Await }

object SillyActor {
  val startPromise = Promise[Boolean]()
  val ref = MigrationSystem.actorOf(Props(() => new SillyActor, "default-stash-dispatcher"))
}

/* PinS, Listing 32.1: A simple actor
 */
class SillyActor extends StashingActor {

  def receive = { case _ => println("Nop") }

  override def act() {
    Await.ready(SillyActor.startPromise.future, 5 seconds)
    for (i <- 1 to 5)
      println("I'm acting!")

    println("Post stop")
  }
}

object SeriousActor {
  val startPromise = Promise[Boolean]()
  val ref = MigrationSystem.actorOf(Props(() => new SeriousActor, "default-stash-dispatcher"))
}

class SeriousActor extends StashingActor {
  def receive = { case _ => println("Nop") }
  override def act() {
    Await.ready(SeriousActor.startPromise.future, 5 seconds)
    for (i <- 1 to 5)
      println("To be or not to be.")
  }
}

/* PinS, Listing 32.3: An actor that calls react
 */
object NameResolver {
  val ref = MigrationSystem.actorOf(Props(() => new NameResolver, "default-stash-dispatcher"))
}

class NameResolver extends StashingActor {
  import java.net.{ InetAddress, UnknownHostException }

  def receive = { case _ => println("Nop") }

  override def act() {
    react {
      case (name: String, actor: ActorRef) =>
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
  def makeEchoActor(): ActorRef = MigrationSystem.actorOf(Props(() =>
    new StashingActor {
    def receive = { case _ => println("Nop") }

    override def act() {
      loop {
        react {
          case 'stop =>
            exit()
          case msg =>
            println("received message: " + msg)
        }
      }
    }
  }, "default-stash-dispatcher"))

  /* PinS, page 696
   */
  def makeIntActor(): ActorRef = MigrationSystem.actorOf(Props(() =>new StashingActor {

    def receive = { case _ => println("Nop") }

    override def act() {
      react {
        case x: Int => // I only want Ints
          println("Got an Int: " + x)
      }
    }
  }, "default-stash-dispatcher"))

 MigrationSystem.actorOf(Props(() => new StashingActor {

    def receive = { case _ => println("Nop") }

    override def act() {
      trapExit = true
      link(SillyActor.ref)
      SillyActor.startPromise.success(true)
      react {
        case Exit(_: SillyActor, _) =>
          link(SeriousActor.ref)
          SeriousActor.startPromise.success(true)
          react {
            case Exit(_: SeriousActor, _) =>
              val seriousPromise2 = Promise[Boolean]()
              // PinS, page 694
              val seriousActor2 = MigrationSystem.actorOf(Props(() =>{
                new StashingActor {

                  def receive = { case _ => println("Nop") }

                  override def act() {
                    for (i <- 1 to 5)
                      println("That is the question.")
                      seriousPromise2.success(true)
                  }
                }
              }, "default-stash-dispatcher"))

              Await.ready(seriousPromise2.future, 5 seconds)
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
  }, "default-stash-dispatcher"))
}
