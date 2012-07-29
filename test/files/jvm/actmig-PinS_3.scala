import scala.actors.{ MigrationSystem, StashingActor, ActorRef, Terminated, Props }
import scala.concurrent.util.duration._
import scala.concurrent.{ Promise, Await }


object SillyActor {
  val startPromise = Promise[Boolean]()
  val ref = MigrationSystem.actorOf(Props(() => new SillyActor, "default-stash-dispatcher"))
}

/* PinS, Listing 32.1: A simple actor
 */
class SillyActor extends StashingActor {
  def receive = { case _ => println("Why are you not dead"); context.stop(self) }

  override def preStart() {
    Await.ready(SillyActor.startPromise.future, 5 seconds)
    for (i <- 1 to 5)
      println("I'm acting!")
    context.stop(self)
  }

  override def postStop() {
    println("Post stop")
  }
}

object SeriousActor {
  val startPromise = Promise[Boolean]()
  val ref = MigrationSystem.actorOf(Props(() => new SeriousActor, "default-stash-dispatcher"))
}

class SeriousActor extends StashingActor {
  def receive = { case _ => println("Nop") }
  override def preStart() {
    Await.ready(SeriousActor.startPromise.future, 5 seconds)
    for (i <- 1 to 5)
      println("To be or not to be.")
    context.stop(self)
  }
}

/* PinS, Listing 32.3: An actor that calls react
 */
object NameResolver {
  val ref = MigrationSystem.actorOf(Props(() => new NameResolver, "default-stash-dispatcher"))
}

class NameResolver extends StashingActor {
  import java.net.{ InetAddress, UnknownHostException }

  def receive = {
    case (name: String, actor: ActorRef) =>
      actor ! getIp(name)
    case "EXIT" =>
      println("Name resolver exiting.")
      context.stop(self) // quit
    case msg =>
      println("Unhandled message: " + msg)
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
  def makeEchoActor(): ActorRef = MigrationSystem.actorOf(Props(() => new StashingActor {

    def receive = { // how to handle receive
      case 'stop =>
        context.stop(self)
      case msg =>
        println("received message: " + msg)
    }
  }, "default-stash-dispatcher"))

  /* PinS, page 696
   */
  def makeIntActor(): ActorRef = MigrationSystem.actorOf(Props(() => new StashingActor {

    def receive = {
      case x: Int => // I only want Ints
        unstashAll()
        println("Got an Int: " + x)
        context.stop(self)
      case _ => stash()
    }
  }, "default-stash-dispatcher"))

  MigrationSystem.actorOf(Props(() => new StashingActor {
    val silly = SillyActor.ref

    override def preStart() {
      context.watch(SillyActor.ref)
      SillyActor.startPromise.success(true)
    }

    def receive = {
      case Terminated(`silly`) =>
        unstashAll()
        val serious = SeriousActor.ref
        context.watch(SeriousActor.ref)
        SeriousActor.startPromise.success(true)
        context.become {
          case Terminated(`serious`) =>
            val seriousPromise2 = Promise[Boolean]()
            // PinS, page 694
            val seriousActor2 = MigrationSystem.actorOf(Props(() => {
              new StashingActor {

                def receive = { case _ => context.stop(self) }

                override def preStart() = {
                  for (i <- 1 to 5)
                    println("That is the question.")
                  seriousPromise2.success(true)
                  context.stop(self)
                }
              }
            }, "default-stash-dispatcher"))

            Await.ready(seriousPromise2.future, 5 seconds)
            val echoActor = makeEchoActor()
            context.watch(echoActor)
            echoActor ! "hi there"
            echoActor ! 15
            echoActor ! 'stop
            context.become {
              case Terminated(_) =>
                unstashAll()
                val intActor = makeIntActor()
                intActor ! "hello"
                intActor ! math.Pi
                // only the following send leads to output
                intActor ! 12
                context.unbecome()
                context.unbecome()
                context.stop(self)
              case m =>
                println("Stash 1 " + m)
                stash(m)
            }
          case m =>
            println("Stash 2 " + m)
            stash(m)
        }
      case m =>
        println("Stash 3 " + m)
        stash(m)
    }
  }, "default-stash-dispatcher"))
}