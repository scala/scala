import scala.actors.Actor
object Test extends Application {
  Actor.actor {
    Actor.reactWithin(1) { case x => println("OK1") }
  }
  Thread.sleep(500) // Wait for scheduler to poll ActorGC
  Actor.actor {
    Actor.reactWithin(1) { case x => println("OK2") }
  }
  Thread.sleep(500)
}
