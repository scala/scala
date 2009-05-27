import scala.actors.Actor

/* Test that an actor that hasn't finished prevents termination */

object Test {
  def main(args: Array[String]) {
    Actor.actor {
      println("I'm going to make you wait.")
      Thread.sleep(5000)
      println("Ok, I'm done.")
    }
  }
}
