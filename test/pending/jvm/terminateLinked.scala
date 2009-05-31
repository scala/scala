import scala.actors.Actor
import Actor._

object Test {
  def main(args: Array[String]) {
    val a = actor {
      for (_ <- 1 to 10)
        receive {
          case b: Actor => link(b)
        }
      throw new Exception
    }

    for (_ <- 1 to 10)
      actor {
        a ! self
        react {
          case _ =>
        }
      }

    println("Done")
  }
}
