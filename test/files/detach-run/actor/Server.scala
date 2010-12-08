/*
 *  @author Stephane Micheloud
 */

import scala.actors.Actor._
import scala.actors.remote.RemoteActor._

object Server extends ServerConsole {
  private def computation(f: Int => Int): Int = {
    //some time-consuming task
    f(2)
  }
  def main(args: Array[String]) {
    actor {
      classLoader = serverClassLoader
      alive(args(0).toInt)
      register('Server, self)
      loopWhile(isRunning) {
        react {
          case f: (Int => Int) =>
            val result = computation(f)
            sender ! result
        }
      }
    }
  }
}
