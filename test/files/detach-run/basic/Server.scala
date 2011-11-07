/*
 *  @author Stephane Micheloud
 */

import scala.remoting.ServerChannel

object Server extends ServerConsole {
  private def computation(f: Int => Int): Int = {
    //some time-consuming task
    f(2)
  }
  def main(args: Array[String]) {
    val server = new ServerChannel(args(0).toInt)
    loop {
      val client = server.accept
      val f = client.receive[Int => Int]
      val result = computation(f)
      client ! result
    }
    server.close()
  }
}
