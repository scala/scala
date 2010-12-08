/*
 *  @author Stephane Micheloud
 */

import scala.actors.Actor._, ClientHelper._
import scala.actors.remote._, RemoteActor._
import scala.remoting._, Debug._

object Foo {
  def trace(msg: String) { info("[Foo.trace] "+msg)}
}
object Client {
  val yInstVal: Int = 10
  var yInstVar: Int = 99
  object Bar {
    def trace(msg: String) { info("[Bar.trace] "+msg) }
  }
  def main(args: Array[String]) {
    init(args)
    actor {
      val server = select(Node(host, port), 'Server)
      val zLocVal: Int = 1000
      var zLocVar: Int = 9998
      server ! detach(
        (x: Int) => {
          println("yInstVal = "+yInstVal)
          this.trace("yInstVar = "+yInstVar)
          Bar.trace("zLocVal = "+zLocVal)
          Foo.trace("zLocVar = "+zLocVar)
          zLocVar += 2
          System.out.println("zLocVal = "+zLocVal)
          Debug.info("zLocVar = "+zLocVar)
          x + yInstVal + yInstVar + zLocVal + zLocVar
        })
      react {
        case result: Int =>
          println("result received: " + result)
          Predef.exit(0)
      }
    }
  }
  private def trace(msg: String) { info("[Client.trace] "+msg) }
}

object ClientHelper {
  private var _host = "127.0.0.1"
  private var _port = 8888
  def host = _host
  def port = _port
  def init(args: Array[String]) {
    try { _host = args(0) } catch { case _ => }
    try { _port = args(1).toInt } catch { case _ => }
  }
}
