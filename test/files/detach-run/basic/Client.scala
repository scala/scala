/*
 *  @author Stephane Micheloud
 */

import java.net._, Thread._, ClientHelper._
import scala.remoting._, Debug._

object Foo {
  def trace(s: String) { info("[Foo.trace] "+s)}
}
object Client {
  val yInstVal: Int = 10
  var yInstVar: Int = 99
  object Bar {
    def trace(s: String) { info("[Bar.trace] "+s) }
  }
  def main(args: Array[String]) {
    init(args)
    val server = new Channel(host, port)
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
    val result = server.receiveInt
    println("result received: " + result)
  }
  private def trace(s: String) { info("[Client.trace] "+s) }
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
