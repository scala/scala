package scala.tools.servlet.engine;

import java.net.ServerSocket;

/** Main loop of the servlet engine, opens a ServerSocket
 *  and spawns a thread when accepting a client connections.
 *
 *  servletEngine depends on mappings
 *  <ul>
 *    <li>p: ports to protocol, and</li>
 *    <li>s: url to servlet</li>
 *  </ul>
 */
object Main {

  //@todo make this configurable with ports and handlers
  private var hcon: Thread = new servlet.http.HttpConnector(8000);
  def main(args: Array[String]): Unit = {
    Console.println("starting http connector at 8000");
    hcon.start();
    while(true) {
      Thread.sleep(50000)
    }
  }
}
