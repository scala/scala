package scala.tools.servletEngine;

import java.net.{ Socket, ServerSocket };

/** for every port, there is one connector that instantiates a handler
 *  per client connection
 */
abstract class Connector(thePort: Int) extends Thread {
  super.setDaemon(true);

  /** concrete instances of Connectors must override this with a factory
   *  for protocol handlers
   */
  def makeHandler(s: Socket): Thread; //Handler;

  final val port          = thePort;
  final val serverSocket  = new ServerSocket(thePort);

  // @todo: handler pooling
  final override def run() = while(true) {
    val client = serverSocket.accept(); /* returns a socket upon connection */
    val t = makeHandler(client);
    t.start();
  }
}
