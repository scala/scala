package scala.tools.servletEngine.http;

import java.net.{ Socket, ServerSocket };

/** for every port, there is one connector that instantiates a handler
 *  per client connection
 */
class HttpConnector(port: Int) extends Connector(port) {

  def makeHandler(s: Socket) = new HttpHandler( s );

}
