package scala.tools.servlet.http;

import servlet.engine.config.HttpConnectorConfig;

/** for every port, there is one connector that instantiates a handler
 *  per client connection
 */
class HttpConnector(port: Int, map: PartialFunction[String,String]) extends servlet.engine.Connector(port) {

  def makeHandler(s: java.net.Socket) = new HttpHandler( s, map );

}
