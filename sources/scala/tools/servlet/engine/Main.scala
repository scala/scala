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

  final val sampleConfig =
        <engine xmlns="http://scala.epfl.ch/scala.tools.servlet.engine/config">
          <connector port="8000" protocol="http">
            <map url="/servlet/foo" to="app1.HelloWorldServlet"/>
          </connector>
        </engine>;


  def main(args: Array[String]): Unit = {
    if( args.length != 1 ) {

      Console.println("usage:");
      Console.println("scala scala.tools.servlet.engine.Main <config>");
      Console.println("  where <config> is the filename of a document like this one:");
      Console.println( new scala.xml.PrettyPrinter(80,5).format( sampleConfig ));

    } else {
      val fileName = args(0);
      Console.println("reading config file from \""+fileName+"\"");
      val theConfig = new config.ConfigReader(fileName).element;

      import config._ ;

      for( val c <- theConfig ) c match {
        case EngineConfig(list) =>
          for( val conn <- list ) conn match {
            case HttpConnectorConfig(port,map) =>
              Console.println("starting http connector at "+port);
              new http.HttpConnector(port, map).start(); //@todo
          }
      }
      //Console.println("starting http connector at 8000");
      //hcon.start();
      while(true) {
        Thread.sleep(50000)
      }
    }
  }
}
