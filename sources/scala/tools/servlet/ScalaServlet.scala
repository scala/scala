package scala.tools.servlet;

import java.io.IOException;
import scala.xml._;
import scala.collection.mutable.HashMap ;
import http.HttpOutputStream;

object ScalaServlet {
  final def showError(out: HttpOutputStream, status:int, msg :String): Unit =
    out.write(
  <html>
    <head>
      <title>SERVLET ERROR</title>
    </head>
    <body>
      <big>Attention,erreur dans la servlet</big>
      <br/>type de l erreur: { Status.getMessage(status) }<br/>{ msg }
    </body>
  </html>.toString());
}

/** subclasses can be registered with the servlet engine to handle requests
 */
abstract class ScalaServlet {

  var output:HttpOutputStream = null;
  // HashMap[String,String]
  def doGetXML(info: HashMap[String,String]): scala.xml.Node ;

  final def doGet(out: HttpOutputStream, info: HashMap[String,String]): Unit= {
    try{
      out.write( doGetXML( info ).toString() );
    }
    catch {
      case ServletException(status, msg) =>
        ScalaServlet.showError(out, status, msg );
      case ex:Exception =>
        ScalaServlet.showError(out, Status.INTERNAL_ERROR, ex.getMessage());
    }
  }

}





















