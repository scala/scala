package scala.tools.servlet;

import java.io.IOException;
import scala.xml._;
import scala.collection.mutable.HashMap ;
import http.HttpOutputStream;

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
        showError( status, msg );
      case ex:Exception =>
        showError(Status.INTERNAL_ERROR, ex.getMessage());
    }
  }

  final def showError(status:int, msg :String): scala.xml.Node = {
  <html>
    <head>
      <title>SERVLET ERROR</title>
    </head>
    <body>
      <big>Attention,erreur dans la servlet</big>
      <br/>type de l erreur: { Status.getMessage(status) }<br/>{ msg }
    </body>
  </html>
  }
}





















