package scala.tools.servletEngine;
import java.io._;
import scala.xml._;
import scala.collection.mutable.HashMap ;

class ExcepServlet() extends ScalaServlet{

override def doGetXML(info: HashMap[String,String]): Node = {
  val code:String= info("code");
  val detail:String=info("detail");

  <html>
    <head>
      <title>SERVLET ERROR</title>
    </head>
    <body>
      <big>Attention,erreur dans la servlet</big>
      <br/>type de l erreur: { code }<br/>{ detail }
    </body>
  </html>
}

}
