package scala.tools.servlet;

import java.io.IOException;
import scala.xml._;
import scala.collection.mutable.HashMap ;
import http.HttpOutputStream;
// import scala.collection.mutable.HashMap
// val x = new HashMap[String,String]
// x.update("key","value");
// x.get("key") match {
//   case Some( value ) => ...
//   case None          => ...
// } x("key")

abstract class ScalaServlet {
  var output:HttpOutputStream = null;
  // HashMap[String,String]
  def doGetXML(info: HashMap[String,String]): scala.xml.Node ;

  final def doGet(out: HttpOutputStream, info: HashMap[String,String]): Unit= {
    try{
      out.write( doGetXML( info ).toString() );
    }
    catch {
      case sException:ServletException => ReturnException( sException.returnType(),sException.returnInfo());
      case ex:Exception => ReturnException(30,"");
    }
  }

  final def ReturnException(code:int, detail :String):unit={
    var info = new HashMap[String,String];
    info.update("code", SERVLET.getCodeMessage(code) );
    info.update("detail", detail);
    new ExcepServlet().doGet(output, info);
    return null;

  }
}





















