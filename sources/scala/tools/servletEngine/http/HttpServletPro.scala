package scala.tools.servletEngine.http;

import java.io._;
import java.net._;
import scala.collection.mutable.HashMap;


class HttpServletPro(in: HttpInputStream, info: HashMap[String,String]) with HttpProcessor{

  var servletName="" ;
  val servletInfo ="";
  var servlet = new File (HTTP.SERVER_LOCATION,HTTP.translateFilename("src"));
  val  contentLength = 0;
  //val repertoire = extract();

  var servletClassName:String = extract();

  /*
  if(!servlet.exists()){
    throw new HttpException (HTTP.STATUS_NOT_FOUND, "la servlet <TT>"+ servletName + "</TT> n'est pas trouvée ");}
  if (!servlet.isFile())
    throw new HttpException (HTTP.STATUS_FORBIDDEN, servletName );
  */

  // assumption: in.getPath() is a string a/b/c, looks up servlet by c
    final def extract():String ={
      val  path = in.getPath();// retourne /servlet/repertoire/servletName
      Console.println("HttpServletPro:path = "+path);
      var repertoireExtract="";
      var repertoireIdx = path.indexOf('/', 1);
      Console.println("HttpServletPro:repIdx = "+repertoireIdx);
      var servletIdx = path.indexOf('/', repertoireIdx+1 );
      Console.println("HttpServletPro:servletIdx = "+servletIdx);
      repertoireExtract = path.substring(repertoireIdx+1,servletIdx );
      Console.println("HttpServletPro:repExtr = "+repertoireExtract);

      var servletExtract = path.substring(servletIdx +1 );
      Console.println("HttpServletPro:servletExtract = "+servletExtract);
      var ma = new Mapping();
      //var servletRequested = ma.switch(servletExtract);
      val res = ma.switch(servletExtract) + "$class";
      Console.println("mapping: servletRequested = "+res);
      res
      //servletName = "/".concat(ma.switch(servletRequested));
      //servlet = new File (HTTP.SERVER_LOCATION,HTTP.translateFilename("src"));
      //servlet = new File (servlet,HTTP.translateFilename(repertoireExtract));
      //servlet = new File (servlet,HTTP.translateFilename(servletName.substring(1)));

      //return repertoireExtract ;
    }




  override def processRequest(out: HttpOutputStream): Unit ={
    out.setHeader("Content-Type", "text/html");
    val sc1:ScalaCookiee = new ScalaCookiee("Server", "test");
    out.setCookie(sc1);
    val x= out.sendHeaders ();

    try{
      /*
      val idx = servletName.indexOf('.');
      val s1 = servletName.substring(1,idx);
      val s2 =repertoire.concat(".").concat(s1).concat("$class");
      var servlet1 = Class.forName(s2);
      */
      Console.println("Getting class "+servletClassName);
      val servlet1 = Class.forName( servletClassName );
      var servletInstance = servlet1.newInstance().asInstanceOf[ScalaServlet];
      servletInstance.doGet(out,info);
   }

    catch{
      case e :ClassNotFoundException => System.out.println("error:attention le nom de la servlet est incorect");
      case e :InstantiationException => System.out.println("InstantiationException"); e.printStackTrace();
      case e : IllegalAccessException => System.out.println("IllegalAccessException");
      case e:Exception =>
	System.out.println("une erreur inconue dans al aclasse HttpServletPro");
	e.printStackTrace();
    }

  }
}
