package scala.tools.servletEngine.http;

import java.io._;
import java.net._;
import scala.collection.mutable.HashMap;

class HttpHandler(client: Socket) extends Thread {

  override def run(): Unit  ={

    try {
      val httpIn  = new HttpInputStream (client.getInputStream());
      var httpOut = new HttpOutputStream(client.getOutputStream(), httpIn);
      val processor= getProcessor(httpIn);
      processor.processRequest(httpOut);
      httpOut.flush();
    }

    catch {
      case e:IOException => e.printStackTrace();
    }

    try {
      client.close ();
    }

    catch {
      case e: IOException => e.printStackTrace();
    }

  }


    /* La methode run: on a construit premierement un HttpInputStream dans le
  client  Input Stream, ceci nous facilte lire et parser les requetes HTTP*/

  def getProcessor(httpIn: HttpInputStream): HttpProcessor={
    var value = 0;
    var infoString = " " ;
    var info:HashMap[String, String] = null;
    var error: HttpProcessor=null;
    var srv:HttpServletPro=null;
    var fil:HttpFile=null;

    try{
      httpIn.readRequest ();
      var index = httpIn.getInitialRequest().indexOf("?");
      if(index != -1){
	infoString = httpIn.getInitialRequest().substring(index+1);
	info = HTTP.processing(infoString);
      }

      if (httpIn.getPath().startsWith(HTTP.SERVLET)){
	value = 1;
	srv = new HttpServletPro(httpIn, info);
      }
      else{
	value = 2;
	fil = new HttpFile(httpIn, info);
      }
    }

    catch {
      case e: HttpException => {
	error=e.toProcessor;
	value=3;
      }
      case ex: Exception => {
	val trace = new StringWriter ();
	ex.printStackTrace(new PrintWriter (trace,true));
	val exept =  new HttpException(HTTP.STATUS_INTERNAL_ERROR,"<PRE>"+ trace +"</PRE>");
	value=3;
	error= exept.toProcessor;}
    }

    if(value == 1)
      return srv ;
    else if(value == 2)
      return  fil;
    else
      return error;
}
}
