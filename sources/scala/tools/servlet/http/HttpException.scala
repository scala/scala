package scala.tools.servlet.http;

import java.io._;

/**
La classe HttpException est une sous-classe de IOException, elle retourne au client une page HTML qui contient les erreurs dont les types se trouvent dans la classe HTTP.

*/
class HttpException(code : int, detail: String) extends IOException (detail) {


  def toProcessor = new HttpProcessor {

    override  def processRequest( out : HttpOutputStream ) : unit ={
     out.setCode(code);
      out.setHeader("Content-Type", "text/html");
      if(out.sendHeaders()){
	val msg = HTTP.getCodeMessage (code);
	out.write("<HTML><HEAD><TITLE>"+ code + " " +msg + "</TITLE></HEAD>\n"+"<BODY> <H1>" +
		  msg +"</H1>\n"+ getMessage () + "<P>\n </BODY></HTML>\n");
      }

    }

  }

}
