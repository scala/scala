
package scala.tools.servletEngine.http;
import java.io._;
import java.lang._;
import java.util._;
import scala.Symbol;
import scala.xml._;
/**
La classe HttpOutputStream fournit un support spécialisé pour écrire les reponses aux requêtes HTTP
*/
class HttpOutputStream(out:OutputStream , in: HttpInputStream ) extends  BufferedOutputStream(out){
  var code: int = 0;
  var sendHeaders1 :  boolean=true;
  var sendBody:  boolean= true;
  var headers:  Hashtable  = new Hashtable ();
  var cookieEnable:boolean = false;
  var currentCookie:ScalaCookiee = null  ;

  code = HTTP.STATUS_OKAY;
  setHeader ("server", HTTP.SERVER_INFO);
  setHeader("Date", new Date ().toString());
  sendHeaders1 = (in.getVersion() >= 1.0);
  sendBody = !HTTP.METHOD_HEAD.equals (in.getMethod());

  def setCode ( code:int):unit={
    this.code=code;
  }

  def setHeader (attr:String , value:String ) :unit ={
    /*
    put(Object key, Object value)
    fait correspondre la clé specifié à la valeur specifié dans la hashtable
    */
    headers.put(attr, value);
    val i =0;
  }


  def sendHeaders() : boolean={
    if(sendHeaders1) {
      write ("HTTP/1.0" +code + " " + HTTP.getCodeMessage(code)+ "\r\n");
      /* la mehode keys() Retourne une enumeration des clés de cette table de
      hashage */
      write("Date: "+headers.get("Date"));
      headers.remove("Date");
      if(cookieEnable){
	write("\nSet-Cookie:"+currentCookie.getName()+"="+currentCookie.getValue()+"\n");
      }
      val attrs = headers.keys ();/*return Enumeration type*/
      while (attrs.hasMoreElements()){
	val attr = attrs.nextElement().asInstanceOf[String];
	write (attr + ":" + headers.get(attr) + "\r\n");
      }
      write ("\n");
    }
    return sendBody;
  }


  def write (msg: String ): unit ={
    var b:Array[byte] = msg.getBytes("latin1");
    write (b,0,b.length);
  }

  override def write(bytes:Array[scala.Byte],offs:int,len:int):unit = {
    super.write (bytes,offs,len);
  }


  override def write(i:int):unit ={
  }


  def write (in: InputStream ):unit={
    //buf le buffer interne dans lequel les données sont enregistrées
    var n = buf.length;
    var length = buf.length;
    n = in.read(buf, count, length - count);
    while (n >= 0){
      count = count + n;
      if (count >= length){
	count =0;
	out.write(buf,count,length);
      }
      n = in.read(buf, count, length - count);

    }
  }


  def setCookie(c : ScalaCookiee):unit={
    cookieEnable= true;
    currentCookie = c;
  }


  def disableCookie():unit={
    cookieEnable= false;
    currentCookie =null;
  }

}
