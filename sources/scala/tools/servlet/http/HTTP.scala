package scala.tools.servlet.http;

import java.io._;
import java.net._;
import scala.collection.mutable.HashMap;

/** constants */
object HTTP {
  val  SERVER_INFO: String= "JNP-HTTPD/1.0";
  val  SERVLET: String= "/servlet/";
  val  SERVER_LOCATION: File= new File(System.getProperty("user.dir"));
  val  HTML_ROOT: File= new File(SERVER_LOCATION,"html");
  val  PORT:Int = 80  ;
  val  DEFAULT_INDEX: String = "index.html";

  final val METHOD_GET : String="GET";
  final val METHOD_POST : String="POST";
  final val METHOD_HEAD : String="HEAD";


  final val STATUS_OKAY  = 200;
  final val STATUS_NO_CONTENT= 204;
  final val STATUS_MOVED_PERMANENTLY= 301;
  final val STATUS_MOVED_TEMPORARILY=302;
  final val STATUS_BAD_REQUEST=400;
  final val STATUS_FORBIDDEN=403;
  final val STATUS_NOT_FOUND=404;
  final val STATUS_NOT_ALLOWED=405;
  final val STATUS_INTERNAL_ERROR =500;
  final val STATUS_NOT_IMPLEMENTED=501;

  def getCodeMessage (code:int): String = code match {

    case STATUS_OKAY =>  "OK"
    case STATUS_NO_CONTENT => "No Content"
    case STATUS_MOVED_PERMANENTLY =>  "Moved Permantely"
    case STATUS_MOVED_TEMPORARILY =>  "Moved Temporarily"
    case STATUS_BAD_REQUEST => "Bad Request"
    case STATUS_FORBIDDEN =>  "Forbidden"
    case STATUS_NOT_FOUND =>  "Not Found"
    case STATUS_NOT_ALLOWED =>  "Method Not Allowed"
    case STATUS_INTERNAL_ERROR =>  "Server Internal Error"
    case STATUS_NOT_IMPLEMENTED =>  "Not Implemented"
    case _ =>  "Unknown Code (" + code + ")"

  }

  def canonicalizePath(path: String):String={
    val chars:Array[char] = path.toCharArray();
    var length:int = chars.length;
    var idx:int =0;
    var odx:int =0;

    idx = indexOf (chars, length,'/',odx);
    while (  idx <  (length - 1)){
      val ndx:int = indexOf (chars, length,'/',idx + 1);
      var  kill = -1;
      if (ndx == idx + 1){
	kill = 1;
      }
      else if((ndx >= idx + 2) && (chars(idx + 1 )=='.')){
	if(ndx == idx + 2){
          kill = 2;
        } else if ((ndx == idx +3) && (chars(idx + 2) == '.')){

          kill = 3;
          while ((idx > 0) && (chars({idx = idx -1; idx}) !='/'))
          kill = kill +1;

        }}
      if (kill == -1){
        odx = ndx;
      } else if (idx +kill >= length){
        odx = idx +1;
	length =odx ;
      } else {
        length =length- kill;
        System.arraycopy(chars,idx +1+kill, chars,idx +1,length-idx-1);

        odx =idx;
      }
      idx = indexOf (chars, length,'/',odx);
    }

    return new String (chars, 0, length);
  }


  def indexOf (chars: Array[char],length:int,chr: char,from:int):int={
    var frome :int = from;
    while ((frome < length) && (chars(frome) != chr))
    frome=frome + 1;
    return frome
  }


  def translateFilename (filename:String ):String ={
    val result:StringBuffer  = new StringBuffer ();
    try {
      var idx = 0;
      var  odx = 0;
      idx = filename.indexOf ('/',odx);
      while (idx != -1){
	result.append(filename.substring(odx,idx)).append(File.separator);
	odx= idx +1;
	idx = filename.indexOf ('/',odx);
      }
      result.append(filename.substring(odx));}
    catch{
      case e :java.lang.NullPointerException => System.out.println("attention erreur");
    }

    return result.toString();
  }

  final var mimeTypes = new HashMap[String, String]();

  mimeTypes.update("gif", "image/gif");
  mimeTypes.update("jpeg", "image/jpeg");
  mimeTypes.update("jpg", "image/jpeg");
  mimeTypes.update("html", "text/html");
  mimeTypes.update("htm" , "text/html");

  def guessMimeType(fileName: String): String ={
    val i =fileName.lastIndexOf(".");
    val typ = mimeTypes.get(fileName.substring (i + 1).toLowerCase()).asInstanceOf[String];
    if (typ != null) {typ} else {"text/plain"};

  }


  def processing(inf: String): HashMap[String, String]={
    var  hashList = new HashMap[String, String];
    var info = inf;
    var i=occurence(info,"&");
    info = "&".concat(info);
    info = info.concat("&");
    var a= "";
    var b = "";
    var j= 0;
    /*
    si info = aaaa=3&bbbb=4&cccc=5 alors on le rend comme suit: &aaaa=3&bbbb=4&cccc=5&
    dans la boucle
    a = aaaa
    b= 3
    */
    while(j < i){
      var idxAND=info.indexOf("&");
      var idxEQUAL=info.indexOf("=");
      var idxSecondAND = info.indexOf("&",idxAND+1);
      a = info.substring(idxAND+1,idxEQUAL);
      b = info.substring(idxEQUAL+ 1,idxSecondAND);
      hashList.update(a, b);
      info = info.substring(idxSecondAND);
      j=j+1;
    }
    hashList;


  }
  def occurence(tsr:String,cr:String):int={
    var n= 1;
    var str = tsr;
    var idx = str.indexOf(cr);
    while (idx != -1){
      n = n+1;
      str=str.substring(idx+1);
      idx = str.indexOf(cr);
    }
    n;
  }
}
