package scala.tools.servlet.http;

import java.io._;
import java.net._;
import java.lang._;
import scala.collection.mutable.HashMap;
import java.util.{ NoSuchElementException, StringTokenizer };
/**
La classe HttpInputStream fournit un support specialisé pour lire les requêtes HTTP
*/
class HttpInputStream(in: InputStream) extends BufferedInputStream(in) {
  var method = "GET" ;
  var queryString = "initialisation" ;
  var path= "index.html";
  var initialRequest =" ";
  var version: double = 0;
  val headers: HashMap[String, String] = new HashMap[String, String]();

  def readRequest(): Unit = {
    //System.out.println((HTTP.SERVER_LOCATION));
    val request: String = readLine();
    if (request == null)
      throw new HttpException (HTTP.STATUS_BAD_REQUEST,"Null query");
    val parts = new StringTokenizer(request);

    try{
      // ici on va parcourir la requete en cherchant les informations requises
      if (parts.hasMoreTokens ())
	parseMethod (parts.nextToken());
      if (parts.hasMoreTokens ()){
	initialRequest = parts.nextToken();
	parseRequest (initialRequest);
      }
    }

    catch{
      case ex:NoSuchElementException => throw new HttpException(HTTP.STATUS_BAD_REQUEST, request);
    }

    if (parts.hasMoreTokens ())
      parseVersion (parts.nextToken ());
    else
      version= 0.9;
    if((version >= 1.0)&&(method == HTTP.METHOD_HEAD))
      throw new HttpException (HTTP.STATUS_NOT_ALLOWED, method);
    if(version >= 1.0)
      readHeaders();

    }


  // on recupere grace à cette methode la methode (get, header ou post)demandé
  def  parseMethod(method: String ): Unit= method.match {
    case HTTP.METHOD_GET
         | HTTP.METHOD_HEAD
         | HTTP.METHOD_POST => this.method = method;
    case _ =>
      throw new HttpException (HTTP.STATUS_NOT_IMPLEMENTED, method);
  }

  /// on recupere grace à cette methode le URI demandé
  def parseRequest(request: String): Unit ={
    if(!request.startsWith("/"))
      throw new HttpException (HTTP.STATUS_BAD_REQUEST,request);
    val queryIdx  = request.indexOf('?');
    if (queryIdx == -1){
      path = HTTP.canonicalizePath(request);
      queryString ="";
    }
    else{
      path = HTTP.canonicalizePath (request.substring(0, queryIdx));
      queryString = request.substring(queryIdx + 1);
      }
  }

  //la methode suivante parse la version
  def parseVersion(verStr: String) : unit ={
    if(!verStr.startsWith("HTTP/"))
      throw new HttpException (HTTP.STATUS_BAD_REQUEST,verStr);

    try{
      version = Float.valueOf (verStr.substring(5)).floatValue ();}

    catch {
      case e:NumberFormatException => throw new HttpException(HTTP.STATUS_BAD_REQUEST, verStr);
      case defaul:Exception => System.out.println("sdfew");}
  }

  //// la methode suivante lit les headers des requetes,on les lit jusqu a ce qu
  //// on trouve une ligne vide
  def readHeaders (): unit ={
    var header = readLine ();
    while ((header !=null) &&  !header.equals("")){
      val colonIdx:int = header.indexOf(':');
      if (colonIdx != -1){
	val name = header.substring(0, colonIdx);
	val value = header.substring(colonIdx + 1);
	val _ = headers.update(name.toLowerCase(), value.trim());
      }
      header = readLine ();
    }
  }

  def readLine (): String = {
    val line = new StringBuffer();
    var c = read();
    if(c == -1)
      return "error";
    while ((c != -1)&&(c != '\n')&&(c != '\r')){
      line.append( c.asInstanceOf[char] );
      c = read();
    }
    if (c == '\r')
      pos = pos - 1;
    return line.toString();
  }

  def getMethod () : String ={
    return method;
  }

  def getPath (): String ={
    return path;
  }

  def getQueryString (): String ={
    return queryString;}

  def getVersion(): double ={
    return version;}

  def getHeader(name:String): String ={
    return headers(name.toLowerCase());
  }

  def getHeaderNames(): Iterator[String] ={
    return headers.keys;
  }

  def getInitialRequest (): String ={
    return initialRequest;
  }

}

