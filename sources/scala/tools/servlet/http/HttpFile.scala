package scala.tools.servlet.http;

import java.io._;
import java.net._;
import scala.collection.mutable.HashMap;

/**
La classe HttpFile implémente le trait HttpProcessor et permit de servir des fichiers statiques depuis le répétoire local des fichiers
*/

class HttpFile(in: HttpInputStream, info: HashMap[String, String]) with HttpProcessor{

  if (in.getMethod() == HTTP.METHOD_HEAD){
    throw new HttpException (HTTP.STATUS_NOT_ALLOWED,"<TT>" + in.getMethod ()+ " "+ in.getPath() +"</TT>");
  }

  var file = new File (HTTP.HTML_ROOT, HTTP.translateFilename(in.getPath()));
  if(in.getPath().endsWith("/"))
    file = new File(file, HTTP.DEFAULT_INDEX);
  if(!file.exists())
    throw new HttpException (HTTP.STATUS_NOT_FOUND,"le Fichier <TT>" +in.getPath() + "</TT> n'est pas trouvé");
  if(file.isDirectory())
    throw new HttpException (HTTP.STATUS_MOVED_PERMANENTLY,in.getPath() + "/");
  if (!file.isFile() || !file.canRead())
    throw new HttpException (HTTP.STATUS_FORBIDDEN,
			     in.getPath());


  override def processRequest(out: HttpOutputStream): Unit ={
    out.setHeader("Content-type", HTTP.guessMimeType(file.getName()));
    out.setHeader("Contents-length", String.valueOf(file.length()));
    if(out.sendHeaders()) {
      val  in = new FileInputStream(file);
      out.write(in);
      in.close();
    }
  }
}
