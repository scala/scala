package scala.tools.nsc.io

import java.io.{File, FileInputStream, InputStream, IOException}
import java.net.{ContentHandler, ContentHandlerFactory, URL, URLConnection}
import java.nio.charset.CharsetDecoder

/**
 *  This class downloads the "code" field of a bug report of aladdin's bugtracking module
 *  and compiles it.
 *
 *  For instance, you can call the compiler with an argument "bug1023.scala"
 *  This file must exists locally, and contain a bug identifier, like "#1023".
 *  The AladdinReader will establish an HTTP connection and download bug #1023 - the file
 *  contents will be replaced by what is found in the bugtracking database.
 */
class AladdinReader(decoder: CharsetDecoder) extends SourceReader(decoder) {

  final val ALADDIN_URL = "http://scala-webapps.epfl.ch/bugtracking/bugs/json/"

  object AladdinHandler extends ContentHandler {
    def getContent(con: URLConnection) = {
      var state = 0
      val is = con.getInputStream
      val src = scala.io.Source.fromInputStream(is)
      while(state < 6 && src.hasNext) {
        state = (state,src.next) match {
          case (0,'\"') => 1
          case (1, 'c') => 2
          case (2, 'o') => 3
          case (3, 'd') => 4
          case (4, 'e') => 5
          case (5,'\"') => 6
          case _        => 0
        }
      }
      while(src.next != ':') {}
      while(src.next != '\"') {}
      val sb = new StringBuilder()
      var c = ' '
      while(c != '\"' && src.hasNext) { /*Console.print("["+c+"]"); */sb.append(c); c = src.next;}
      is.close
      //Console.println("!!"+sb.toString+"!!") // DEBUG if you want to see what you are downloading
      sb.toString.replace("\\u0022","\"").replace("\\uu005c","\\").toCharArray
    }
  }

  java.net.URLConnection.setContentHandlerFactory(new ContentHandlerFactory { def createContentHandler(s:String)   = AladdinHandler })

  private def wellformed(str: String): Boolean = try {
    str.charAt(0) == '#' && { Integer.parseInt(str.substring(1)); true }
  } catch {
    case _ => false
  }

  /** Reads the specified file. */
  @throws(classOf[java.net.MalformedURLException])
  override def read(file: File): Array[Char] = {

    val content = new String(super.read(file)).trim()
    if(!wellformed(content))
      throw FatalError("content should just be a bug identifier, like \"#1023\"")

    var bugURL  = ALADDIN_URL + new String(content).trim().substring(1)
    try {
      new java.net.URL(bugURL).getContent().asInstanceOf[Array[Char]]
    } catch {
      case e => throw FatalError("while getting content of URL, "+e.getClass()+" saying "+e.getMessage)
    }
  }

}
