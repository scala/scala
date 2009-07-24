/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml
package include.sax
import scala.xml.include._

import org.xml.sax.SAXException
import org.xml.sax.SAXParseException
import org.xml.sax.EntityResolver
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.XMLReader

object Main {

    /**
      * The driver method for xinc
      * Output is written to System.out via Conolse
      * </p>
      *
      * @param args  contains the URLs and/or filenames
      *              of the documents to be procesed.
      */
    def main(args: Array[String]) {
      var parser: XMLReader = null
      var err = false
      try {
        parser = XMLReaderFactory.createXMLReader()
      }
      catch {
        case e:SAXException =>
          try {
            parser = XMLReaderFactory.createXMLReader(
              "org.apache.xerces.parsers.SAXParser")
          } catch {
            case e2:SAXException =>
              System.err.println("Could not find an XML parser")
              err = true
          }
      }

      if(err) return;
      // Need better namespace handling
      try {
        parser.setFeature("http://xml.org/sax/features/namespace-prefixes", true);
      }
      catch {
        case e:SAXException =>
          System.err.println(e)
          err = true
      }
      if (err) return

      if (args.length == 0) return
      var resolver: EntityResolver = null
      var arg: Int = 0
      if (args(0).equals("-r")) {
        try {
          resolver = Class.forName(args(1)).newInstance().asInstanceOf[EntityResolver];
          parser.setEntityResolver(resolver);
        }
        catch {
          case ex:Exception =>
            System.err.println("Could not load requested EntityResolver")
            err = true
        }
        arg = 2
      }
      if (err) return

      while (arg < args.length) {
        try {
          val includer = new XIncludeFilter();
          includer.setParent(parser)
          val s = new XIncluder(System.out, "UTF-8")
          includer.setContentHandler(s)
          if (resolver != null) includer.setEntityResolver(resolver)
          try {
            includer.setProperty(
              "http://xml.org/sax/properties/lexical-handler",
              s)
            s.setFilter(includer)
          }
          catch {
            case e:SAXException => // Will not support comments
          }
          includer.parse(args(arg))
        }
        catch {
          case e:SAXParseException =>
            System.err.println(e)
            System.err.println("Problem in " + e.getSystemId()
                               + " at line " + e.getLineNumber())
          case e: Exception => // be specific about exceptions????
            System.err.println(e)
            e.printStackTrace()
        }
        arg += 1
      }
    }
}
