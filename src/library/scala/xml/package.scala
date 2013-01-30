/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

package object xml {
  val XercesClassName = "org.apache.xerces.parsers.SAXParser"

  type SAXException       = org.xml.sax.SAXException
  type SAXParseException  = org.xml.sax.SAXParseException
  type EntityResolver     = org.xml.sax.EntityResolver
  type InputSource        = org.xml.sax.InputSource
  type SAXParser          = javax.xml.parsers.SAXParser
}
