package scala

package object xml {
  val XercesClassName = "org.apache.xerces.parsers.SAXParser"

  type SAXException       = org.xml.sax.SAXException
  type SAXParseException  = org.xml.sax.SAXParseException
  type EntityResolver     = org.xml.sax.EntityResolver
  type InputSource        = org.xml.sax.InputSource

  type SAXParser          = javax.xml.parsers.SAXParser

  type IOException                  = java.io.IOException
  type UnsupportedEncodingException = java.io.UnsupportedEncodingException

  type URL                    = java.net.URL
  type MalformedURLException  = java.net.MalformedURLException
}