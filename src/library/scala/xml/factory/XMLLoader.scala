/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml
package factory

import javax.xml.parsers.SAXParserFactory
import parsing.{ FactoryAdapter, NoBindingFactoryAdapter }
import java.io.{ InputStream, Reader, StringReader, File, FileDescriptor, FileInputStream }

/** Presents collection of XML loading methods which use the parser
 *  created by "def parser".
 */
trait XMLLoader[T <: Node]
{
  import scala.xml.Source._
  def adapter: FactoryAdapter = new NoBindingFactoryAdapter()

  private val saxFactory = {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    // Discovered at: http://www.jdom.org/docs/faq.html#a0350 (see ticket #2725)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f
  }

  /* Override this to use a different SAXParser. */
  def parser: SAXParser = saxFactory.newSAXParser()

  /** Loads XML from the given InputSource, using the supplied parser.
   *  The methods available in scala.xml.XML use the XML parser in the JDK.
   */
  def loadXML(source: InputSource, parser: SAXParser): T = {
    val newAdapter = adapter

    newAdapter.scopeStack push TopScope
    parser.parse(source, newAdapter)
    newAdapter.scopeStack.pop

    newAdapter.rootElem.asInstanceOf[T]
  }

  /** Loads XML from the given file, file descriptor, or filename. */
  def loadFile(file: File): T          = loadXML(fromFile(file), parser)
  def loadFile(fd: FileDescriptor): T  = loadXML(fromFile(fd), parser)
  def loadFile(name: String): T        = loadXML(fromFile(name), parser)

  /** loads XML from given InputStream, Reader, sysID, InputSource, or URL. */
  def load(is: InputStream): T         = loadXML(fromInputStream(is), parser)
  def load(reader: Reader): T          = loadXML(fromReader(reader), parser)
  def load(sysID: String): T           = loadXML(fromSysId(sysID), parser)
  def load(source: InputSource): T     = loadXML(source, parser)
  def load(url: URL): T                = loadXML(fromInputStream(url.openStream()), parser)

  /** Loads XML from the given String. */
  def loadString(string: String): T    = loadXML(fromString(string), parser)
}
