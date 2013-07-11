/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package xml

import parsing.NoBindingFactoryAdapter
import factory.XMLLoader
import java.io.{ File, FileDescriptor, FileInputStream, FileOutputStream }
import java.io.{ InputStream, Reader, StringReader, Writer }
import java.nio.channels.Channels
import scala.util.control.Exception.ultimately

object Source {
  def fromFile(file: File)              = new InputSource(new FileInputStream(file))
  def fromFile(fd: FileDescriptor)      = new InputSource(new FileInputStream(fd))
  def fromFile(name: String)            = new InputSource(new FileInputStream(name))

  def fromInputStream(is: InputStream)  = new InputSource(is)
  def fromReader(reader: Reader)        = new InputSource(reader)
  def fromSysId(sysID: String)          = new InputSource(sysID)
  def fromString(string: String)        = fromReader(new StringReader(string))
}

/**
 * Governs how empty elements (i.e. those without child elements) should be serialized.
 */
object MinimizeMode extends Enumeration {
  /** Minimize empty tags if they were originally empty when parsed, or if they were constructed
   *  with [[scala.xml.Elem]]`#minimizeEmpty` == true
   */
  val Default = Value

  /** Always minimize empty tags.  Note that this may be problematic for XHTML, in which
   * case [[scala.xml.Xhtml]]`#toXhtml` should be used instead.
   */
  val Always = Value

  /** Never minimize empty tags.
   */
  val Never = Value
}

/** The object `XML` provides constants, and functions to load
 *  and save XML elements. Use this when data binding is not desired, i.e.
 *  when XML is handled using `Symbol` nodes.
 *
 *  @author  Burak Emir
 *  @version 1.0, 25/04/2005
 */
object XML extends XMLLoader[Elem] {
  val xml       = "xml"
  val xmlns     = "xmlns"
  val namespace = "http://www.w3.org/XML/1998/namespace"
  val preserve  = "preserve"
  val space     = "space"
  val lang      = "lang"
  val encoding  = "ISO-8859-1"

  /** Returns an XMLLoader whose load* methods will use the supplied SAXParser. */
  def withSAXParser(p: SAXParser): XMLLoader[Elem] =
    new XMLLoader[Elem] { override val parser: SAXParser = p }

  /** Saves a node to a file with given filename using given encoding
   *  optionally with xmldecl and doctype declaration.
   *
   *  @param filename the filename
   *  @param node     the xml node we want to write
   *  @param enc      encoding to use
   *  @param xmlDecl  if true, write xml declaration
   *  @param doctype  if not null, write doctype declaration
   */
  final def save(
    filename: String,
    node: Node,
    enc: String = encoding,
    xmlDecl: Boolean = false,
    doctype: dtd.DocType = null
    ): Unit =
  {
    val fos = new FileOutputStream(filename)
    val w = Channels.newWriter(fos.getChannel(), enc)

    ultimately(w.close())(
      write(w, node, enc, xmlDecl, doctype)
    )
  }

  /** Writes the given node using writer, optionally with xml decl and doctype.
   *  It's the caller's responsibility to close the writer.
   *
   *  @param w        the writer
   *  @param node     the xml node we want to write
   *  @param enc      the string to be used in `xmlDecl`
   *  @param xmlDecl  if true, write xml declaration
   *  @param doctype  if not null, write doctype declaration
   */
  final def write(w: java.io.Writer, node: Node, enc: String, xmlDecl: Boolean, doctype: dtd.DocType, minimizeTags: MinimizeMode.Value = MinimizeMode.Default) {
    /* TODO: optimize by giving writer parameter to toXML*/
    if (xmlDecl) w.write("<?xml version='1.0' encoding='" + enc + "'?>\n")
    if (doctype ne null) w.write( doctype.toString() + "\n")
    w.write(Utility.serialize(node, minimizeTags = minimizeTags).toString)
  }
}
