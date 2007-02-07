/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml


import Predef._
import scala.xml.parsing.NoBindingFactoryAdapter
import org.xml.sax.InputSource
import java.io.{File, FileDescriptor, FileInputStream, FileOutputStream}
import java.io.{InputStream, Reader, StringReader, Writer}

/** The object <code>XML</code> provides constants, and functions to load
 *  and save XML elements. Use this when data binding is not desired, i.e.
 *  when XML is handled using <code>Symbol</code> nodes.
 *
 *  @author  Burak Emir
 *  @version 1.0, 25/04/2005
 */
object XML  {

  val xml       = "xml"
  val xmlns     = "xmlns"
  val namespace = "http://www.w3.org/XML/1998/namespace"
  val preserve  = "preserve"
  val space     = "space"
  val lang      = "lang"

  // functions for generic xml loading, saving

  /** loads XML from given file, using XML parser in JDK. */
  final def loadFile(file: File): Elem =
    new NoBindingFactoryAdapter().loadXML(new InputSource(
      new FileInputStream(file)
    ))

  /** loads XML from given file descriptor, using XML parser in JDK.
   *
   *  @param fileDesc ...
   *  @return         ...
   */
  final def loadFile(fileDesc: FileDescriptor): Elem =
    new NoBindingFactoryAdapter().loadXML(new InputSource(
      new FileInputStream(fileDesc)
    ))

  /** loads XML from given file, using XML parser in JDK. */
  final def loadFile(fileName: String): Elem =
    new NoBindingFactoryAdapter().loadXML(new InputSource(
      new FileInputStream(fileName)
    ));

  /** loads XML from given InputStream, using XML parser in JDK. */
  final def load( is:InputStream ): Elem =
    new NoBindingFactoryAdapter().loadXML(new InputSource(is))

  /** loads XML from given Reader, using XML parser in JDK. */
  final def load(reader: Reader): Elem =
    new NoBindingFactoryAdapter().loadXML(new InputSource(reader))

  /** loads XML from given sysID, using XML parser in JDK. */
  final def load(sysID: String): Elem =
    new NoBindingFactoryAdapter().loadXML(new InputSource(sysID))

  /** loads XML from a given input source, using XML parser in JDK.
   *
   *  @param source ...
   *  @return       ...
   */
  final def load(source: InputSource): Elem =
    new NoBindingFactoryAdapter().loadXML(source)

  /** loads XML from a string, using XML parser in JDK. */
  final def loadString(string: String): Elem =
    load(new StringReader(string))

  /** Saves XML to filename with encoding ISO-8859-1 without xml-decl without
   *  <code>doctype</code>.
   *
   *  @param filename ...
   *  @param node     ...
   */
  final def save(filename: String, node: Node): Unit =
    save(filename, node, "ISO-8859-1")

  /** saves XML to filename with given encoding, without xml-decl without
   *  <code>doctype</code>.
   *
   *  @param filename ...
   *  @param node     ...
   *  @param enc      ...
   */
  final def save(filename: String, node: Node, enc: String): Unit =
    saveFull(filename, node, enc, false, null);

  /** saves a node to a file with given filename using encoding iso-8859-1
   *  optionally with xmldecl and doctype declaration.
   *
   *  @param filename the filename
   *  @param node     the xml node we want to write
   *  @param xmlDecl  if true, write xml declaration
   *  @param doctype  if not null, write doctype declaration
   */
  final def saveFull(filename: String, node: Node, xmlDecl: Boolean, doctype: dtd.DocType): Unit =
    saveFull(filename, node, "ISO-8859-1", xmlDecl, doctype)

  /** Saves a node to a file with given filename using given encoding
   *  optionally with xmldecl and doctype declaration.
   *
   *  @param filename the filename
   *  @param node     the xml node we want to write
   *  @param enc      encoding to use
   *  @param xmlDecl  if true, write xml declaration
   *  @param doctype  if not null, write doctype declaration
   */

  final def saveFull(filename: String, node: Node, enc: String, xmlDecl: Boolean, doctype: dtd.DocType): Unit = {
    var fos: FileOutputStream = null
    var w: Writer = null
    try {
      // using NIO classes of JDK 1.4
      import java.io.{FileOutputStream, Writer}
      import java.nio.channels.{Channels, FileChannel}

      fos = new FileOutputStream(filename)
      w = Channels.newWriter(fos.getChannel(), enc)
      write(w, node, enc, xmlDecl, doctype)
    } finally {
      w.close()
      fos.close()
    }
  }

  /** Writes the given node using writer, optionally with xml decl and doctype.
   *  It's the caller's responsibility to close the writer.
   *
   *  @param w        the writer
   *  @param node     the xml node we want to write
   *  @param enc      the string to be used in <code>xmlDecl</code>
   *  @param xmlDecl  if true, write xml declaration
   *  @param doctype  if not null, write doctype declaration
   */
  final def write(w: java.io.Writer, node: Node, enc: String, xmlDecl: Boolean, doctype: dtd.DocType): Unit = {
    /* TODO: optimize by giving writer parameter to toXML*/
    if (xmlDecl) w.write("<?xml version='1.0' encoding='" + enc + "'?>\n")
    if (doctype ne null) w.write( doctype.toString() + "\n")
    w.write(Utility.toXML(node))
  }
}
