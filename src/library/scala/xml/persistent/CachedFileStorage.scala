/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml
package persistent

import scala.io.File
import java.io.{ File => JFile, FileOutputStream }
import java.nio.ByteBuffer
import java.nio.channels.Channels

/** <p>
 *    Mutable storage of immutable xml trees. Everything is kept in memory,
 *    with a thread periodically checking for changes and writing to file.
 *    To ensure atomicity, two files are used, filename1 and '$'+filename1.
 *    The implementation switches between the two, deleting the older one
 *    after a complete dump of the database has been written.
 *  </p>
 *
 *  @author Burak Emir
 */
abstract class CachedFileStorage(private val file1: File)
extends java.lang.Thread with scala.util.logging.Logged
{
  private val file2 = (file1.parent.get / (file1.name + "$")).toFile

  /**  either equals file1 or file2, references the next file in which updates will be stored
   */
  private var theFile: File = null

  private def switch = { theFile = if (theFile == file1) file2 else file1; }

  /** this storage modified since last modification check */
  protected var dirty = false

  /** period between modification checks, in milliseconds */
  protected val interval = 1000

  /** finds and loads the storage file. subclasses should call this method
   *  prior to any other, but only once, to obtain the initial sequence of nodes.
   */
  protected lazy val initialNodes: Iterator[Node] = {
    val (e1, e2) = (file1.exists, file2.exists)

    theFile = if (e2 && (file2 isFresher file1)) file2 else file1
    if (!e1 && !e2) Iterator.empty else load
  }

  /** returns an iterator over the nodes in this storage */
  def nodes: Iterator[Node]

  /** adds a node, setting this.dirty to true as a side effect */
  def += (e: Node): Unit

  /** removes a tree, setting this.dirty to true as a side effect */
  def -= (e: Node): Unit

  /* loads and parses XML from file */
  private def load: Iterator[Node] = {
    import scala.io.Source
    import scala.xml.parsing.ConstructingParser

    log("[load]\nloading "+theFile)
    val src = theFile.chars()
    log("parsing "+theFile)
    val res = ConstructingParser.fromSource(src,false).document.docElem(0)
    switch
    log("[load done]")
    res.child.iterator
  }

  /** saves the XML to file */
  private def save = if (this.dirty) {
    log("[save]\ndeleting "+theFile);
    theFile.delete();
    log("creating new "+theFile);
    theFile.createFile();
    val fos = theFile.outputStream()
    val c   = fos.getChannel()

    // @todo: optimize
    val storageNode = <nodes>{ nodes.toList }</nodes>
    val w = Channels.newWriter(c, "UTF-8")
    XML.write(w, storageNode, "UTF-8", true, null)

    log("writing to "+theFile)

    w.close
    c.close
    fos.close
    dirty = false
    switch
    log("[save done]")
  }

  /** run method of the thread. remember to use start() to start a thread, not run. */
  override def run = {
    log("[run]\nstarting storage thread, checking every %d ms" format interval)

    while (true) {
      Thread sleep interval
      save
    }
  }

  /** forces writing of contents to the file, even if there has not been any update. */
  def flush = {
    this.dirty = true
    save
  }
}

