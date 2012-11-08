/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package persistent

import java.io.{ File, FileOutputStream }
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.lang.Thread
import scala.util.logging.Logged
import scala.collection.Iterator

/** Mutable storage of immutable xml trees. Everything is kept in memory,
 *  with a thread periodically checking for changes and writing to file.
 *
 *  To ensure atomicity, two files are used, `filename1` and `'$'+filename1`.
 *  The implementation switches between the two, deleting the older one
 *  after a complete dump of the database has been written.
 *
 *  @author Burak Emir
 */
abstract class CachedFileStorage(private val file1: File) extends Thread with Logged {

  private val file2 = new File(file1.getParent, file1.getName+"$")

  /** Either equals `file1` or `file2`, references the next file in which
   *  updates will be stored.
   */
  private var theFile: File = null

  private def switch() = { theFile = if (theFile == file1) file2 else file1; }

  /** this storage modified since last modification check */
  protected var dirty = false

  /** period between modification checks, in milliseconds */
  protected val interval = 1000

  /** finds and loads the storage file. subclasses should call this method
   *  prior to any other, but only once, to obtain the initial sequence of nodes.
   */
  protected def initialNodes: Iterator[Node] = (file1.exists, file2.exists) match {
    case (false,false) =>
      theFile = file1
      Iterator.empty
    case (true, true ) if (file1.lastModified < file2.lastModified) =>
      theFile = file2
      load
    case (true, _ ) =>
      theFile = file1
      load
    case _ =>
      theFile = file2
      load
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
    val src = Source.fromFile(theFile)
    log("parsing "+theFile)
    val res = ConstructingParser.fromSource(src,false).document.docElem(0)
    switch
    log("[load done]")
    res.child.iterator
  }

  /** saves the XML to file */
  private def save() = if (this.dirty) {
    log("[save]\ndeleting "+theFile)
    theFile.delete()
    log("creating new "+theFile)
    theFile.createNewFile()
    val fos = new FileOutputStream(theFile)
    val c   = fos.getChannel()

    // @todo: optimize
    val storageNode = <nodes>{ nodes.toList }</nodes>
    val w = Channels.newWriter(c, "utf-8")
    XML.write(w, storageNode, "utf-8", true, null)

    log("writing to "+theFile)

    w.close
    c.close
    fos.close
    dirty = false
    switch
    log("[save done]")
  }

  /** Run method of the thread. remember to use `start()` to start a thread,
    * not `run`. */
  override def run = {
    log("[run]\nstarting storage thread, checking every "+interval+" ms")
    while (true) {
      Thread.sleep( this.interval )
      save
    }
  }

  /** Force writing of contents to the file, even if there has not been any
    * update. */
  def flush() = {
    this.dirty = true
    save
  }
}
