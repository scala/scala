/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml.pull

import java.util.concurrent.LinkedBlockingQueue
import java.nio.channels.ClosedChannelException

import scala.io.Source
import scala.xml.parsing.{ ExternalSources, MarkupHandler, MarkupParser }

/** <p>
 *   A pull parser that offers to view an XML document as a series of events.
 *   Example usage:
 *  </p><pre>
 *  <b>import</b> scala.xml.pull._
 *  <b>import</b> scala.io.Source
 *
 *  <b>object</b> reader {
 *    <b>val</b> src = Source.fromString("<hello><world/></hello>")
 *    <b>val</b> er = new XMLEventReader(src)
 *
 *    <b>def</b> main(args: Array[String]) {
 *      while (er.hasNext)
 *        Console.println(er.next)
 *    }
 *  }
 *  </pre>
 *
 *  @author Burak Emir
 *  @author Paul Phillips
 */

class XMLEventReader(src: Source) extends ProducerConsumerIterator[XMLEvent]
{
  // We implement a pull parser as an iterator, but since we may be operating on
  // a stream (e.g. XML over a network) there may be arbitrarily long periods when
  // the queue is empty.  Fortunately the ProducerConsumerIterator is ideally
  // suited to this task, possibly because it was written for use by this class.

  // to override as necessary
  val preserveWS = true

  override val MaxQueueSize = 1000
  protected case object POISON extends XMLEvent
  val EndOfStream = POISON

  // thread machinery
  private[this] val parser = new Parser(src)
  private[this] val parserThread = new Thread(parser, "XMLEventReader")
  parserThread.start
  // enqueueing the poison object is the reliable way to cause the
  // iterator to terminate; hasNext will return false once it sees it.
  // Calling interrupt() on the parserThread is the only way we can get
  // it to stop producing tokens since it's lost deep in document() -
  // we cross our fingers the interrupt() gets to its target, but if it
  // fails for whatever reason the iterator correctness is not impacted,
  // only performance (because it will finish the entire XML document,
  // or at least as much as it can fit in the queue.)
  def stop = {
    produce(POISON)
    parserThread.interrupt()
  }

  private class Parser(val input: Source) extends MarkupHandler with MarkupParser with ExternalSources with Runnable {
    val preserveWS = XMLEventReader.this.preserveWS

    // this is Parser's way to add to the queue - the odd return type
    // is to conform to MarkupHandler's interface
    def setEvent(es: XMLEvent*): NodeSeq = {
      es foreach produce
      NodeSeq.Empty
    }

    override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
      setEvent(EvElemStart(pre, label, attrs, scope))
    }
    override def elemEnd(pos: Int, pre: String, label: String) {
      setEvent(EvElemEnd(pre, label))
    }

    // this is a dummy to satisfy MarkupHandler's API
    final def elem(pos: Int, pre: String, label: String, attrs: MetaData, pscope: NamespaceBinding, nodes: NodeSeq): NodeSeq = <ignore/>

    def procInstr(pos: Int, target: String, txt: String)  = setEvent(EvProcInstr(target, txt))
    def comment(pos: Int, txt: String)                    = setEvent(EvComment(txt))
    def entityRef(pos: Int, n: String)                    = setEvent(EvEntityRef(n))
    def text(pos: Int, txt:String)                        = setEvent(EvText(txt))

    override def run() {
      curInput = input
      interruptibly { this.nextch ; this.document() }
      setEvent(POISON)
    }
  }
}

// An iterator designed for one or more producers to generate
// elements, and a single consumer to iterate.  Iteration will continue
// until closeIterator() is called, after which point producers
// calling produce() will receive interruptions.
//
// Since hasNext may block indefinitely if nobody is producing,
// there is also an available() method which will return true if
// the next call hasNext is guaranteed not to block.
//
// This is not thread-safe for multiple consumers!
trait ProducerConsumerIterator[T >: Null] extends Iterator[T]
{
  // abstract - iterator-specific distinguished object for marking eos
  val EndOfStream: T

  // defaults to unbounded - override to positive Int if desired
  val MaxQueueSize = -1

  def interruptibly[T](body: => T): Option[T] = try Some(body) catch {
    case _: InterruptedException    => Thread.currentThread.interrupt() ; None
    case _: ClosedChannelException  => None
  }

  private[this] lazy val queue =
    if (MaxQueueSize < 0) new LinkedBlockingQueue[T]()
    else new LinkedBlockingQueue[T](MaxQueueSize)
  private[this] var buffer: T = _
  private def fillBuffer() = {
    buffer = interruptibly(queue.take) getOrElse EndOfStream
    isElement(buffer)
  }
  private def isElement(x: T) = x != null && x != EndOfStream
  private def eos() = buffer == EndOfStream

  // public producer interface - this is the only method producers call, so
  // LinkedBlockingQueue's synchronization is all we need.
  def produce(x: T): Unit = if (!eos) interruptibly(queue put x)

  // consumer/iterator interface - we need not synchronize access to buffer
  // because we required there to be only one consumer.
  def hasNext() = !eos && (buffer != null || fillBuffer)
  def next() = {
    if (eos) throw new NoSuchElementException("ProducerConsumerIterator")
    if (buffer == null) fillBuffer

    drainBuffer
  }
  def available() = isElement(buffer) || isElement(queue.peek)

  private def drainBuffer() = {
    assert(!eos)
    val res = buffer
    buffer = null
    res
  }
}
