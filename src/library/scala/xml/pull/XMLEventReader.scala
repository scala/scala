/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.pull


import java.lang.{Runnable, Thread}

import scala.io.Source
import scala.xml.parsing.{ExternalSources, MarkupHandler, MarkupParser}

/** <p>
 *   A pull parser that offers to view an XML document as a series of events.
 *   Please note that this API might change. Here's how to use this class
 *  </p><pre>
 *  <b>import</b> scala.xml._
 *  <b>import</b> scala.xml.pull._
 *  <b>import</b> scala.io.Source
 *
 *  <b>object</b> reader {
 *    <b>val</b> src = Source.fromString("<hello><world/></hello>")
 *    <b>val</b> er = new XMLEventReader().initialize(src)
 *
 *    <b>def</b> main(args: Array[String]) {
 *      Console.println(er.next)
 *      Console.println(er.next)
 *    }
 *  }
 *  </pre>
 *
 *  @author Burak Emir
 */
class XMLEventReader extends Iterator[XMLEvent] {

  var src:Source = null
  def getSource = this.src
  def initialize(src: Source): this.type = {
    this.src = src
    this.parserThread = new Thread(new Parser())
    this.parserThread.start()
    this
  }

  // -- this part of the class is for communication with the thread
  var xmlEvent: XMLEvent = null
  var continue: Boolean = true

  def myresume = synchronized {
    while (continue) {
      wait()
    }
    continue = true
    notify()
  }
  def getAndClearEvent: XMLEvent = synchronized {
    while (xmlEvent eq null) {
      wait()
    }
    val r = xmlEvent
    xmlEvent = null
    r
  }
  def setEvent(e: XMLEvent) {
    xmlEvent = e
  }

  def doNotify(): NodeSeq = synchronized {
    XMLEventReader.this.continue = false
    notify()
    while (!XMLEventReader.this.continue) {
      try { wait() } catch {
       case _: java.lang.InterruptedException => /* ignore */
	  }
	}
    NodeSeq.Empty
  }

  // iterator methods

  def next: XMLEvent = {
    myresume
    val r = getAndClearEvent
    r
  }

  def hasNext = true

  // After calling stop, one must call initialize to be able to get new events.
  def stop = {
    continue = true;
    parserThread.interrupt();
    parserThread = null;
  }

  var parserThread: Thread = null

  class Parser extends MarkupHandler with MarkupParser with ExternalSources with Runnable {

    val preserveWS = true
    val input = XMLEventReader.this.getSource
	// document must contain one element - avoid spurious syntax error
    final val ignore_node = <ignore/>

    override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
      setEvent(EvElemStart(pre, label, attrs, scope)); doNotify
    }

    override def elemEnd(pos: Int, pre: String, label: String) {
      setEvent(EvElemEnd(pre, label)); doNotify
    }

    final def elem(pos: Int, pre: String, label: String, attrs: MetaData, pscope: NamespaceBinding, nodes: NodeSeq): NodeSeq =
      ignore_node

    def procInstr(pos: Int, target: String, txt: String): NodeSeq = {
      setEvent(EvProcInstr(target, txt)); doNotify
    }

    def comment(pos: Int, txt: String): NodeSeq = {
      setEvent(EvComment(txt)); doNotify
    }

    def entityRef(pos: Int, n: String): NodeSeq = {
      setEvent(EvEntityRef(n)); doNotify
    }

    def text(pos: Int, txt:String): NodeSeq = {
      setEvent(EvText(txt)); doNotify
    }

    override def run() {
      curInput = input
      this.nextch
      doNotify()
      this.document()
    }
  }
}
