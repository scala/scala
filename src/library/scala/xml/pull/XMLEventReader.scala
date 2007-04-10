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
    while(continue) {
      wait()
    }
    continue = true
    notifyAll
  }
  def getAndClearEvent: XMLEvent = synchronized {
    while(xmlEvent eq null) {
      wait()
    }
    val r = xmlEvent
    xmlEvent = null
    r
  }
  def setEvent(e: XMLEvent) {
    xmlEvent = e
  }

  def doNotify() = synchronized {
    XMLEventReader.this.continue = false
    notifyAll()
    while (!XMLEventReader.this.continue) wait();
    NodeSeq.Empty
  }

  // iterator methods

  def next: XMLEvent = {
    myresume
    val r = getAndClearEvent
    r
  }

  def hasNext = true

  var parserThread: Thread = null

  class Parser extends MarkupHandler with MarkupParser with ExternalSources with Runnable {

    val preserveWS = true
    val input = XMLEventReader.this.getSource

    override def elemStart(pos:int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
      setEvent(ElemStart(pre, label, attrs, scope)); doNotify
    }

    override def elemEnd(pos: int, pre: String, label: String) {
      setEvent(ElemEnd(pre, label)); doNotify
    }

    final def elem(pos: int, pre: String, label: String, attrs: MetaData, pscope: NamespaceBinding, nodes: NodeSeq): NodeSeq =
      NodeSeq.Empty

    def procInstr(pos: Int, target: String, txt: String) {
      setEvent(ElemStart(null, "comm", null, null)); doNotify
    }

    def comment(pos: Int, txt: String) {
      setEvent(ElemStart(null, "comm", null, null)); doNotify
    }

    def entityRef(pos: Int, n: String) {
      setEvent(ElemStart(null, "eref", null, null)); doNotify
    }

    def text(pos: Int, txt:String) {
      setEvent(ElemStart(null, "tex", null, null)); doNotify
    }

    override def run() {
      curInput = input
      this.nextch
      doNotify()
      this.document()
    }
  }
}
