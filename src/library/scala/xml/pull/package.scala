package scala.xml

/**
 * Classes needed to view an XML document as a series of events.  The document
 * is parsed by an [[scala.xml.pull.XMLEventReader]] instance.  You can treat it as
 * an [[scala.collection.Iterator]] to retrieve the events, which are all
 * subclasses of [[scala.xml.pull.XMLEvent]].
 *
 * {{{
 * scala> val source = Source.fromString("""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
 * <?instruction custom value="customvalue"?>
 * <!DOCTYPE foo [
 *   <!ENTITY bar "BAR">
 * ]><foo>Hello<!-- this is a comment --><bar>&bar;</bar><bar>&gt;</bar></foo>""")
 *
 * source: scala.io.Source = non-empty iterator
 *
 * scala> val reader = new XMLEventReader(source)
 * reader: scala.xml.pull.XMLEventReader = non-empty iterator
 *
 * scala> reader.foreach{ println(_) }
 * EvProcInstr(instruction,custom value="customvalue")
 * EvText(
 * )
 * EvElemStart(null,foo,,)
 * EvText(Hello)
 * EvComment( this is a comment )
 * EvElemStart(null,bar,,)
 * EvText(BAR)
 * EvElemEnd(null,bar)
 * EvElemStart(null,bar,,)
 * EvEntityRef(gt)
 * EvElemEnd(null,bar)
 * EvElemEnd(null,foo)
 * EvText(
 *
 * )
 *
 * }}}
 */
package object pull
