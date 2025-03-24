/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.doc.html

import java.io.Writer
import javax.xml.stream.XMLStreamWriter

import scala.language.implicitConversions

// a lightweight replacement for scala-xml
object HtmlTags {
  def textOf(elems: Elems) = elems.map(_.toText).foldLeft("")(_ + _)

  trait Elem extends Product {
    def toXhtml(xsw: XMLStreamWriter, raw: Writer): Unit = {
      if (elems.isEmpty && attribs.isEmpty) xsw.writeEmptyElement(tagName)
      else {
        xsw.writeStartElement(tagName)
        attribs.foreach { case (n, v) => xsw.writeAttribute(n, v) }
        elems.foreach(_.toXhtml(xsw, raw))
        xsw.writeEndElement()
      }
    }

    def toText: String = elems.map(_.toText).foldLeft("")(_ + _)

    def elems: Elems

    def tagName = productPrefix.toLowerCase
    def attribNames = productElementNames.toList
    def attribValues = productIterator.toList

    def attribs: Seq[(String, String)] = {
      attribNames.lazyZip(attribValues).collect { case (k, v) if k != "elems" && v != null => (k, v.toString)}.toSeq
    }
  }

  implicit def liftElems(el: Elem): Elems = el :: NoElems

  case class Raw(txt: String) extends Elem {
    def elems = Nil

    override def toText = txt.trim

    // TODO: can we do better?
    override def toXhtml(xsw: XMLStreamWriter, raw: Writer): Unit = {
      xsw.writeCharacters("")
      xsw.flush()
      raw.write(txt)
      raw.flush()
    }
  }

  val NoElems: Elems = Nil
  type Elems = List[Elem]

  trait WithoutElems extends Elem {
    def elems = Nil
    override def toXhtml(xsw: XMLStreamWriter, raw: Writer): Unit = {
      xsw.writeEmptyElement(tagName)
      attribs.foreach { case (n, v) => xsw.writeAttribute(n, v) }
    }
  }

  case class Html(elems: Elems) extends Elem

  case class Body(elems: Elems, `class`: String = null) extends Elem
  case class Head(elems: Elems) extends Elem
  case class Link(href: String, media: String = null, `type`: String = null, rel: String = null, id: String = null) extends Elem with WithoutElems
  case class Script(`type`: String = null, src: String = null, id: String = null, elems: Elems = NoElems) extends Elem // TODO: write script body as raw
  case class Meta(`http-equiv`: String = null, content: String = null, name: String = null) extends Elem with WithoutElems

  case class Title(elems: Elems = Nil) extends Elem
  case class Txt(txt: String) extends Elem {
    def elems = Nil

    override def toText = txt.trim

    override def toXhtml(xsw: XMLStreamWriter, raw: Writer): Unit = {
      xsw.writeCharacters(txt)
    }
  }
  case class DocType(tp: String, elems: Elems = Nil)  extends Elem
  case class H(i: Int, elems: Elems, id: String = null, `class`: String = null) extends Elem {
    override def attribNames = super.attribNames.tail
    override def attribValues = super.attribValues.tail
    override def tagName = s"h$i"
  }
  case class A(elems: Elems = NoElems, href: String = null, target: String = null, name: String = null, id: String = null, title: String = null, `class`: String = null) extends Elem
  case class Span(elems: Elems = NoElems, name: String = null, id: String = null, `class`: String = null, title: String = null) extends Elem
  case class Input(autocapitalize: String = null, placeholder: String = null, id: String = null, `type`: String = null, accesskey: String = null) extends Elem with WithoutElems
  case class Div(elems: Elems = NoElems, id: String = null, name: String = null, style: String = null, `class`: String = null) extends Elem
  case class P(elems: Elems, id: String = null, `class`: String = null) extends Elem
  case class Button(elems: Elems, title: String = null, id: String = null, `class`: String = null) extends Elem
  case class B(elems: Elems) extends Elem
  case class I(elems: Elems, id: String = null, `class`: String = null) extends Elem
  case class U(elems: Elems) extends Elem
  case class Sub(elems: Elems) extends Elem
  case class Sup(elems: Elems) extends Elem
  case class Pre(elems: Elems) extends Elem
  case class Ul(elems: Elems) extends Elem
  case class Ol(elems: Elems, id: String = null, `class`: String = null) extends Elem
  case class Li(elems: Elems, `class`: String = null, name: String = null, `data-hidden`: String = null, group: String = null, fullComment: String = null, `data-isabs`: String = null, visbl: String = null) extends Elem
  case class Dl(elems: Elems, `class`: String = null) extends Elem
  case class Dt(elems: Elems, `class`: String = null) extends Elem
  case class Dd(elems: Elems, `class`: String = null) extends Elem
  case class Code(elems: Elems) extends Elem
  case class Svg(elems: Elems = Nil, id: String = null, `class`: String = null, width: String, height: String) extends Elem
  case object Hr extends Elem { def elems = Nil; override def tagName = "hr" }
  case object Br extends Elem { def elems = Nil; override def tagName = "br" }

  case class Table(thead: THead = null, tbody: TBody = null, `class`: String = null) extends Elem { def elems = (thead :: tbody :: Nil).filterNot(_ == null)}
  case class THead(elems: List[Tr]) extends Elem
  case class TBody(elems: List[Tr]) extends Elem
  case class Tr(elems: Elems = NoElems) extends Elem
  case class Th(elems: Elems, `class`: String = null) extends Elem
  case class Td(elems: Elems, `class`: String = null) extends Elem
}
