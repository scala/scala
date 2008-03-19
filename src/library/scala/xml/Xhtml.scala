// $Id$

package scala.xml

import parsing.XhtmlEntities

/* (c) David Pollak  2007 WorldWide Conferencing, LLC */

object Xhtml {

  def toXhtml(n: Node, stripComment: Boolean, convertAmp: Boolean): String = {
    val sb = new StringBuilder()
    toXhtml(n, TopScope, sb, stripComment, convertAmp)
    sb.toString()
  }

  /**
   * Appends a tree to the given stringbuffer within given namespace scope.
   *
   * @param n            the node
   * @param pscope       the parent scope
   * @param sb           stringbuffer to append to
   * @param stripComment if true, strip comments
   */
  def toXhtml(x: Node, pscope: NamespaceBinding, sb: StringBuilder, stripComment: Boolean, convertAmp: Boolean): Unit = {
    x match {

      case c: Comment if !stripComment =>
        c.toString(sb)

      case er: EntityRef if convertAmp =>
        XhtmlEntities.entMap.get(er.entityName) match {
          case Some(chr) if chr.toInt >= 128 => sb.append(chr)
          case _ => er.toString(sb)
        }

      case x: SpecialNode =>
        x.toString(sb)

      case g: Group =>
        for (c <- g.nodes) toXhtml(c, x.scope, sb, stripComment, convertAmp)

      case _  =>
        if (((x.child eq null) || (x.child.length == 0)) && x.label != "div" && x.label != "script" && x.label != "textarea") {
          sb.append('<')
          x.nameToString(sb)
          if (x.attributes ne null) x.attributes.toString(sb)
          x.scope.toString(sb, pscope)
          sb.append(" />")
        } else {
          // print tag with namespace declarations
          sb.append('<')
          x.nameToString(sb)
          if (x.attributes ne null) x.attributes.toString(sb)
          x.scope.toString(sb, pscope)
          sb.append('>')
          sequenceToXML(x.child, x.scope, sb, stripComment, convertAmp)
          sb.append("</")
          x.nameToString(sb)
          sb.append('>')
        }
    }
  }

  /**
   * @param children     ...
   * @param pscope       ...
   * @param sb           ...
   * @param stripComment ...
   */
  def sequenceToXML(children: Seq[Node], pscope: NamespaceBinding,
                    sb: StringBuilder, stripComment: Boolean, convertAmp: Boolean): Unit = {
    if (children.isEmpty)
      return
    else if (children forall { y => y.isInstanceOf[Atom[_]] && !y.isInstanceOf[Text] }) { // add space
      val it = children.elements
      val f = it.next
      toXhtml(f, pscope, sb, stripComment, convertAmp)
      while (it.hasNext) {
        val x = it.next
        sb.append(' ')
        toXhtml(x, pscope, sb, stripComment, convertAmp)
      }
    } else {
      for (c <- children) toXhtml(c, pscope, sb, stripComment, convertAmp)
    }
  }
}

