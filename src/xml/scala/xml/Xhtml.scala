
package scala
package xml

import parsing.XhtmlEntities
import Utility.{ sbToString, isAtomAndNotText }

/* (c) David Pollak  2007 WorldWide Conferencing, LLC */

object Xhtml
{
  /**
   * Convenience function: same as toXhtml(node, false, false)
   *
   * @param node      the node
   */
  def toXhtml(node: Node): String = sbToString(sb => toXhtml(x = node, sb = sb))

  /**
   * Convenience function: amounts to calling toXhtml(node) on each
   * node in the sequence.
   *
   * @param nodeSeq   the node sequence
   */
  def toXhtml(nodeSeq: NodeSeq): String = sbToString(sb => sequenceToXML(nodeSeq: Seq[Node], sb = sb))

  /** Elements which we believe are safe to minimize if minimizeTags is true.
   *  See http://www.w3.org/TR/xhtml1/guidelines.html#C_3
   */
  private val minimizableElements =
    List("base", "meta", "link", "hr", "br", "param", "img", "area", "input", "col")

  def toXhtml(
    x: Node,
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = false,
    preserveWhitespace: Boolean = false,
    minimizeTags: Boolean = true): Unit =
  {
    def decode(er: EntityRef) = XhtmlEntities.entMap.get(er.entityName) match {
      case Some(chr) if chr.toInt >= 128  => sb.append(chr)
      case _                              => er.buildString(sb)
    }
    def shortForm =
      minimizeTags &&
      (x.child == null || x.child.length == 0) &&
      (minimizableElements contains x.label)

    x match {
      case c: Comment                       => if (!stripComments) c buildString sb
      case er: EntityRef if decodeEntities  => decode(er)
      case x: SpecialNode                   => x buildString sb
      case g: Group                         =>
        g.nodes foreach { toXhtml(_, x.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags) }

      case _  =>
        sb.append('<')
        x.nameToString(sb)
        if (x.attributes ne null) x.attributes.buildString(sb)
        x.scope.buildString(sb, pscope)

        if (shortForm) sb.append(" />")
        else {
          sb.append('>')
          sequenceToXML(x.child, x.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
          sb.append("</")
          x.nameToString(sb)
          sb.append('>')
        }
    }
  }

  /**
   * Amounts to calling toXhtml(node, ...) with the given parameters on each node.
   */
  def sequenceToXML(
    children: Seq[Node],
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = false,
    preserveWhitespace: Boolean = false,
    minimizeTags: Boolean = true): Unit =
  {
    if (children.isEmpty)
      return

    val doSpaces = children forall isAtomAndNotText // interleave spaces
    for (c <- children.take(children.length - 1)) {
      toXhtml(c, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      if (doSpaces) sb append ' '
    }
    toXhtml(children.last, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
  }
}
