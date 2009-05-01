// $Id$

package scala.xml

import parsing.XhtmlEntities

/* (c) David Pollak  2007 WorldWide Conferencing, LLC */

object Xhtml
{
  /**
   * Convenience function: same as toXhtml(node, false, false)
   *
   * @param node      the node
   */
  def toXhtml(node: Node): String = toXhtml(node, false, false)

  /**
   * Convenience function: amounts to calling toXhtml(node) on each
   * node in the sequence.
   *
   * @param nodeSeq   the node sequence
   */
  def toXhtml(nodeSeq: NodeSeq): String = {
    val sb = new StringBuilder
    sequenceToXML(nodeSeq, TopScope, sb, false, false)
    sb.toString
  }

  /**
   * Convenience function: amounts to calling toXhtml(node, TopScope, ...)
   * with the supplied parameters.
   *
   * @param nodeSeq   the node sequence
   */
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
   * @param convertAmp   if true, decode entity references
   */
  def toXhtml(
    x: Node,
    pscope: NamespaceBinding,
    sb: StringBuilder,
    stripComment: Boolean,
    convertAmp: Boolean): Unit =
  {
    def decode(er: EntityRef) = XhtmlEntities.entMap.get(er.entityName) match {
      case Some(chr) if chr.toInt >= 128  => sb.append(chr)
      case _                              => er.buildString(sb)
    }
    def shortForm =
      (x.child == null || x.child.length == 0) &&
      !(List("div", "script", "textarea") contains x.label)

    x match {
      case c: Comment if !stripComment    => c.buildString(sb)
      case er: EntityRef if convertAmp    => decode(er)
      case x: SpecialNode                 => x.buildString(sb)
      case g: Group                       =>
        g.nodes foreach { toXhtml(_, x.scope, sb, stripComment, convertAmp) }

      case _  =>
        sb.append('<')
        x.nameToString(sb)
        if (x.attributes ne null) x.attributes.buildString(sb)
        x.scope.buildString(sb, pscope)

        if (shortForm) sb.append(" />")
        else {
          sb.append('>')
          sequenceToXML(x.child, x.scope, sb, stripComment, convertAmp)
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
    pscope: NamespaceBinding,
    sb: StringBuilder,
    stripComment: Boolean,
    convertAmp: Boolean): Unit =
  {
    def isAtomAndNotText(x: Node) = x.isInstanceOf[Atom[_]] && !x.isInstanceOf[Text]
    val doSpaces = children forall isAtomAndNotText // interleave spaces

    for (c <- children.take(children.length - 1)) {
      toXhtml(c, pscope, sb, stripComment, convertAmp)
      if (doSpaces) sb append ' '
    }
    toXhtml(children.last, pscope, sb, stripComment, convertAmp)
  }
}

