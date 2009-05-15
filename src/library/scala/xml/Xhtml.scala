// $Id$

package scala.xml

import parsing.XhtmlEntities
import Utility.{ XMLOptions, sbToString, isAtomAndNotText }

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
  def toXhtml(nodeSeq: NodeSeq): String =
    sbToString(sequenceToXML(nodeSeq, TopScope, _, false, false))

  /**
   * Convenience function: amounts to calling toXhtml(node, TopScope, ...)
   * with the supplied parameters.
   *
   * @param nodeSeq   the node sequence
   */
  def toXhtml(n: Node, _stripComments: Boolean, _convertAmp: Boolean): String = {
    sbToString(toXhtml(n, TopScope, _, _stripComments, _convertAmp))
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
  def toXhtml(n: Node, pscope: NamespaceBinding, sb: StringBuilder, _stripComments: Boolean, _convertAmp: Boolean): String = {
    implicit val config = new XMLOptions {
      override val stripComments = _stripComments
      override val decodeEntities = _convertAmp
    }
    sbToString(toXhtml(n, TopScope, _))
  }

  def toXhtml(
    x: Node,
    pscope: NamespaceBinding,
    sb: StringBuilder)(implicit config: XMLOptions): Unit =
  {
    import config._

    def decode(er: EntityRef) = XhtmlEntities.entMap.get(er.entityName) match {
      case Some(chr) if chr.toInt >= 128  => sb.append(chr)
      case _                              => er.buildString(sb)
    }
    def shortForm =
      minimizeTags &&
      (x.child == null || x.child.length == 0) &&
      !(List("div", "script", "textarea") contains x.label)

    x match {
      case c: Comment if !stripComments     => c buildString sb
      case er: EntityRef if decodeEntities  => decode(er)
      case x: SpecialNode                   => x buildString sb
      case g: Group                         =>
        g.nodes foreach { toXhtml(_, x.scope, sb) }

      case _  =>
        sb.append('<')
        x.nameToString(sb)
        if (x.attributes ne null) x.attributes.buildString(sb)
        x.scope.buildString(sb, pscope)

        if (shortForm) sb.append(" />")
        else {
          sb.append('>')
          sequenceToXML(x.child, x.scope, sb)
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
    _stripComments: Boolean,
    _convertAmp: Boolean): Unit =
  {
    implicit val config = new XMLOptions {
      override val stripComments = _stripComments
      override val decodeEntities = _convertAmp
    }
    sequenceToXML(children, pscope, sb)
  }

  def sequenceToXML(
    children: Seq[Node],
    pscope: NamespaceBinding,
    sb: StringBuilder)(implicit config: XMLOptions): Unit =
  {
    val doSpaces = children forall isAtomAndNotText // interleave spaces

    for (c <- children.take(children.length - 1)) {
      toXhtml(c, pscope, sb)
      if (doSpaces) sb append ' '
    }
    toXhtml(children.last, pscope, sb)
  }
}

