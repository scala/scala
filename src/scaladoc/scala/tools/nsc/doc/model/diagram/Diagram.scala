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

package scala.tools.nsc.doc
package model
package diagram

import model._

/**
 *  The diagram base classes
 *
 *  @author Damien Obrist
 *  @author Vlad Ureche
 */
sealed abstract class Diagram {
  def nodes: List[Node]
  def edges: List[(Node, List[Node])]
  def isContentDiagram = false     // Implemented by ContentDiagram
  def isInheritanceDiagram = false // Implemented by InheritanceDiagram
  def depthInfo: DepthInfo
}

case class ContentDiagram(nodes:List[/*Class*/Node], edges:List[(Node, List[Node])]) extends Diagram {
  override def isContentDiagram = true
  lazy val depthInfo = new ContentDiagramDepth(this)
}

/** A class diagram */
case class InheritanceDiagram(thisNode: ThisNode,
                        superClasses: List[/*Class*/Node],
                        subClasses: List[/*Class*/Node],
                        incomingImplicits: List[ImplicitNode],
                        outgoingImplicits: List[ImplicitNode]) extends Diagram {
  def nodes = thisNode :: superClasses ::: subClasses ::: incomingImplicits ::: outgoingImplicits
  def edges = (thisNode -> (superClasses ::: outgoingImplicits)) ::
              (subClasses ::: incomingImplicits).map(_ -> List(thisNode))

  override def isInheritanceDiagram = true
  lazy val depthInfo = new DepthInfo {
    def maxDepth = 3
  }
}

trait DepthInfo {
  /** Gives the maximum depth */
  def maxDepth: Int
}

sealed abstract class Node {
  def name = tpe.name
  def tpe: TypeEntity
  def tpl: Option[TemplateEntity]
  /** shortcut to get a DocTemplateEntity */
  def doctpl: Option[DocTemplateEntity] = tpl match {
    case Some(tpl) => tpl match {
      case d: DocTemplateEntity => Some(d)
      case _ => None
    }
    case _ => None
  }
  /* shortcuts to find the node type without matching */
  def isThisNode = false
  def isNormalNode = false
  def isClassNode = if (tpl.isDefined) (tpl.get.isClass || tpl.get.qualifiedName == "scala.AnyRef") else false
  def isTraitNode = if (tpl.isDefined) tpl.get.isTrait else false
  def isObjectNode= if (tpl.isDefined) tpl.get.isObject else false
  def isTypeNode  = if (doctpl.isDefined) doctpl.get.isAbstractType || doctpl.get.isAliasType else false
  def isOtherNode = !(isClassNode || isTraitNode || isObjectNode || isTypeNode)
  def isImplicitNode = false
  def isOutsideNode = false
  def tooltip: Option[String]
}

// different matchers, allowing you to use the pattern matcher against any node
// NOTE: A ThisNode or ImplicitNode can at the same time be ClassNode/TraitNode/OtherNode, not exactly according to
// case class specification -- thus a complete match would be:
//   node match {
//     case ThisNode(tpe, _) =>     /* case for this node, you can still use .isClass, .isTrait and .isOther */
//     case ImplicitNode(tpe, _) => /* case for an implicit node, you can still use .isClass, .isTrait and .isOther */
//     case _ => node match {
//       case ClassNode(tpe, _) =>  /* case for a non-this, non-implicit Class node */
//       case TraitNode(tpe, _) =>  /* case for a non-this, non-implicit Trait node */
//       case OtherNode(tpe, _) =>  /* case for a non-this, non-implicit Other node */
//     }
//   }
object Node        { def unapply(n: Node): Option[(TypeEntity, Option[TemplateEntity])] = Some((n.tpe, n.tpl)) }
object ClassNode   { def unapply(n: Node): Option[(TypeEntity, Option[TemplateEntity])] = if (n.isClassNode)   Some((n.tpe, n.tpl)) else None }
object TraitNode   { def unapply(n: Node): Option[(TypeEntity, Option[TemplateEntity])] = if (n.isTraitNode)   Some((n.tpe, n.tpl)) else None }
object TypeNode    { def unapply(n: Node): Option[(TypeEntity, Option[TemplateEntity])] = if (n.isTypeNode)    Some((n.tpe, n.tpl)) else None }
object ObjectNode  { def unapply(n: Node): Option[(TypeEntity, Option[TemplateEntity])] = if (n.isObjectNode)  Some((n.tpe, n.tpl)) else None }
object OutsideNode { def unapply(n: Node): Option[(TypeEntity, Option[TemplateEntity])] = if (n.isOutsideNode) Some((n.tpe, n.tpl)) else None }
object OtherNode   { def unapply(n: Node): Option[(TypeEntity, Option[TemplateEntity])] = if (n.isOtherNode)   Some((n.tpe, n.tpl)) else None }



/** The node for the current class */
case class ThisNode(tpe: TypeEntity, tpl: Option[TemplateEntity])(val tooltip: Option[String] = None) extends Node { override def isThisNode = true }

/** The usual node */
case class NormalNode(tpe: TypeEntity, tpl: Option[TemplateEntity])(val tooltip: Option[String] = None) extends Node { override def isNormalNode = true }

/** A class or trait the thisnode can be converted to by an implicit conversion
 *  TODO: I think it makes more sense to use the tpe links to templates instead of the TemplateEntity for implicit nodes
 *  since some implicit conversions convert the class to complex types that cannot be represented as a single template
 */
case class ImplicitNode(tpe: TypeEntity, tpl: Option[TemplateEntity])(val tooltip: Option[String] = None) extends Node { override def isImplicitNode = true }

/** An outside node is shown in packages when a class from a different package makes it to the package diagram due to
 * its relation to a class in the template (see @contentDiagram hideInheritedNodes annotation) */
case class OutsideNode(tpe: TypeEntity, tpl: Option[TemplateEntity])(val tooltip: Option[String] = None) extends Node { override def isOutsideNode = true }


// Computing and offering node depth information
class ContentDiagramDepth(pack: ContentDiagram) extends DepthInfo {
  private[this] var _maxDepth = 0
  private[this] var _nodeDepth = Map[Node, Int]()
  private[this] var seedNodes = Set[Node]()
  private[this] val invertedEdges: Map[Node, List[Node]] =
    pack.edges.flatMap({case (node: Node, outgoing: List[Node]) => outgoing.map((_, node))}).groupBy(_._1).map({case (k, values) => (k, values.map(_._2))}).withDefaultValue(Nil)
  private[this] val directEdges: Map[Node, List[Node]] = pack.edges.toMap.withDefaultValue(Nil)

  // seed base nodes, to minimize noise - they can't all have parents, else there would only be cycles
  seedNodes ++= pack.nodes.filter(directEdges(_).isEmpty)

  while (seedNodes.nonEmpty) {
    var newSeedNodes = Set[Node]()
    for (node <- seedNodes) {
      val depth = 1 + (-1 :: directEdges(node).map(_nodeDepth.getOrElse(_, -1))).max
      if (depth != _nodeDepth.getOrElse(node, -1)) {
        _nodeDepth += (node -> depth)
        newSeedNodes ++= invertedEdges(node)
        if (depth > _maxDepth) _maxDepth = depth
      }
    }
    seedNodes = newSeedNodes
  }

  val maxDepth = _maxDepth
}
