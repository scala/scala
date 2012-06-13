package scala.tools.nsc.doc
package model
package diagram

import model._
import comment.CommentFactory
import collection.mutable

// statistics
import  html.page.diagram.DiagramStats

/**
 *  This trait takes care of generating the diagram for classes and packages
 *
 *  @author Damien Obrist
 *  @author Vlad Ureche
 */
trait DiagramFactory extends DiagramDirectiveParser {
  this: ModelFactory with DiagramFactory with CommentFactory with TreeFactory =>

  /** Create the inheritance diagram for this template */
  def makeInheritanceDiagram(tpl: DocTemplateImpl): Option[Diagram] = {

    tFilter = 0
    tModel = -System.currentTimeMillis

    // the diagram filter
    val diagramFilter = makeInheritanceDiagramFilter(tpl)

    val result =
      if (diagramFilter == NoDiagramAtAll)
        None
      else {
        // the main node
        val thisNode = ThisNode(tpl.ownType, Some(tpl))

        // superclasses
        var superclasses = List[Node]()
        tpl.parentTypes.collect { case p: (TemplateEntity, TypeEntity) if !classExcluded(p._1) => p } foreach {
          t: (TemplateEntity, TypeEntity) =>
          val n = NormalNode(t._2, Some(t._1))
          superclasses ::= n
        }
        val filteredSuperclasses = if (diagramFilter.hideSuperclasses) Nil else superclasses

        // incoming implcit conversions
        lazy val incomingImplicitNodes = tpl.incomingImplicitlyConvertedClasses.map(tpl => ImplicitNode(tpl.ownType, Some(tpl)))
        val filteredIncomingImplicits = if (diagramFilter.hideIncomingImplicits) Nil else incomingImplicitNodes

        // subclasses
        val subclasses = tpl.directSubClasses.flatMap {
          case d: TemplateEntity if !classExcluded(d) => List(NormalNode(d.ownType, Some(d)))
          case _ => Nil
        }
        val filteredSubclasses = if (diagramFilter.hideSubclasses) Nil else subclasses

        // outgoing implicit coversions
        lazy val implicitNodes = tpl.outgoingImplicitlyConvertedClasses.map(pair => ImplicitNode(pair._2, Some(pair._1)))
        val filteredImplicitOutgoingNodes = if (diagramFilter.hideOutgoingImplicits) Nil else implicitNodes

        // final diagram filter
        filterDiagram(ClassDiagram(thisNode, filteredSuperclasses.reverse, filteredSubclasses.reverse, filteredIncomingImplicits, filteredImplicitOutgoingNodes), diagramFilter)
      }

    tModel += System.currentTimeMillis
    DiagramStats.addFilterTime(tFilter)
    DiagramStats.addModelTime(tModel-tFilter)

    result
  }

  /** Create the content diagram for this template */
  def makeContentDiagram(pack: DocTemplateImpl): Option[Diagram] = {

    tFilter = 0
    tModel = -System.currentTimeMillis

    // the diagram filter
    val diagramFilter = makeContentDiagramFilter(pack)

    val result =
      if (diagramFilter == NoDiagramAtAll)
        None
      else {
        var mapNodes = Map[DocTemplateEntity, Node]()
        var nodesShown = Set[DocTemplateEntity]()
        var edgesAll = List[(DocTemplateEntity, List[DocTemplateEntity])]()

        // classes is the entire set of classes and traits in the package, they are the superset of nodes in the diagram
        // we collect classes, traits and objects without a companion, which are usually used as values(e.g. scala.None)
        val dnodes = pack.members collect {
          case d: DocTemplateEntity if d.isClass || d.isTrait || (d.isObject && !d.companion.isDefined) &&
            ((d.inTemplate == pack) || diagramFilter.showInheritedNodes) => d
        }

        // for each node, add its subclasses
        for (node <- dnodes if !classExcluded(node)) {
          val superClasses = node.parentTypes.collect {
            case (tpl: DocTemplateEntity, tpe) if tpl.inTemplate == pack && !classExcluded(tpl) => tpl
            case (tpl: DocTemplateEntity, tpe) if tpl.inTemplate != pack && !classExcluded(tpl) && diagramFilter.showInheritedNodes && (pack.members contains tpl) => tpl
          }

          if (!superClasses.isEmpty) {
            nodesShown += node
            nodesShown ++= superClasses
          }

          edgesAll ::= node -> superClasses
          mapNodes += node -> (if (node.inTemplate == pack) NormalNode(node.ownType, Some(node)) else OutsideNode(node.ownType, Some(node)))
        }

        if (nodesShown.isEmpty)
          None
        else {
          val nodes = dnodes.filter(nodesShown.contains(_)).map(mapNodes(_))
          val edges = edgesAll.map(pair => (mapNodes(pair._1), pair._2.map(mapNodes(_)))).filterNot(pair => pair._2.isEmpty)
          filterDiagram(PackageDiagram(nodes, edges), diagramFilter)
        }
      }

    tModel += System.currentTimeMillis
    DiagramStats.addFilterTime(tFilter)
    DiagramStats.addModelTime(tModel-tFilter)

    result
  }

  /** Diagram filtering logic */
  private def filterDiagram(diagram: Diagram, diagramFilter: DiagramFilter): Option[Diagram] = {
    tFilter -= System.currentTimeMillis

    val result =
      if (diagramFilter == FullDiagram)
        Some(diagram)
      else if (diagramFilter == NoDiagramAtAll)
        None
      else {
        // Final diagram, with the filtered nodes and edges
        diagram match {
          case ClassDiagram(thisNode, _, _, _, _) if diagramFilter.hideNode(thisNode.tpl.get) =>
            None

          case ClassDiagram(thisNode, superClasses, subClasses, incomingImplicits, outgoingImplicits) =>

            def hideIncoming(node: Node): Boolean =
              if (node.tpl.isDefined) diagramFilter.hideNode(node.tpl.get) || diagramFilter.hideEdge(node.tpl.get, thisNode.tpl.get)
              else false // hopefully we won't need to fallback here

            def hideOutgoing(node: Node): Boolean =
              if (node.tpl.isDefined) diagramFilter.hideNode(node.tpl.get) || diagramFilter.hideEdge(thisNode.tpl.get, node.tpl.get)
              else false // hopefully we won't need to fallback here

            // println(thisNode)
            // println(superClasses.map(cl => "super: " + cl + "  " + hideOutgoing(cl)).mkString("\n"))
            // println(subClasses.map(cl => "sub: " + cl + "  " + hideIncoming(cl)).mkString("\n"))
            Some(ClassDiagram(thisNode,
                             superClasses.filterNot(hideOutgoing(_)),
                             subClasses.filterNot(hideIncoming(_)),
                             incomingImplicits.filterNot(hideIncoming(_)),
                             outgoingImplicits.filterNot(hideOutgoing(_))))

          case PackageDiagram(nodes0, edges0) =>
            // Filter out all edges that:
            // (1) are sources of hidden classes
            // (2) are manually hidden by the user
            // (3) are destinations of hidden classes
            val edges: List[(Node, List[Node])] =
              diagram.edges.flatMap({
                case (source@Node(_, Some(tpl1)), dests) if !diagramFilter.hideNode(tpl1) =>
                  val dests2 = dests.collect({ case node@Node(_, Some(tpl2)) if (!(diagramFilter.hideEdge(tpl1, tpl2) || diagramFilter.hideNode(tpl2))) => node })
                  if (dests2 != Nil)
                    List((source, dests2))
                  else
                    Nil
                case _ => Nil
              })

            // Only show the the non-isolated nodes
            // TODO: Decide if we really want to hide package members, I'm not sure that's a good idea (!!!)
            // TODO: Does .distinct cause any stability issues?
            val sourceNodes = edges.map(_._1)
            val sinkNodes = edges.map(_._2).flatten
            val nodes = (sourceNodes ::: sinkNodes).distinct
            Some(PackageDiagram(nodes, edges))
        }
      }

    tFilter += System.currentTimeMillis

    // eliminate all empty diagrams
    if (result.isDefined && result.get.edges.forall(_._2.isEmpty))
      None
    else
      result
  }

}
