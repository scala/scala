package scala.tools.nsc.doc
package model
package diagram

import model._
import comment.CommentFactory
import collection.mutable

// statistics
import  html.page.diagram.DiagramStats

import scala.collection.immutable.SortedMap

/**
 *  This trait takes care of generating the diagram for classes and packages
 *
 *  @author Damien Obrist
 *  @author Vlad Ureche
 */
trait DiagramFactory extends DiagramDirectiveParser {
  this: ModelFactory with DiagramFactory with CommentFactory with TreeFactory =>

  import this.global.definitions._
  import this.global._

  // the following can used for hardcoding different relations into the diagram, for bootstrapping purposes
  lazy val AnyNode = normalNode(AnyClass)
  lazy val AnyRefNode = normalNode(AnyRefClass)
  lazy val AnyValNode = normalNode(AnyValClass)
  lazy val NullNode = normalNode(NullClass)
  lazy val NothingNode = normalNode(NothingClass)
  def normalNode(sym: Symbol) =
    NormalNode(makeTemplate(sym).ownType, Some(makeTemplate(sym)))
  def aggregationNode(text: String) =
    NormalNode(new TypeEntity { val name = text; val refEntity = SortedMap[Int, (TemplateEntity, Int)]() }, None)

  /** Create the inheritance diagram for this template */
  def makeInheritanceDiagram(tpl: DocTemplateImpl): Option[Diagram] = {

    tFilter = 0
    tModel = -System.currentTimeMillis

    // the diagram filter
    val diagramFilter = makeInheritanceDiagramFilter(tpl)

    def implicitTooltip(from: DocTemplateEntity, to: TemplateEntity, conv: ImplicitConversion) =
      Some(from.qualifiedName + " can be implicitly converted to " + conv.targetType + " by the implicit method "
        + conv.conversionShortName + " in " + conv.convertorOwner.kind + " " + conv.convertorOwner.qualifiedName)

    val result =
      if (diagramFilter == NoDiagramAtAll)
        None
      else {
        // the main node
        val thisNode = ThisNode(tpl.ownType, Some(tpl), Some(tpl.qualifiedName + " (this " + tpl.kind + ")"))

        // superclasses
        var superclasses: List[Node] =
          tpl.parentTypes.collect {
            case p: (TemplateEntity, TypeEntity) if !classExcluded(p._1) => NormalNode(p._2, Some(p._1))
          }.reverse

        // incoming implcit conversions
        lazy val incomingImplicitNodes = tpl.incomingImplicitlyConvertedClasses.map {
          case (incomingTpl, conv) =>
            ImplicitNode(incomingTpl.ownType, Some(incomingTpl), implicitTooltip(from=incomingTpl, to=tpl, conv=conv))
        }

        // subclasses
        var subclasses: List[Node] =
          tpl.directSubClasses.flatMap {
            case d: TemplateEntity if !classExcluded(d) => List(NormalNode(d.ownType, Some(d)))
            case _ => Nil
          }.sortBy(_.tpl.get.name)(implicitly[Ordering[String]].reverse)

        // outgoing implicit coversions
        lazy val outgoingImplicitNodes = tpl.outgoingImplicitlyConvertedClasses.map {
          case (outgoingTpl, outgoingType, conv) =>
            ImplicitNode(outgoingType, Some(outgoingTpl), implicitTooltip(from=tpl, to=tpl, conv=conv))
        }

        // TODO: Everyone should be able to use the @{inherit,content}Diagram annotation to change the diagrams.
        // Currently, it's possible to leave nodes and edges out, but there's no way to create new nodes and edges
        // The implementation would need to add the annotations and the logic to select nodes (or create new ones)
        // and add edges to the diagram -- I bet it wouldn't take too long for someone to do it (one or two days
        // at most) and it would be a great add to the diagrams.
        if (tpl.sym == AnyRefClass)
          subclasses = List(aggregationNode("All user-defined classes and traits"))

        val filteredSuperclasses = if (diagramFilter.hideSuperclasses) Nil else superclasses
        val filteredIncomingImplicits = if (diagramFilter.hideIncomingImplicits) Nil else incomingImplicitNodes
        val filteredSubclasses = if (diagramFilter.hideSubclasses) Nil else subclasses
        val filteredImplicitOutgoingNodes = if (diagramFilter.hideOutgoingImplicits) Nil else outgoingImplicitNodes

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
        var mapNodes = Map[TemplateEntity, Node]()
        var nodesShown = Set[TemplateEntity]()
        var edgesAll = List[(TemplateEntity, List[TemplateEntity])]()

        // classes is the entire set of classes and traits in the package, they are the superset of nodes in the diagram
        // we collect classes, traits and objects without a companion, which are usually used as values(e.g. scala.None)
        val nodesAll = pack.members collect {
          case d: TemplateEntity if ((!diagramFilter.hideInheritedNodes) || (d.inTemplate == pack)) => d
        }

        // for each node, add its subclasses
        for (node <- nodesAll if !classExcluded(node)) {
          node match {
            case dnode: DocTemplateImpl =>
              var superClasses = dnode.parentTypes.map(_._1)

              superClasses = superClasses.filter(nodesAll.contains(_))

              // TODO: Everyone should be able to use the @{inherit,content}Diagram annotation to change the diagrams.
              if (pack.sym == ScalaPackage)
                if (dnode.sym == NullClass)
                  superClasses = List(makeTemplate(AnyRefClass))
                else if (dnode.sym == NothingClass)
                  superClasses = (List(NullClass) ::: ScalaValueClasses).map(makeTemplate(_))

              if (!superClasses.isEmpty) {
                nodesShown += dnode
                nodesShown ++= superClasses
              }
              edgesAll ::= dnode -> superClasses
            case _ =>
          }

          mapNodes += node -> (if (node.inTemplate == pack) NormalNode(node.ownType, Some(node)) else OutsideNode(node.ownType, Some(node)))
        }

        if (nodesShown.isEmpty)
          None
        else {
          val nodes = nodesAll.filter(nodesShown.contains(_)).map(mapNodes(_))
          val edges = edgesAll.map(pair => (mapNodes(pair._1), pair._2.map(mapNodes(_)))).filterNot(pair => pair._2.isEmpty)
          val diagram =
            // TODO: Everyone should be able to use the @{inherit,content}Diagram annotation to change the diagrams.
            if (pack.sym == ScalaPackage) {
              // Tried it, but it doesn't look good:
              // var anyRefSubtypes: List[Node] = List(mapNodes(makeTemplate(AnyRefClass)))
              // var dirty = true
              // do {
              //   val length = anyRefSubtypes.length
              //   anyRefSubtypes :::= edges.collect { case p: (Node, List[Node]) if p._2.exists(anyRefSubtypes.contains(_)) => p._1 }
              //   anyRefSubtypes = anyRefSubtypes.distinct
              //   dirty = (anyRefSubtypes.length != length)
              // } while (dirty)
              // println(anyRefSubtypes)
              val anyRefSubtypes = Nil
              val allAnyRefTypes = aggregationNode("All AnyRef subtypes")
              val nullTemplate = makeTemplate(NullClass)
              PackageDiagram(allAnyRefTypes::nodes, (mapNodes(nullTemplate), allAnyRefTypes::anyRefSubtypes)::edges.filterNot(_._1.tpl == Some(nullTemplate)))
            } else
              PackageDiagram(nodes, edges)

          filterDiagram(diagram, diagramFilter)
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
          case ClassDiagram(thisNode, _, _, _, _) if diagramFilter.hideNode(thisNode) =>
            None

          case ClassDiagram(thisNode, superClasses, subClasses, incomingImplicits, outgoingImplicits) =>

            def hideIncoming(node: Node): Boolean =
              diagramFilter.hideNode(node) || diagramFilter.hideEdge(node, thisNode)

            def hideOutgoing(node: Node): Boolean =
              diagramFilter.hideNode(node) || diagramFilter.hideEdge(thisNode, node)

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
                case (source, dests) if !diagramFilter.hideNode(source) =>
                  val dests2 = dests.collect({ case dest if (!(diagramFilter.hideEdge(source, dest) || diagramFilter.hideNode(dest))) => dest })
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
