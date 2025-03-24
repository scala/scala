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

package scala
package tools
package nsc
package doc
package html
package page
package diagram

import scala.collection.immutable._
import model._
import model.diagram._
import scala.tools.nsc.doc.html.HtmlTags._

// this needs a rewrite
class DotDiagramGenerator(settings: doc.Settings) extends DiagramGenerator {
  // the suffix used when there are two many classes to show
  private final val MultiSuffix = " classes/traits"

  // used to generate unique node and edge ids (i.e. avoid conflicts with multiple diagrams)
  private var graphId = 0

  /**
    * TODO: check this was ported from old code
    * - adds a class attribute to the SVG element
    * - changes the path of the node images from absolute to relative
    * - assigns id and class attributes to nodes and edges
    * - removes title elements
    */
  def generate(diagram: Diagram, template: DocTemplateEntity, page: HtmlPage): HtmlTags.Elems = {
    graphId = graphId + 1

    // TODO: reconcile show/hide via display CSS attrib and rendering into the SVG (which only works when the element is visible so that its dimensions are known) -- see template.js for trigger('beforeShow')
    List(Svg(id=s"graph$graphId", width= "800", height="600", `class` = if (diagram.isInstanceOf[InheritanceDiagram]) "class-diagram" else "package-diagram"),
         Script(`type`="text/dot", id=s"dot$graphId", elems=Raw("\n" + generateDot(diagram)(page) + "\n")),
         Script(elems= Txt(
      s"""$$("#inheritance-diagram").bind('beforeShow', function() {
         |  if ($$("svg#graph$graphId").children().length == 0) {
         |    var dot = document.querySelector("#dot$graphId").text;
         |    var svg = d3.select("#graph$graphId");
         |    var inner = svg.append("g");
         |
         |    // Set up zoom support
         |    var zoom = d3.zoom()
         |        .on("zoom", function() {
         |          inner.attr("transform", d3.event.transform);
         |        });
         |    svg.call(zoom);
         |
         |    var render = new dagreD3.render();
         |    var g = graphlibDot.read(dot);
         |    render(inner, g);
         |
         |    inner.selectAll("g.node").each(function(v) {
         |      // https://stackoverflow.com/questions/27381452/wrapping-existing-inline-svg-g-element-with-a-element
         |      var tgt = $$("#"+ g.node(v).id +" g.label text")[0];
         |      var parent = tgt.parentNode;
         |      var a = document.createElementNS('http://www.w3.org/2000/svg', 'a');
         |      a.setAttributeNS('http://www.w3.org/1999/xlink', 'xlink:href', g.node(v).URL);
         |      a.appendChild(tgt);
         |      parent.appendChild(a);
         |    });
         |  }
         |})""".stripMargin)))
  }

  /**
   * Generates a dot string for a given diagram.
   */
  private def generateDot(d: Diagram, preamble: String = "")(implicit page: HtmlPage): String = {
    var tDot = -System.currentTimeMillis

    // yuck
    val (
      thisNode,
      nodes,  // inheritance nodes (all nodes except thisNode and implicit nodes)
      edges, // inheritance edges (all edges except implicit edges) : List[(Node, List[Node])] = null
      node2Index, // maps nodes to unique indices : Map[Node, Int] = null
      superClasses,
      subClasses,
      incomingImplicits, // incoming implicit nodes (needed for determining the CSS class of a node)
      outgoingImplicits)
       = d match {
      case InheritanceDiagram(thisNode, _superClasses, _subClasses, _incomingImplicits, _outgoingImplicits) =>
        // it seems dot chokes on node names over 8000 chars, so let's limit the size of the string
        // conservatively, we'll limit at 4000, to be sure:
        def limitSize(str: String) = if (str.length > 4000) str.substring(0, 3996) + " ..." else str

        def tooltip(nodes: List[Node]) = limitSize(nodes.map(_.tpe.name).mkString(", "))

        def counted(nodes: List[Node]) = new TypeEntity {
          val name = "" + nodes.length + MultiSuffix;
          def refEntity: SortedMap[Int, (base.LinkTo, Int)] = SortedMap()
        }
        // avoid overcrowding the diagram:
        //   if there are too many super / sub / implicit nodes, represent
        //   them by on node with a corresponding tooltip

        val outgoingImplicits =
          if (_outgoingImplicits.lengthIs > settings.docDiagramsMaxImplicitClasses.value) {
            List(ImplicitNode(counted(_outgoingImplicits), None)(Some(tooltip(_outgoingImplicits))))
          } else _outgoingImplicits

        val incomingImplicits =
          if (_incomingImplicits.lengthIs > settings.docDiagramsMaxImplicitClasses.value) {
            List(ImplicitNode(counted(_incomingImplicits), None)(Some(tooltip(_incomingImplicits))))
          } else _incomingImplicits

        val subClasses =
          if (_subClasses.lengthIs > settings.docDiagramsMaxNormalClasses.value) {
            List(NormalNode(counted(_subClasses), None)(Some(tooltip(_subClasses))))
          } else _subClasses

        val superClasses =
          if (_superClasses.lengthIs > settings.docDiagramsMaxNormalClasses.value) {
            List(NormalNode(counted(_superClasses), None)(Some(tooltip(_superClasses))))
          } else _superClasses

        (thisNode,
          Nil,
          (thisNode -> superClasses) :: subClasses.map(_ -> List(thisNode)),
          (thisNode :: subClasses ::: superClasses ::: incomingImplicits ::: outgoingImplicits).zipWithIndex.toMap,
          superClasses,
          subClasses,
          incomingImplicits,
          outgoingImplicits)

      case _ =>
        (null, d.nodes, d.edges, d.nodes.zipWithIndex.toMap, Nil, Nil, Nil, Nil)
    }


    /**
      * Generates the dot string of a given node.
      */
    def node2Dot(node: Node)(implicit page: HtmlPage) = {
      val nodeIdx = node2Index(node)

      val baseClass =
        if (node.isClassNode) " class"
        else if (node.isTraitNode) " trait"
        else if (node.isObjectNode) " object"
        else if (node.isTypeNode) " type"

      val cls =
        if (node.isImplicitNode && incomingImplicits.contains(node)) "implicit-incoming" + baseClass
        else if (node.isImplicitNode) "implicit-outgoing" + baseClass
        else if (node.isThisNode) "this" + baseClass
        else if (node.isOutsideNode) "outside" + baseClass
        else "default"

      val attr =
        Map("label" -> node.name,
            "id" -> s"graph${graphId}_$nodeIdx",
            "class" -> cls) ++
        node.doctpl.toList.map { tpl => "URL" -> (page.relativeLinkTo(tpl) + "#inheritance-diagram-container") } ++
        (node.tooltip orElse node.tpl.map(_.qualifiedName)).toList.map { "tooltip" -> _ }

      s"node$nodeIdx ${nodeAttrString(attr)} ;"
    }

    val implicitsDot =
      if (!d.isInstanceOf[InheritanceDiagram]) ""
      else {
        // dot cluster containing thisNode
        val thisCluster =
          s"""subgraph clusterThis {
             |  style="invis"
             |  ${node2Dot(thisNode)}
             |}""".stripMargin

        // dot cluster containing incoming implicit nodes, if any
        def implicitCluster(impls: List[Node], clusterName: String) =
          if (impls.isEmpty) ""
          else {
            val str =
              if (impls.lengthCompare(1) <= 0) ""
              else impls.map(n => "node" + node2Index(n)).mkString(" -> ") +
                   nodeAttrString(Map("constraint" -> "false", "style" -> "invis", "minlen" -> "0.0")) + ";\n"

            s"""subgraph $clusterName {
               |  style = "invis"
               |  ${impls.reverse.map(n => node2Dot(n)).mkString}
               |  $str
               }""".stripMargin
          }

        // assemble clusters into another cluster
        val incomingTooltip = incomingImplicits.map(_.name).mkString(", ") + " can be implicitly converted to " + thisNode.name
        val outgoingTooltip = thisNode.name + " can be implicitly converted to " + outgoingImplicits.map(_.name).mkString(", ")

        def implicitEdge(from: Node, to: Node, tailLabel: String, headLabel: String, tooltip: String) = {
          "node" + node2Index(from) + " -> node" + node2Index(to) +
          nodeAttrString(Map(
                              "class" -> edgeClass(from, to),
                              "id" -> ("" + node2Index(from) + "_" + node2Index(to)),
                              "tooltip" -> tooltip,
                              "constraint" -> "false",
                              "minlen" -> "2",
                              "ltail" -> tailLabel,
                              "lhead" -> headLabel,
                              "label" -> "implicitly"))
        }

        val incomingImplicitEdges =
          if (incomingImplicits.isEmpty) ""
          else implicitEdge(incomingImplicits.last, thisNode, "clusterIncoming", "clusterThis", incomingTooltip)

        val outgoingImplicitEdges =
          if (outgoingImplicits.isEmpty) ""
          else implicitEdge(thisNode, outgoingImplicits.head, "clusterThis", "clusterOutgoing", outgoingTooltip)

        s"""subgraph clusterAll {
           |  style = "invis"
           |  ${implicitCluster(outgoingImplicits, "clusterOutgoing")}
           |  $thisCluster
           |  ${implicitCluster(incomingImplicits, "clusterIncoming")}
           |  $incomingImplicitEdges
           |  $outgoingImplicitEdges
           |}""".stripMargin
      }

    val edgesStr = edges.map { case (from, tos) =>
      tos.map { to =>
        val id = "graph" + graphId + "_" + node2Index(to) + "_" + node2Index(from)
        // the X -> Y edge is inverted twice to keep the diagram flowing the right way
        // that is, an edge from node X to Y will result in a dot instruction nodeY -> nodeX [dir="back"]
        val attrs = Map("class" -> edgeClass(to, from),
                        "id" -> id,
                        "tooltip" -> s"${from.name} ${if (from.name.endsWith(MultiSuffix)) "are subtypes" else "is a subtype"} of ${to.name}",
                        "dir" -> "back",
                        "arrowtail" -> "empty")

        s"""node${node2Index(to)} -> node${node2Index(from)} ${nodeAttrString(attrs)} ;"""
      }.mkString
    }.mkString("\n  ")


    tDot += System.currentTimeMillis
    DiagramStats.addDotGenerationTime(tDot)

    def nodeStrings(nodes: List[Node]) = nodes.map(node2Dot).mkString("\n  ")

    s"""digraph G {
       |  $preamble
       |  $implicitsDot
       |  ${nodeStrings(nodes)}
       |  ${nodeStrings(subClasses)}
       |  ${nodeStrings(superClasses)}
       |  $edgesStr
       |}""".stripMargin
  }

  private def nodeAttrString(attributes: Map[String, String]) = {
    // escape HTML characters in node names
    def escape(name: String) = name.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    attributes.map{ case (key, value) => s"""$key="${escape(value)}"""" }.mkString("[", ", ", "]")
  }


  /**
   * Returns the CSS class for an edge connecting node1 and node2.
   */
  private def edgeClass(node1: Node, node2: Node): String =
    if (node1.isImplicitNode && node2.isThisNode) "implicit-incoming"
    else if (node1.isThisNode && node2.isImplicitNode) "implicit-outgoing"
    else "inheritance"



  // styles
  //    if(node.isImplicitNode)
  //      attr ++= implicitStyle
  //    else if(node.isOutsideNode)
  //           attr ++= outsideStyle
  //    else if(node.isTraitNode)
  //           attr ++= traitStyle
  //    else if(node.isClassNode)
  //           attr ++= classStyle
  //    else if(node.isObjectNode)
  //           attr ++= objectStyle
  //    else if(node.isTypeNode)
  //           attr ++= typeStyle
  //    else
  //      attr ++= defaultStyle
  //    var img =
  //      if(node.isTraitNode) "trait_diagram.png"
  //      else if(node.isClassNode) "class_diagram.png"
  //      else if(node.isObjectNode) "object_diagram.png"
  //      else if(node.isTypeNode) "type_diagram.png"
  //      else ""
  //
  //    if(!img.equals("")) {
  //      img = "<TD><IMG SCALE=\"TRUE\" SRC=\"" + pathToLib +"/"+ img + "\" /></TD>"
  //      name = name + " "
  //    }
  //    val label = "<<TABLE BORDER=\"0\" CELLBORDER=\"0\">" +
  //    		       "<TR>" + img + "<TD VALIGN=\"MIDDLE\">" + name + "</TD></TR>" +
  //    		    "</TABLE>>"

//
//  def getKind(klass: String): String =
//    if (klass.contains("class")) "class"
//    else if (klass.contains("trait")) "trait"
//    else if (klass.contains("object")) "object"
//    else ""

//
//  /* graph / node / edge attributes */
//
//  private val graphAttributes: Map[String, String] = Map(
//      "compound" -> "true",
//      "rankdir" -> "TB"
//  )
//
//  private val nodeAttributes = Map(
//    "shape" -> "rect",
//    "style" -> "filled,rounded",
//    "penwidth" -> "1",
//    "margin" -> "0.08,0.01",
//    "width" -> "0.0",
//    "height" -> "0.0",
//    "fontname" -> "Source Code Pro",
//    "fontsize" -> "8.00"
//  )
//
//  private val edgeAttributes = Map(
//    "color" -> "#d4d4d4",
//    "arrowsize" -> "0.7",
//    "fontcolor" -> "#aaaaaa",
//    "fontsize" -> "9.00",
//    "fontname" -> "Source Code Pro"
//  )
//
//  private val defaultStyle = Map(
//    "color" -> "#ababab",
//    "fillcolor" -> "#e1e1e1",
//    "fontcolor" -> "#7d7d7d",
//    "margin" -> "0.1,0.04"
//  )
//
//  private val implicitStyle = Map(
//    "color" -> "#ababab",
//    "fillcolor" -> "#e1e1e1",
//    "fontcolor" -> "#7d7d7d"
//  )
//
//  private val outsideStyle = Map(
//    "color" -> "#ababab",
//    "fillcolor" -> "#e1e1e1",
//    "fontcolor" -> "#7d7d7d"
//  )
//
//  private val traitStyle = Map(
//    "color" -> "#2E6D82",
//    "fillcolor" -> "#2E6D82",
//    "fontcolor" -> "#ffffff"
//  )
//
//  private val classStyle = Map(
//    "color" -> "#418565",
//    "fillcolor" -> "#418565",
//    "fontcolor" -> "#ffffff"
//  )
//
//  private val objectStyle = Map(
//    "color" -> "#103A51",
//    "fillcolor" -> "#103A51",
//    "fontcolor" -> "#ffffff"
//  )
//
//  private val typeStyle = Map(
//    "color" -> "#2E6D82",
//    "fillcolor" -> "#2E6D82",
//    "fontcolor" -> "#ffffff"
//  )


//
//  private val graphAttributesStr = graphAttributes.map{ case (key, value) => key + "=\"" + value + "\";\n" }.mkString
//  private val nodeAttributesStr = flatten(nodeAttributes)
//  private val edgeAttributesStr = flatten(edgeAttributes)
}
