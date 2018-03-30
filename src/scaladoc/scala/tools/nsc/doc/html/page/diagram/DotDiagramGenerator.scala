/**
 * @author Damien Obrist
 * @author Vlad Ureche
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

class DotDiagramGenerator(settings: doc.Settings, dotRunner: DotRunner) extends DiagramGenerator {

  // the page where the diagram will be embedded
  private var page: HtmlPage = null
  // path to the "lib" folder relative to the page
  private var pathToLib: String = null
  // maps nodes to unique indices
  private var node2Index: Map[Node, Int] = null
  // true if the current diagram is a class diagram
  private var isInheritanceDiagram = false
  // incoming implicit nodes (needed for determining the CSS class of a node)
  private var incomingImplicitNodes: List[Node] = List()
  // the suffix used when there are two many classes to show
  private final val MultiSuffix = " classes/traits"
  // used to generate unique node and edge ids (i.e. avoid conflicts with multiple diagrams)
  private var counter = 0

  def getKind(klass: String): String =
    if (klass.contains("class")) "class"
    else if (klass.contains("trait")) "trait"
    else if (klass.contains("object")) "object"
    else ""

  /* graph / node / edge attributes */

  private val graphAttributes: Map[String, String] = Map(
      "compound" -> "true",
      "rankdir" -> "TB"
  )

  private val nodeAttributes = Map(
    "shape" -> "rect",
    "style" -> "filled,rounded",
    "penwidth" -> "1",
    "margin" -> "0.08,0.01",
    "width" -> "0.0",
    "height" -> "0.0",
    "fontname" -> "Source Code Pro",
    "fontsize" -> "8.00"
  )

  private val edgeAttributes = Map(
    "color" -> "#d4d4d4",
    "arrowsize" -> "0.7",
    "fontcolor" -> "#aaaaaa",
    "fontsize" -> "9.00",
    "fontname" -> "Source Code Pro"
  )

  private val defaultStyle = Map(
    "color" -> "#ababab",
    "fillcolor" -> "#e1e1e1",
    "fontcolor" -> "#7d7d7d",
    "margin" -> "0.1,0.04"
  )

  private val implicitStyle = Map(
    "color" -> "#ababab",
    "fillcolor" -> "#e1e1e1",
    "fontcolor" -> "#7d7d7d"
  )

  private val outsideStyle = Map(
    "color" -> "#ababab",
    "fillcolor" -> "#e1e1e1",
    "fontcolor" -> "#7d7d7d"
  )

  private val traitStyle = Map(
    "color" -> "#2E6D82",
    "fillcolor" -> "#2E6D82",
    "fontcolor" -> "#ffffff"
  )

  private val classStyle = Map(
    "color" -> "#418565",
    "fillcolor" -> "#418565",
    "fontcolor" -> "#ffffff"
  )

  private val objectStyle = Map(
    "color" -> "#103A51",
    "fillcolor" -> "#103A51",
    "fontcolor" -> "#ffffff"
  )

  private val typeStyle = Map(
    "color" -> "#2E6D82",
    "fillcolor" -> "#2E6D82",
    "fontcolor" -> "#ffffff"
  )

  private def flatten(attributes: Map[String, String]) = attributes.map{ case (key, value) => key + "=\"" + value + "\"" }.mkString(", ")

  private val graphAttributesStr = graphAttributes.map{ case (key, value) => key + "=\"" + value + "\";\n" }.mkString
  private val nodeAttributesStr = flatten(nodeAttributes)
  private val edgeAttributesStr = flatten(edgeAttributes)
}
