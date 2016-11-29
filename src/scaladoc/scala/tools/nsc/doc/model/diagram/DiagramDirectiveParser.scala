package scala.tools.nsc.doc
package model
package diagram

import model._
import java.util.regex.Pattern
import scala.util.matching.Regex

/**
 *  This trait takes care of parsing @{inheritance, content}Diagram annotations
 *
 *  @author Damien Obrist
 *  @author Vlad Ureche
 */
trait DiagramDirectiveParser {
  this: ModelFactory with DiagramFactory with CommentFactory with TreeFactory =>

  import this.global.definitions.AnyRefClass

  ///// DIAGRAM FILTERS //////////////////////////////////////////////////////////////////////////////////////////////

  /**
   *  The DiagramFilter trait directs the diagram engine about the way the diagram should be displayed
   *
   *  Vlad: There's an explanation I owe to people using diagrams and not finding a way to hide a specific class from
   *  all diagrams at once. So why did I choose to allow you to only control the diagrams at class level? So, the
   *  reason is you would break the separate scaladoc compilation:
   *  If you have an "@diagram hideMyClass" annotation in class A and you run scaladoc on it along with its subclass B
   *  A will not appear in B's diagram. But if you scaladoc only on B, A's comment will not be parsed and the
   *  instructions to hide class A from all diagrams will not be available. Thus I prefer to force you to control the
   *  diagrams of each class locally. The problem does not appear with scalac, as scalac stores all its necessary
   *  information (like scala signatures) serialized in the .class file. But we couldn't store doc comments in the class
   *  file, could we? (Turns out we could, but that's another story)
   *
   *  Any flaming for this decision should go to scala-internals@googlegroups.com
   */
  trait DiagramFilter {
    /** A flag to hide the diagram completely */
    def hideDiagram: Boolean
    /** Hide incoming implicit conversions (for type hierarchy diagrams) */
    def hideIncomingImplicits: Boolean
    /** Hide outgoing implicit conversions (for type hierarchy diagrams) */
    def hideOutgoingImplicits: Boolean
    /** Hide superclasses (for type hierarchy diagrams) */
    def hideSuperclasses: Boolean
    /** Hide subclasses (for type hierarchy diagrams) */
    def hideSubclasses: Boolean
    /** Show related classes from other objects/traits/packages (for content diagrams) */
    def hideInheritedNodes: Boolean
    /** Hide a node from the diagram */
    def hideNode(clazz: Node): Boolean
    /** Hide an edge from the diagram */
    def hideEdge(clazz1: Node, clazz2: Node): Boolean
  }

  /** Main entry point into this trait: generate the filter for inheritance diagrams */
  def makeInheritanceDiagramFilter(template: DocTemplateImpl): DiagramFilter = {

    val defaultFilter =
      if (template.isClass || template.isTrait || template.sym == AnyRefClass)
        FullDiagram
      else
        NoDiagramAtAll

    if (template.comment.isDefined)
      makeDiagramFilter(template, template.comment.get.inheritDiagram, defaultFilter, isInheritanceDiagram = true)
    else
      defaultFilter
  }

  /** Main entry point into this trait: generate the filter for content diagrams */
  def makeContentDiagramFilter(template: DocTemplateImpl): DiagramFilter = {
    val defaultFilter = if (template.isPackage || template.isObject) FullDiagram else NoDiagramAtAll
    if (template.comment.isDefined)
      makeDiagramFilter(template, template.comment.get.contentDiagram, defaultFilter, isInheritanceDiagram = false)
    else
      defaultFilter
  }

  protected var tFilter = 0l
  protected var tModel = 0l

  /** Show the entire diagram, no filtering */
  case object FullDiagram extends DiagramFilter {
    val hideDiagram: Boolean = false
    val hideIncomingImplicits: Boolean = false
    val hideOutgoingImplicits: Boolean = false
    val hideSuperclasses: Boolean = false
    val hideSubclasses: Boolean = false
    val hideInheritedNodes: Boolean = false
    def hideNode(clazz: Node): Boolean = false
    def hideEdge(clazz1: Node, clazz2: Node): Boolean = false
  }

  /** Hide the diagram completely, no need for special filtering */
  case object NoDiagramAtAll extends DiagramFilter {
    val hideDiagram: Boolean = true
    val hideIncomingImplicits: Boolean = true
    val hideOutgoingImplicits: Boolean = true
    val hideSuperclasses: Boolean = true
    val hideSubclasses: Boolean = true
    val hideInheritedNodes: Boolean = true
    def hideNode(clazz: Node): Boolean = true
    def hideEdge(clazz1: Node, clazz2: Node): Boolean = true
  }

  /** The AnnotationDiagramFilter trait directs the diagram engine according to an annotation
   *  TODO: Should document the annotation, for now see parseDiagramAnnotation in ModelFactory.scala */
  case class AnnotationDiagramFilter(hideDiagram: Boolean,
                                             hideIncomingImplicits: Boolean,
                                             hideOutgoingImplicits: Boolean,
                                             hideSuperclasses: Boolean,
                                             hideSubclasses: Boolean,
                                             hideInheritedNodes: Boolean,
                                             hideNodesFilter: List[Pattern],
                                             hideEdgesFilter: List[(Pattern, Pattern)]) extends DiagramFilter {

    private[this] def getName(n: Node): String =
      if (n.tpl.isDefined)
        n.tpl.get.qualifiedName
      else
        n.name

    def hideNode(clazz: Node): Boolean = {
      val qualifiedName = getName(clazz)
      for (hideFilter <- hideNodesFilter)
        if (hideFilter.matcher(qualifiedName).matches) {
          // println(hideFilter + ".matcher(" + qualifiedName + ").matches = " + hideFilter.matcher(qualifiedName).matches)
          return true
        }
      false
    }

    def hideEdge(clazz1: Node, clazz2: Node): Boolean = {
      val clazz1Name = getName(clazz1)
      val clazz2Name = getName(clazz2)
      for ((clazz1Filter, clazz2Filter) <- hideEdgesFilter) {
        if (clazz1Filter.matcher(clazz1Name).matches &&
            clazz2Filter.matcher(clazz2Name).matches) {
          // println(clazz1Filter + ".matcher(" + clazz1Name + ").matches = " + clazz1Filter.matcher(clazz1Name).matches)
          // println(clazz2Filter + ".matcher(" + clazz2Name + ").matches = " + clazz2Filter.matcher(clazz2Name).matches)
          return true
        }
      }
      false
    }
  }

  // TODO: This could certainly be improved -- right now the only regex is *, but there's no way to match a single identifier
  private val NodeSpecRegex = "\\\"[A-Za-z\\*][A-Za-z\\.\\*]*\\\""
  private val NodeSpecPattern = Pattern.compile(NodeSpecRegex)
  private val EdgeSpecRegex = "\\(" + NodeSpecRegex + "\\s*\\->\\s*" + NodeSpecRegex + "\\)"
  // And the composed regexes:
  private val HideNodesRegex = new Regex("^hideNodes(\\s*" + NodeSpecRegex + ")+$")
  private val HideEdgesRegex = new Regex("^hideEdges(\\s*" + EdgeSpecRegex + ")+$")

  private def makeDiagramFilter(template: DocTemplateImpl,
                                directives: List[String],
                                defaultFilter: DiagramFilter,
                                isInheritanceDiagram: Boolean): DiagramFilter = directives match {

    // if there are no specific diagram directives, return the default filter (either FullDiagram or NoDiagramAtAll)
    case Nil =>
      defaultFilter

    // compute the exact filters. By including the annotation, the diagram is automatically added
    case _ =>
      tFilter -= System.currentTimeMillis
      var hideDiagram0: Boolean = false
      var hideIncomingImplicits0: Boolean = false
      var hideOutgoingImplicits0: Boolean = false
      var hideSuperclasses0: Boolean = false
      var hideSubclasses0: Boolean = false
      var hideInheritedNodes0: Boolean = false
      var hideNodesFilter0: List[Pattern] = Nil
      var hideEdgesFilter0: List[(Pattern, Pattern)] = Nil

      def warning(message: String) = {
        // we need the position from the package object (well, ideally its comment, but yeah ...)
        val sym = if (template.sym.hasPackageFlag) template.sym.packageObject else template.sym
        assert((sym != global.NoSymbol) || (sym == global.rootMirror.RootPackage))
        global.reporter.warning(sym.pos, message)
      }

      def preparePattern(className: String) =
        "^" + className.stripPrefix("\"").stripSuffix("\"").replaceAll("\\.", "\\\\.").replaceAll("\\*", ".*") + "$"

      // separate entries:
      val entries = directives.foldRight("")(_ + " " + _).split(",").map(_.trim)
      for (entry <- entries)
        entry match {
          case "hideDiagram" =>
              hideDiagram0 = true
          case "hideIncomingImplicits" if isInheritanceDiagram =>
              hideIncomingImplicits0 = true
          case "hideOutgoingImplicits" if isInheritanceDiagram  =>
              hideOutgoingImplicits0 = true
          case "hideSuperclasses" if isInheritanceDiagram =>
              hideSuperclasses0 = true
          case "hideSubclasses" if isInheritanceDiagram =>
              hideSubclasses0 = true
          case "hideInheritedNodes" if !isInheritanceDiagram =>
              hideInheritedNodes0 = true
          case HideNodesRegex(last) =>
            val matcher = NodeSpecPattern.matcher(entry)
            while (matcher.find()) {
              val classPattern = Pattern.compile(preparePattern(matcher.group()))
              hideNodesFilter0 ::= classPattern
            }
          case HideEdgesRegex(last) =>
            val matcher = NodeSpecPattern.matcher(entry)
            while (matcher.find()) {
              val class1Pattern = Pattern.compile(preparePattern(matcher.group()))
              assert(matcher.find()) // it's got to be there, just matched it!
              val class2Pattern = Pattern.compile(preparePattern(matcher.group()))
              hideEdgesFilter0 ::= ((class1Pattern, class2Pattern))
            }
          case "" =>
            // don't need to do anything about it
          case _ =>
            warning("Could not understand diagram annotation in " + template.kind + " " + template.qualifiedName +
              ": unmatched entry \"" + entry + "\".\n" +
              "  This could be because:\n" +
              "   - you forgot to separate entries by commas\n" +
              "   - you used a tag that is not allowed in the current context (like @contentDiagram hideSuperclasses)\n"+
              "   - you did not use one of the allowed tags (see docs.scala-lang.org for scaladoc annotations)")
        }
      val result =
        if  (hideDiagram0)
          NoDiagramAtAll
        else if ((hideNodesFilter0.isEmpty) &&
                 (hideEdgesFilter0.isEmpty) &&
                 (hideIncomingImplicits0 == false) &&
                 (hideOutgoingImplicits0 == false) &&
                 (hideSuperclasses0 == false) &&
                 (hideSubclasses0 == false) &&
                 (hideInheritedNodes0 == false) &&
                 (hideDiagram0 == false))
          FullDiagram
        else
          AnnotationDiagramFilter(
            hideDiagram = hideDiagram0,
            hideIncomingImplicits = hideIncomingImplicits0,
            hideOutgoingImplicits = hideOutgoingImplicits0,
            hideSuperclasses = hideSuperclasses0,
            hideSubclasses = hideSubclasses0,
            hideInheritedNodes = hideInheritedNodes0,
            hideNodesFilter = hideNodesFilter0,
            hideEdgesFilter = hideEdgesFilter0)

      if (settings.docDiagramsDebug && result != NoDiagramAtAll && result != FullDiagram)
        settings.printMsg(template.kind + " " + template.qualifiedName + " filter: " + result)
      tFilter += System.currentTimeMillis

      result
  }
}
