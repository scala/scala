/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Vlad Ureche
 */

package scala.tools.partest

import scala.tools.nsc._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.doc.{ DocFactory, Universe }
import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.reporters.ConsoleReporter

/** A class for testing scaladoc model generation
 *   - you need to specify the code in the `code` method
 *   - you need to override the testModel method to test the model
 *   - you may specify extra parameters to send to scaladoc in `scaladocSettings`
 * {{{
      import scala.tools.nsc.doc.model._
      import scala.tools.partest.ScaladocModelTest

      object Test extends ScaladocModelTest {

        override def code = """ ... """ // or override def resourceFile = "<file>.scala" (from test/scaladoc/resources)
        def scaladocSettings = " ... "
        def testModel(rootPackage: Package) = {
          // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
          import access._

          // just need to check the member exists, access methods will throw an error if there's a problem
          rootPackage._package("scala")._package("test")._class("C")._method("foo")
        }
      }
 * }}}
 */
abstract class ScaladocModelTest extends DirectTest {

  /** Override this to give scaladoc command line parameters */
  def scaladocSettings: String

  /** Override this to test the model */
  def testModel(root: Package): Unit

  /** Override to feed a file in resources to scaladoc*/
  def resourceFile: String = null

  /** Override to feed code into scaladoc */
  override def code =
    if (resourceFile ne null)
      io.File(resourcePath + "/" + resourceFile).slurp()
    else
      sys.error("Scaladoc Model Test: You need to give a file or some code to feed to scaladoc!")

  def resourcePath = io.Directory(sys.props("partest.cwd") + "/../resources")

  // Implementation follows:
  override def extraSettings: String = "-usejavacp"

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)

    try {
      // 1 - compile with scaladoc and get the model out
      val universe = model.getOrElse({sys.error("Scaladoc Model Test ERROR: No universe generated!")})
      // 2 - check the model generated
      testModel(universe.rootPackage)
      println("Done.")
    } catch {
      case e: Exception =>
        println(e)
        e.printStackTrace
    }
    // set err back to the real err handler
    System.setErr(prevErr)
  }

  private[this] var settings: doc.Settings = null

  // create a new scaladoc compiler
  def newDocFactory: DocFactory = {
    settings = new doc.Settings(_ => ())
    settings.scaladocQuietRun = true // yaay, no more "model contains X documentable templates"!
    val args = extraSettings + " " + scaladocSettings
    new ScalaDoc.Command((CommandLineParser tokenize (args)), settings) // side-effecting, I think
    val docFact = new DocFactory(new ConsoleReporter(settings), settings)
    docFact
  }

  // compile with scaladoc and output the result
  def model: Option[Universe] = newDocFactory.makeUniverse(Right(code))

  // so we don't get the newSettings warning
  override def isDebug = false

  // finally, enable easy navigation inside the entities
  object access {

    implicit class TemplateAccess(tpl: DocTemplateEntity) {
      def _class(name: String): DocTemplateEntity = getTheFirst(_classes(name), tpl.qualifiedName + ".class(" + name + ")")
      def _classes(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case c: DocTemplateEntity with Class => c})

      def _classMbr(name: String): MemberTemplateEntity = getTheFirst(_classesMbr(name), tpl.qualifiedName + ".classMember(" + name + ")")
      def _classesMbr(name: String): List[MemberTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case c: MemberTemplateEntity if c.isClass => c})

      def _trait(name: String): DocTemplateEntity = getTheFirst(_traits(name), tpl.qualifiedName + ".trait(" + name + ")")
      def _traits(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case t: DocTemplateEntity with Trait => t})

      def _traitMbr(name: String): MemberTemplateEntity = getTheFirst(_traitsMbr(name), tpl.qualifiedName + ".traitMember(" + name + ")")
      def _traitsMbr(name: String): List[MemberTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case t: MemberTemplateEntity if t.isTrait => t})

      def _object(name: String): DocTemplateEntity = getTheFirst(_objects(name), tpl.qualifiedName + ".object(" + name + ")")
      def _objects(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case o: DocTemplateEntity with Object => o})

      def _objectMbr(name: String): MemberTemplateEntity = getTheFirst(_objectsMbr(name), tpl.qualifiedName + ".objectMember(" + name + ")")
      def _objectsMbr(name: String): List[MemberTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case o: MemberTemplateEntity if o.isObject => o})

      def _method(name: String): Def = getTheFirst(_methods(name), tpl.qualifiedName + ".method(" + name + ")")
      def _methods(name: String): List[Def] = tpl.methods.filter(_.name == name)

      def _value(name: String): Val = getTheFirst(_values(name), tpl.qualifiedName + ".value(" + name + ")")
      def _values(name: String): List[Val] = tpl.values.filter(_.name == name)

      def _conversion(name: String): ImplicitConversion = getTheFirst(_conversions(name), tpl.qualifiedName + ".conversion(" + name + ")")
      def _conversions(name: String): List[ImplicitConversion] = tpl.conversions.filter(_.conversionQualifiedName == name)

      def _absType(name: String): MemberEntity = getTheFirst(_absTypes(name), tpl.qualifiedName + ".abstractType(" + name + ")")
      def _absTypes(name: String): List[MemberEntity] = tpl.members.filter(mbr => mbr.name == name && mbr.isAbstractType)

      def _absTypeTpl(name: String): DocTemplateEntity = getTheFirst(_absTypeTpls(name), tpl.qualifiedName + ".abstractType(" + name + ")")
      def _absTypeTpls(name: String): List[DocTemplateEntity] = tpl.members.collect({ case dtpl: DocTemplateEntity with AbstractType if dtpl.name == name => dtpl })

      def _aliasType(name: String): MemberEntity = getTheFirst(_aliasTypes(name), tpl.qualifiedName + ".aliasType(" + name + ")")
      def _aliasTypes(name: String): List[MemberEntity] = tpl.members.filter(mbr => mbr.name == name && mbr.isAliasType)

      def _aliasTypeTpl(name: String): DocTemplateEntity = getTheFirst(_aliasTypeTpls(name), tpl.qualifiedName + ".aliasType(" + name + ")")
      def _aliasTypeTpls(name: String): List[DocTemplateEntity] = tpl.members.collect({ case dtpl: DocTemplateEntity with AliasType if dtpl.name == name => dtpl })
    }

    trait WithMembers {
      def members: List[MemberEntity]
      def _member(name: String): MemberEntity = getTheFirst(_members(name), this.toString + ".member(" + name + ")")
      def _members(name: String): List[MemberEntity] = members.filter(_.name == name)
    }
    implicit class PackageAccess(pack: Package) extends TemplateAccess(pack) {
      def _package(name: String): Package = getTheFirst(_packages(name), pack.qualifiedName + ".package(" + name + ")")
      def _packages(name: String): List[Package] = pack.packages.filter(_.name == name)
    }
    implicit class DocTemplateEntityMembers(val underlying: DocTemplateEntity) extends WithMembers {
      def members = underlying.members
    }
    implicit class ImplicitConversionMembers(val underlying: ImplicitConversion) extends WithMembers {
      def members = underlying.members
    }

    def getTheFirst[T](list: List[T], expl: String): T = list.length match {
      case 1 => list.head
      case 0 => sys.error("Error getting " + expl + ": No such element.")
      case _ => sys.error("Error getting " + expl + ": " + list.length + " elements with this name. " +
                  "All elements in list: [" + list.map({
                    case ent: Entity => ent.kind + " " + ent.qualifiedName
                    case other => other.toString
                  }).mkString(", ") + "]")
    }

    def extractCommentText(c: Any) = {
      def extractText(body: Any): String = body match {
        case s: String  => s
        case s: Seq[_]  => s.toList.map(extractText(_)).mkString
        case p: Product => p.productIterator.toList.map(extractText(_)).mkString
        case _          => ""
      }
      c match {
        case c: Comment =>
          extractText(c.body)
        case b: Body =>
          extractText(b)
      }
    }

    def countLinks(c: Comment, p: EntityLink => Boolean): Int = countLinksInBody(c.body, p)

    def countLinksInBody(body: Body, p: EntityLink => Boolean): Int = {
      def countLinks(b: Any): Int = b match {
        case el: EntityLink if p(el) => 1
        case s: Seq[_]  => s.toList.map(countLinks(_)).sum
        case p: Product => p.productIterator.toList.map(countLinks(_)).sum
        case _          => 0
      }
      countLinks(body)
    }

    def testDiagram(doc: DocTemplateEntity, diag: Option[Diagram], nodes: Int, edges: Int) = {
      assert(diag.isDefined, doc.qualifiedName + " diagram missing")
      assert(diag.get.nodes.length == nodes,
             doc.qualifiedName + "'s diagram: node count " + diag.get.nodes.length + " == " + nodes)
      assert(diag.get.edges.map(_._2.length).sum == edges,
             doc.qualifiedName + "'s diagram: edge count " + diag.get.edges.length + " == " + edges)
    }
  }
}
