/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Vlad Ureche
 */

package scala.tools.partest

import scala.tools.partest._
import java.io._
import scala.tools.nsc._
import scala.tools.nsc.util.CommandLineParser
import scala.tools.nsc.doc.{Settings, DocFactory, Universe}
import scala.tools.nsc.doc.model._
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
      case e =>
        println(e)
        e.printStackTrace
    }
    // set err back to the real err handler
    System.setErr(prevErr)
  }

  private[this] var settings: Settings = null

  // create a new scaladoc compiler
  private[this] def newDocFactory: DocFactory = {
    settings = new Settings(_ => ())
    settings.scaladocQuietRun = true // yaay, no more "model contains X documentable templates"!
    val args = extraSettings + " " + scaladocSettings
    val command = new ScalaDoc.Command((CommandLineParser tokenize (args)), settings)
    val docFact = new DocFactory(new ConsoleReporter(settings), settings)
    docFact
  }

  // compile with scaladoc and output the result
  def model: Option[Universe] = newDocFactory.makeUniverse(Right(code))

  // so we don't get the newSettings warning
  override def isDebug = false


  // finally, enable easy navigation inside the entities
  object access {

    class TemplateAccess(tpl: DocTemplateEntity) {
      def _class(name: String): DocTemplateEntity = getTheFirst(_classes(name), tpl.qualifiedName + ".class(" + name + ")")
      def _classes(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case c: Class => c})

      def _trait(name: String): DocTemplateEntity = getTheFirst(_traits(name), tpl.qualifiedName + ".trait(" + name + ")")
      def _traits(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case t: Trait => t})

      def _object(name: String): DocTemplateEntity = getTheFirst(_objects(name), tpl.qualifiedName + ".object(" + name + ")")
      def _objects(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).collect({ case o: Object => o})

      def _method(name: String): Def = getTheFirst(_methods(name), tpl.qualifiedName + ".method(" + name + ")")
      def _methods(name: String): List[Def] = tpl.methods.filter(_.name == name)

      def _value(name: String): Val = getTheFirst(_values(name), tpl.qualifiedName + ".value(" + name + ")")
      def _values(name: String): List[Val] = tpl.values.filter(_.name == name)

      def _conversion(name: String): ImplicitConversion = getTheFirst(_conversions(name), tpl.qualifiedName + ".conversion(" + name + ")")
      def _conversions(name: String): List[ImplicitConversion] = tpl.conversions.filter(_.conversionQualifiedName == name)
    }

    class PackageAccess(pack: Package) extends TemplateAccess(pack) {
      def _package(name: String): Package = getTheFirst(_packages(name), pack.qualifiedName + ".package(" + name + ")")
      def _packages(name: String): List[Package] = pack.packages.filter(_.name == name)
    }

    class MemberAccess(mbrs: WithMembers) {
      def _member(name: String): MemberEntity = getTheFirst(_members(name), mbrs.toString + ".member(" + name + ")")
      def _members(name: String): List[MemberEntity] = mbrs.members.filter(_.name == name)
    }

    type WithMembers = { def members: List[MemberEntity]; def toString: String } /* DocTemplates and ImplicitConversions */

    implicit def templateAccess(tpl: DocTemplateEntity) = new TemplateAccess(tpl)
    implicit def packageAccess(pack: Package) = new PackageAccess(pack)
    implicit def membersAccess(mbrs: WithMembers) = new MemberAccess(mbrs)

    def getTheFirst[T](list: List[T], expl: String): T = list.length match {
      case 1 => list.head
      case 0 => sys.error("Error getting " + expl + ": No such element.")
      case _ => sys.error("Error getting " + expl + ": " + list.length + " elements with this name. " +
                  "All elements in list: [" + list.mkString(", ") + "]")
    }
  }
}
