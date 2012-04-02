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

        def code = """ ... """
        def scaladocSettings = ""
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

  // Implementation follows:
  override def extraSettings: String = "-usejavacp"

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    
    try {
      // 1 - compile with scaladoc and get the model out
      val args = scaladocSettings.split(" ")
      val universe = model(args:_*).getOrElse({sys.error("Scaladoc Model Test ERROR: No universe generated!")})
      // 2 - check the model generated
      testModel(universe.rootPackage)
    } catch {
      case e => 
        println(e)
        e.printStackTrace
    }
    // set err back to the real err handler
    System.setErr(prevErr)
  }

  // create a new scaladoc compiler
  def newDocFactory(args: String*): DocFactory = {
    val settings = new Settings(_ => ())
    val command = new ScalaDoc.Command((CommandLineParser tokenize extraSettings) ++ args.toList, settings)
    val docFact = new DocFactory(new ConsoleReporter(settings), settings)
    docFact
  }

  // compile with scaladoc and output the result
  def model(args: String*): Option[Universe] = newDocFactory(args: _*).makeUniverse(Right(code))

  // so we don't get the newSettings warning
  override def isDebug = false 


  // finally, enable easy navigation inside the entities
  object access {

    // Make it easy to access things
    class TemplateAccess(tpl: DocTemplateEntity) {

      def _class(name: String): DocTemplateEntity = getTheFirst(_classes(name), tpl.qualifiedName + ".class(" + name + ")")
      def _classes(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).flatMap({ case c: Class => List(c)})

      def _trait(name: String): DocTemplateEntity = getTheFirst(_traits(name), tpl.qualifiedName + ".trait(" + name + ")")
      def _traits(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).flatMap({ case t: Trait => List(t)})

      def _object(name: String): DocTemplateEntity = getTheFirst(_objects(name), tpl.qualifiedName + ".object(" + name + ")")
      def _objects(name: String): List[DocTemplateEntity] = tpl.templates.filter(_.name == name).flatMap({ case o: Object => List(o)})

      def _method(name: String): Def = getTheFirst(_methods(name), tpl.qualifiedName + ".method(" + name + ")")
      def _methods(name: String): List[Def] = tpl.methods.filter(_.name == name)
      
      def _value(name: String): Val = getTheFirst(_values(name), tpl.qualifiedName + ".value(" + name + ")")
      def _values(name: String): List[Val] = tpl.values.filter(_.name == name)

      def getTheFirst[T](list: List[T], expl: String): T = {
        if (list.length == 1)
          list.head
        else if (list.length == 0)
          sys.error("Error getting " + expl + ": No such element. All elements in list: [" + list.mkString(", ") + "]")
        else 
          sys.error("Error getting " + expl + ": " + list.length + " elements with this name. " +
            "All elements in list: [" + list.mkString(", ") + "]")
      }
    }

    class PackageAccess(pack: Package) extends TemplateAccess(pack) {
      def _package(name: String): Package = getTheFirst(_packages(name), pack.qualifiedName + ".package(" + name + ")")
      def _packages(name: String): List[Package] = pack.packages.filter(_.name == name)
    }

    implicit def templateAccess(tpl: DocTemplateEntity) = new TemplateAccess(tpl)
    implicit def packageAccess(pack: Package) = new PackageAccess(pack)
  }
}
