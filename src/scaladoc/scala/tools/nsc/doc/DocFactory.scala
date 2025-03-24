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

package scala.tools.nsc
package doc

import scala.tools.nsc.reporters.Reporter
import scala.reflect.internal.util.{BatchSourceFile, NoPosition}
import scala.tools.nsc.Reporting.WarningCategory
import scala.util.control.ControlThrowable

/** A documentation processor controls the process of generating Scala
  * documentation, which is as follows.
  *
  * * A simplified compiler instance (with only the front-end phases enabled)
  * * is created, and additional `sourceless` comments are registered.
  * * Documentable files are compiled, thereby filling the compiler's symbol table.
  * * A documentation model is extracted from the post-compilation symbol table.
  * * A generator is used to transform the model into the correct final format (HTML).
  *
  * A processor contains a single compiler instantiated from the processor's
  * `settings`. Each call to `document` uses the same compiler instance with
  * the same symbol table. In particular, this implies that the scaladoc site
  * obtained from a call to `run` will contain documentation about files compiled
  * during previous calls to the same processor's `run` method.
  *
  * @param reporter The reporter to which both documentation and compilation errors will be reported.
  * @param settings The settings to be used by the documenter and compiler for generating documentation.
  *
  * @author Gilles Dubochet */
// takes a `Reporter`, not `FilteringReporter` for sbt compatibility
class DocFactory(val reporter: Reporter, val settings: doc.Settings) { processor =>
  /** The unique compiler instance used by this processor and constructed from its `settings`. */
  object compiler extends ScaladocGlobal(settings, reporter)
  def runReporting: compiler.PerRunReporting = compiler.currentRun.reporting

  /** Creates a scaladoc site for all symbols defined in this call's `source`,
    * as well as those defined in `sources` of previous calls to the same processor.
    * @param source The list of paths (relative to the compiler's source path,
    *        or absolute) of files to document or the source code. */
  def makeUniverse(source: Either[List[String], String]): Option[Universe] = {
    assert(settings.docformat.value == "html")
    source match {
      case Left(files) =>
        new compiler.Run() compile files
      case Right(sourceCode) =>
        new compiler.Run() compileSources List(new BatchSourceFile("newSource", sourceCode))
    }

    if (reporter.hasErrors)
      return None

    val extraTemplatesToDocument: Set[compiler.Symbol] = {
      if (settings.docUncompilable.isDefault) Set()
      else {
        val uncompilable = new {
          val global: compiler.type = compiler
          val settings = processor.settings
        } with Uncompilable { }

        compiler.docComments ++= uncompilable.comments
        docdbg("" + uncompilable)

        uncompilable.templates
      }
    }

    val modelFactory = (
      new { override val global: compiler.type = compiler }
        with model.ModelFactory(compiler, settings)
        with model.ModelFactoryImplicitSupport
        with model.ModelFactoryTypeSupport
        with model.diagram.DiagramFactory
        with model.CommentFactory
        with model.TreeFactory
        with model.MemberLookup {
          override def templateShouldDocument(sym: compiler.Symbol, inTpl: DocTemplateImpl) =
            extraTemplatesToDocument(sym) || super.templateShouldDocument(sym, inTpl)
        }
    )

    modelFactory.makeModel match {
      case Some(madeModel) =>
        if (settings.verbose.value)
          reporter.echo("model contains " + modelFactory.templatesCount + " documentable templates")
        Some(madeModel)
      case None =>
        if (settings.verbose.value)
          reporter.echo("no documentable class found in compilation units")
        None
    }
  }

  object NoCompilerRunException extends ControlThrowable { }

  val documentError: PartialFunction[Throwable, Unit] = {
    case NoCompilerRunException =>
      if (settings.verbose.value)
        reporter.echo("No documentation generated with unsuccessful compiler run")
    case e @ (_:ClassNotFoundException | _:IllegalAccessException | _:InstantiationException | _:SecurityException | _:ClassCastException) =>
      reporter.error(null, s"Cannot load the doclet class ${settings.docgenerator.value} (specified with ${settings.docgenerator.name}): $e. Leaving the default settings will generate the html version of scaladoc.")
  }

  /** Generate document(s) for all `files` containing scaladoc documentation.
    * @param files The list of paths (relative to the compiler's source path, or absolute) of files to document. */
  def document(files: List[String]): Unit = {
    def generate() = {
      import doclet._
      val docletClass    = Class.forName(settings.docgenerator.value) // default is html.Doclet
      val docletInstance =
        docletClass
        .getConstructors
        .find { constr =>
          constr.getParameterTypes.length == 1 &&
          constr.getParameterTypes.apply(0) == classOf[scala.reflect.internal.Reporter]
        }
        .map(_.newInstance(reporter))
        .getOrElse{
          runReporting.warning(NoPosition, "Doclets should be created with the Reporter constructor, otherwise logging reporters will not be shared by the creating parent", WarningCategory.Scaladoc, site = "")
          docletClass.getConstructor().newInstance()
        }
        .asInstanceOf[Generator]

      docletInstance match {
        case universer: Universer =>
          val universe = makeUniverse(Left(files)) getOrElse { throw NoCompilerRunException }
          universer setUniverse universe
        case _ => ()
      }
      docletInstance.generate()
    }

    try generate()
    catch documentError
  }

  private[doc] def docdbg(msg: String): Unit = {
    if (settings.Ydocdebug.value)
      println(msg)
  }
}
