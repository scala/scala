/* NSC -- new Scala compiler -- Copyright 2007-2011 LAMP/EPFL */

package scala.tools.nsc
package doc

import reporters.Reporter
import util.NoPosition
import java.lang.ClassNotFoundException

/** A documentation processor controls the process of generating Scala documentation, which is as follows.
  *
  * * A simplified compiler instance (with only the front-end phases enabled) is created, and additional
  *   ''sourceless'' comments are registered.
  * * Documentable files are compiled, thereby filling the compiler's symbol table.
  * * A documentation model is extracted from the post-compilation symbol table.
  * * A generator is used to transform the model into the correct final format (HTML).
  *
  * A processor contains a single compiler instantiated from the processor's `settings`. Each call to `document`
  * uses the same compiler instance with the same symbol table. In particular, this implies that the scaladoc site
  * obtained from a call to `run` will contain documentation about files compiled during previous calls to the same
  * processor's `run` method.
  *
  * @param reporter The reporter to which both documentation and compilation errors will be reported.
  * @param settings The settings to be used by the documenter and compiler for generating documentation.
  *
  * @author Gilles Dubochet */
class DocFactory(val reporter: Reporter, val settings: doc.Settings) { processor =>

  /** The unique compiler instance used by this processor and constructed from its `settings`. */
  object compiler extends Global(settings, reporter) with interactive.RangePositions {
    override protected def computeInternalPhases() {
      phasesSet += syntaxAnalyzer
      phasesSet += analyzer.namerFactory
      phasesSet += analyzer.packageObjects
      phasesSet += analyzer.typerFactory
      phasesSet += superAccessors
      phasesSet += pickler
      phasesSet += refchecks
    }
    override def forScaladoc = true
    lazy val addSourceless = {
      val sless = new SourcelessComments { val global = compiler }
      docComments ++= sless.comments
    }
  }

  /** Creates a scaladoc site for all symbols defined in this call's `files`, as well as those defined in `files` of
    * previous calls to the same processor.
    * @param files The list of paths (relative to the compiler's source path, or absolute) of files to document. */
  def makeUniverse(files: List[String]): Option[Universe] = {
    (new compiler.Run()) compile files
    compiler.addSourceless
    assert(settings.docformat.value == "html")
    if (!reporter.hasErrors) {
      val modelFactory = (new model.ModelFactory(compiler, settings) with model.comment.CommentFactory with model.TreeFactory)
      val madeModel = Some(modelFactory.makeModel)
      println("model contains " + modelFactory.templatesCount + " documentable templates")
      madeModel
    }
    else None
  }

  /** Generate document(s) for all `files` containing scaladoc documenataion.
    * @param files The list of paths (relative to the compiler's source path, or absolute) of files to document. */
  def document(files: List[String]): Unit = {

    class NoCompilerRunException extends Exception

    try {
      val docletClass = Class.forName(settings.docgenerator.value) // default is html.Doclet
      val docletInstance = docletClass.newInstance().asInstanceOf[doclet.Generator]
      if (docletInstance.isInstanceOf[doclet.Universer]) {
        makeUniverse(files) match {
          case Some(universe) =>
            docletClass.getMethod("setUniverse", classOf[Universe]).invoke(docletInstance, universe)
            if (docletInstance.isInstanceOf[doclet.Indexer]) {
              val index = model.IndexModelFactory.makeIndex(universe)
              docletClass.getMethod("setIndex", classOf[Index]).invoke(docletInstance, index)
            }
          case None =>
            throw new NoCompilerRunException()
        }
      }
      docletInstance.generate
    }
    catch {
      case e: ClassNotFoundException =>
      case e: NoCompilerRunException =>
        reporter.info(NoPosition, "No documentation generated with unsucessful compiler run", false)
    }



  }

}
