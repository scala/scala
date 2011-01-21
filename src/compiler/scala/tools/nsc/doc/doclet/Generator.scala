package scala.tools.nsc.doc
package doclet

import scala.collection._

/** Hook into the documentation generation process.  The Doclet receives a model of the code being generated, and
  * can then do whatever it wants with it. */
abstract class Generator {

  val checks: mutable.Set[()=>Boolean] =
    mutable.Set.empty[()=>Boolean]

  /** Called after the model of the generated documentation is created */
  def generate: Unit = {
    assert(checks forall { check => check() })
    generateImpl
  }

  def generateImpl: Unit

}



