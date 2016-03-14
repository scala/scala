package scala.tools.nsc.doc
package doclet

import scala.collection._

/** Custom Scaladoc generators must implement the `Generator` class. A custom generator can be selected in Scaladoc
  * using the `-doc-generator` command line option.
  * The `Generator` class does not provide data about the documented code. A number of data provider traits can be used
  * to configure what data is actually available to the generator:
  *  - A `Universer` provides a `Universe` data structure representing the interfaces and comments of the documented
  *    program.
  * To implement this class only requires defining method `generateImpl`. */
abstract class Generator {

  /** A series of tests that must be true before generation can be done. This is used by data provider traits to
    * confirm that they have been correctly initialised before allowing generation to proceed. */
  protected val checks: mutable.Set[()=>Boolean] =
    mutable.Set.empty[()=>Boolean]

  /** Outputs documentation (as a side effect). */
  def generate(): Unit = {
    assert(checks forall { check => check() })
    generateImpl()
  }

  /** Outputs documentation (as a side effect). This method is called only if all `checks` are true. */
  protected def generateImpl(): Unit

}
