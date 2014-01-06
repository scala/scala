package scala.tools.nsc
package doc
package doclet

/** A `Generator` may implement the `Universer` trait to gain access to a model of the documented program */
trait Universer extends Generator {

  protected var universeField: Universe = null

  def universe: Universe = universeField

  def setUniverse(u: Universe) {
    assert(universeField == null)
    universeField = u
  }

  checks += { () =>
    universeField != null
  }

}