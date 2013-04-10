package scala.tools.nsc
package doc
package doclet

/** A `Generator` may implement the `Indexer` trait to gain access to pre-calculated indexing information */
trait Indexer extends Generator with Universer {

  protected var indexField: Index = null

  def index: Index = indexField

  def setIndex(i: Index) {
    assert(indexField == null)
    indexField = i
  }

  checks += { () =>
    indexField != null
  }

}