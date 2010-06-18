package scala.collection




trait Sequentializable[+T, +Repr] {

  /** A view of this parallel collection, but with all
   *  of the operations implemented sequentially (i.e. in a single-threaded manner).
   *
   *  @return a sequential view of the collection.
   */
  def seq: Repr

}