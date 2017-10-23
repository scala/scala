package strawman
package collection
package decorators

class MutableMapDecorator[K, V](`this`: mutable.Map[K, V]) {

  /**
    * Updates an existing binding or create a new one according to the
    * result of the application of `f`.
    *
    * This operation retrieves the current binding for `key` and passes
    * it to the partial function `f`. If `f` is not defined, nothing
    * is changed on the underlying Map. If `f` returns `Some(v)`, the
    * binding is updated with the new value `v`. If `f` returns `None`,
    * the binding is removed.
    *
    * For example, to update an existing binding:
    *
    * {{{
    *   updateWith("foo") { case Some(previous) => Some(previous * 2) }
    * }}}
    *
    * @return The current value associated with `key`, or `None` if the
    *         there is no such binding or if it has been removed
    */
  def updateWith(key: K)(f: PartialFunction[Option[V], Option[V]]): Option[V] = {
    val pf = f.lift
    val previousValue = `this`.get(key)
    pf(previousValue) match {
      case None => previousValue
      case Some(result) =>
        result match {
          case None =>
            `this` -= key
            None
          case Some(v) =>
            `this`(key) = v
            Some(v)
        }
    }
  }

}