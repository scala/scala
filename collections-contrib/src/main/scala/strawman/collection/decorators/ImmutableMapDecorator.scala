package strawman
package collection
package decorators

class ImmutableMapDecorator[K, V, CC[X, +Y] <: immutable.MapOps[X, Y, CC, CC[X, Y]]](`this`: CC[K, V]) {

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
    *   val updatedMap =
    *     map.updatedWith("foo") { case Some(previous) => Some(previous * 2) }
    * }}}
    *
    * @return A new updated `Map`
    */
  def updatedWith(key: K)(f: PartialFunction[Option[V], Option[V]]): CC[K, V] = {
    val pf = f.lift
    val previousValue = `this`.get(key)
    pf(previousValue) match {
      case None => `this`
      case Some(result) =>
        result match {
          case None => `this` - key
          case Some(v) => `this` + (key -> v)
        }
    }
  }

}
