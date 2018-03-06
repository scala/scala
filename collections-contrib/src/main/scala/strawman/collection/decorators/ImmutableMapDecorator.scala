package strawman
package collection
package decorators

class ImmutableMapDecorator[C, M <: HasImmutableMapOps[C]](coll: C)(implicit val map: M) {

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
  def updatedWith[That](key: map.K)(f: PartialFunction[Option[map.V], Option[map.V]])(implicit bf: BuildFrom[C, (map.K, map.V), That]): That = {
    val pf = f.lift
    val `this` = map(coll)
    val previousValue = `this`.get(key)
    pf(previousValue) match {
      case None => bf.fromSpecificIterable(coll)(`this`.toIterable)
      case Some(result) =>
        result match {
          case None => bf.fromSpecificIterable(coll)((`this` - key).toIterable)
          case Some(v) => bf.fromSpecificIterable(coll)((`this` + (key -> v)).toIterable)
        }
    }
  }

}
