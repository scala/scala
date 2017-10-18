package strawman
package collection
package decorators

class ImmutableMapDecorator[K, V, CC[X, +Y] <: immutable.MapOps[X, Y, CC, CC[X, Y]]](`this`: CC[K, V]) {

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
