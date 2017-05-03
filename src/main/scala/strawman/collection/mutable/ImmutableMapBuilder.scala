package strawman
package collection
package mutable

class ImmutableMapBuilder[K, V, CC[X, +Y] <: immutable.Map[X, Y] with immutable.MapLike[X, Y, CC]](empty: CC[K, V])
  extends ImmutableBuilder[(K, V), CC[K, V]](empty) {

  def add(kv: (K, V)): this.type = { elems = elems + kv; this }

}
