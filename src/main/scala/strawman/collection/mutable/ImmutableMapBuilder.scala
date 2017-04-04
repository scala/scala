package strawman
package collection.mutable

class ImmutableMapBuilder[
  K, V,
  C[X, +Y] <: collection.immutable.Map[X, Y] with collection.immutable.MapValuePolyTransforms[X, Y, C]
](empty: C[K, V])
  extends ImmutableBuilder[(K, V), C[K, V]](empty) {

  def += (kv: (K, V)): this.type = { elems = elems + kv; this }

}
