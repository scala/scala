package strawman.collection
package decorators

trait MapDecorator[K, V] {

  val `this`: Map[K, V]

  /**
    * Combines entries of `this` Map with entries of `that` Map that have the same key,
    * using the combination function `f`
    */
  def zipByKeyWith[W, X, C](that: Map[K, W])(f: (V, W) => X)(implicit bf: BuildFrom[`this`.type, (K, X), C]): C = {
    val b = bf.newBuilder(`this`)
    for {
      (k, v) <- `this`
      w <- that.get(k)
    } {
      b += k -> f(v, w)
    }
    b.result()
  }

}
