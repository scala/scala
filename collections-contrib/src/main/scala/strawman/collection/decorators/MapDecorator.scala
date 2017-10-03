package strawman.collection
package decorators

trait MapDecorator[K, V] {

  val `this`: Map[K, V]

  /**
    * Combines entries of `this` Map with entries of `that` Map that have the same key,
    * using the combination function `f`
    *
    * @param that Other Map
    * @param f Combination function
    * @param bf Builder driven by the type of `this` Map
    * @tparam W Type of values of the other Map (e.g. `Int`, `String`)
    * @tparam X Type of values of the resulting Map
    * @tparam C Type of the resulting Map
    * @return A Map that associates all the keys `k` contained by both `this` and `that` to the result of
    *         the application of `f` to the values `v` and `w` respectively associated by `this` and `that` to `k`
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

  /**
    * Combines the entries of `this` Map with the entries of `that` Map that have the same key,
    * tupling their values. `xs.zipByKey(ys)` is a shorthand for `xs.zipByKeyWith(ys)((_, _))`.
    *
    * @param that Other Map
    * @param bf Builder driven by the type of `this` Map
    * @tparam W Type of values of the other Map (e.g. `Int`, `String`)
    * @tparam C Type of the result (e.g. `Map[Int, (String, Boolean)]`, `TreeMap[String, (Int, Int)]`)
    * @return A Map that associates all the keys `k` contained by both `this` and `that` to pairs `(v, w)` where `v`
    *         is the value associated by `this` to `k` and `w` the value associated by `that` to `k`
    */
  def zipByKey[W, C](that: Map[K, W])(implicit bf: BuildFrom[`this`.type, (K, (V, W)), C]): C =
    zipByKeyWith(that)((_, _))

  /** Alias for `zipByKey` */
  @`inline` final def join[W, C](that: Map[K, W])(implicit bf: BuildFrom[`this`.type, (K, (V, W)), C]): C = zipByKey(that)

  /**
    * @return A Map associating all the keys from `this` and `that` with values returned by the partial function
    *         `f`, when this one is defined.
    *
    * @param that Map to merge
    * @param f Combination function
    * @param bf Builder driven by the type of `this` Map
    * @tparam W Type of values of the other Map (e.g. `Int`, `String`)
    * @tparam X Type of values of the resulting Map
    * @tparam C Type of the result (e.g. `Map[Int, (String, Option[Boolean])]`)
    */
  def mergeByKeyWith[W, X, C](that: Map[K, W])(f: PartialFunction[(Option[V], Option[W]), X])(implicit bf: BuildFrom[`this`.type, (K, X), C]): C = {
    val b = bf.newBuilder(`this`)
    val traversed = mutable.Set.empty[W]
    val pf = f.lift
    for {
      (k, v) <- `this`
      x <- pf(that.get(k).fold[(Option[V], Option[W])]((Some(v), None)){ w => traversed += w; (Some(v), Some(w)) })
    } {
      b += k -> x
    }
    for {
      (k, w) <- that if !traversed(w)
      x <- pf((None, Some(w)))
    } {
      b += k -> x
    }
    b.result()
  }

  /**
    * Perform a full outer join of `this` and `that`.
    *
    * Equivalent to `mergeByKeyWith(that) { case any => any }`.
    *
    * @param bf Builder for the resulting collection
    * @tparam W Type of values of `that` (e.g. `String`)
    * @tparam C Type of the resulting collection (e.g. `Map[Int, (Option[Boolean], Option[String])]`)
    * @return A Map that associates all the keys `k` of `this` or `that` to pairs `(Some(v), Some(w))` if `this`
    *         associates `k` to `v` and `that` associates `k` to `w`, or pairs `(None, Some(w))` if `this` doesn’t
    *         contain `k`, or pairs `(Some(v), None)` if `that` doesn’t contain `k`
    */
  @`inline` final def mergeByKey[W, C](that: Map[K, W])(implicit bf: BuildFrom[`this`.type, (K, (Option[V], Option[W])), C]): C =
    mergeByKeyWith(that) { case any => any }

  /** Alias for `mergeByKey` */
  @`inline` final def fullOuterJoin[W, C](that: Map[K, W])(implicit bf: BuildFrom[`this`.type, (K, (Option[V], Option[W])), C]): C =
    mergeByKey(that)

  /**
    * Perform a left outer join of `this` and `that`.
    *
    * Equivalent to `mergeByKeyWith(that) { case (Some(v), maybeW) => (v, maybeW) }`.
    *
    * @param bf Builder for the resulting collection
    * @tparam W Type of values of `that`
    * @tparam C Type of the resulting collection
    * @return A Map that associates all the keys `k` of `this` to pairs `(v, Some(w))` if `that` associates `k` to `w`,
    *         or `(v, None)` if `that` doesn’t contain `k`
    */
  def leftOuterJoin[W, C](that: Map[K, W])(implicit bf: BuildFrom[`this`.type, (K, (V, Option[W])), C]): C = {
    val b = bf.newBuilder(`this`)
    for ((k, v) <- `this`) {
      b += k -> (v, that.get(k))
    }
    b.result()
  }

  /**
    * Perform a right outer join of `this` and `that`.
    *
    * Equivalent to `mergeByKeyWith(that) { case (maybeV, Some(w)) => (maybeV, w) }`.
    *
    * @param bf Builder for the resulting collection
    * @tparam W Type of values of `that` (e.g. `String`)
    * @tparam C Type of the resulting collection (e.g. `Map[Int, (Option[Boolean], String)]`)
    * @return A Map that associates all the keys `k` of `that` to pairs `(Some(v), w)` if `this` associates `k` to `v`,
    *         or `(None, w)` if `this` doesn’t contain `k`
    */
  def rightOuterJoin[W, C](that: Map[K, W])(implicit bf: BuildFrom[`this`.type, (K, (Option[V], W)), C]): C = {
    val b = bf.newBuilder(`this`)
    for ((k, w) <- that) {
      b += k -> (`this`.get(k), w)
    }
    b.result()
  }

}
