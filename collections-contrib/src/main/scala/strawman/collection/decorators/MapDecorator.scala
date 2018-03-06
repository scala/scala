package strawman.collection
package decorators

class MapDecorator[C, M <: HasMapOps[C]](coll: C)(implicit val map: M) {

  /**
    * Combines entries of `this` Map with entries of `that` Map that have the same key,
    * using the combination function `f`
    *
    * @param other Other Map
    * @param f Combination function
    * @param bf Builder driven by the type of `this` Map
    * @tparam W Type of values of the other Map (e.g. `Int`, `String`)
    * @tparam X Type of values of the resulting Map
    * @tparam That Type of the resulting Map
    * @return A Map that associates all the keys `k` contained by both `this` and `that` to the result of
    *         the application of `f` to the values `v` and `w` respectively associated by `this` and `that` to `k`
    */
  def zipByKeyWith[W, X, That](other: Map[map.K, W])(f: (map.V, W) => X)(implicit bf: BuildFrom[C, (map.K, X), That]): That = {
    val b = bf.newBuilder(coll)
    for {
      (k, v) <- map(coll)
      w <- other.get(k)
    } {
      b += k -> f(v, w)
    }
    b.result()
  }

  /**
    * Combines the entries of `this` Map with the entries of `that` Map that have the same key,
    * tupling their values. `xs.zipByKey(ys)` is a shorthand for `xs.zipByKeyWith(ys)((_, _))`.
    *
    * @param other Other Map
    * @param bf Builder driven by the type of `this` Map
    * @tparam W Type of values of the other Map (e.g. `Int`, `String`)
    * @tparam That Type of the result (e.g. `Map[Int, (String, Boolean)]`, `TreeMap[String, (Int, Int)]`)
    * @return A Map that associates all the keys `k` contained by both `this` and `that` to pairs `(v, w)` where `v`
    *         is the value associated by `this` to `k` and `w` the value associated by `that` to `k`
    */
  def zipByKey[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (map.V, W)), That]): That =
    zipByKeyWith(other)((_, _))

  /** Alias for `zipByKey` */
  @`inline` final def join[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (map.V, W)), That]): That = zipByKey(other)

  /**
    * @return A Map associating all the keys from `this` and `that` with values returned by the partial function
    *         `f`, when this one is defined.
    *
    * @param other Map to merge
    * @param f Combination function
    * @param bf Builder driven by the type of `this` Map
    * @tparam W Type of values of the other Map (e.g. `Int`, `String`)
    * @tparam X Type of values of the resulting Map
    * @tparam That Type of the result (e.g. `Map[Int, (String, Option[Boolean])]`)
    */
  def mergeByKeyWith[W, X, That](other: Map[map.K, W])(f: PartialFunction[(Option[map.V], Option[W]), X])(implicit bf: BuildFrom[C, (map.K, X), That]): That = {
    val b = bf.newBuilder(coll)
    val traversed = mutable.Set.empty[W]
    val pf = f.lift
    for {
      (k, v) <- map(coll)
      x <- pf(other.get(k).fold[(Option[map.V], Option[W])]((Some(v), None)){ w => traversed += w; (Some(v), Some(w)) })
    } {
      b += k -> x
    }
    for {
      (k, w) <- other if !traversed(w)
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
    * @tparam That Type of the resulting collection (e.g. `Map[Int, (Option[Boolean], Option[String])]`)
    * @return A Map that associates all the keys `k` of `this` or `that` to pairs `(Some(v), Some(w))` if `this`
    *         associates `k` to `v` and `that` associates `k` to `w`, or pairs `(None, Some(w))` if `this` doesn’t
    *         contain `k`, or pairs `(Some(v), None)` if `that` doesn’t contain `k`
    */
  @`inline` final def mergeByKey[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], Option[W])), That]): That =
    mergeByKeyWith(other) { case any => any }

  /** Alias for `mergeByKey` */
  @`inline` final def fullOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], Option[W])), That]): That =
    mergeByKey(other)

  /**
    * Perform a left outer join of `this` and `that`.
    *
    * Equivalent to `mergeByKeyWith(that) { case (Some(v), maybeW) => (v, maybeW) }`.
    *
    * @param bf Builder for the resulting collection
    * @tparam W Type of values of `that`
    * @tparam That Type of the resulting collection
    * @return A Map that associates all the keys `k` of `this` to pairs `(v, Some(w))` if `that` associates `k` to `w`,
    *         or `(v, None)` if `that` doesn’t contain `k`
    */
  def leftOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (map.V, Option[W])), That]): That = {
    val b = bf.newBuilder(coll)
    for ((k, v) <- map(coll)) {
      b += k -> (v, other.get(k))
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
    * @tparam That Type of the resulting collection (e.g. `Map[Int, (Option[Boolean], String)]`)
    * @return A Map that associates all the keys `k` of `that` to pairs `(Some(v), w)` if `this` associates `k` to `v`,
    *         or `(None, w)` if `this` doesn’t contain `k`
    */
  def rightOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], W)), That]): That = {
    val b = bf.newBuilder(coll)
    for ((k, w) <- other) {
      b += k -> (map(coll).get(k), w)
    }
    b.result()
  }

}
