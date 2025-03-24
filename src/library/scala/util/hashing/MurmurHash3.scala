/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util.hashing

import java.lang.Integer.{ rotateLeft => rotl }

private[hashing] class MurmurHash3 {
  /** Mix in a block of data into an intermediate hash value. */
  final def mix(hash: Int, data: Int): Int = {
    var h = mixLast(hash, data)
    h = rotl(h, 13)
    h * 5 + 0xe6546b64
  }

  /** May optionally be used as the last mixing step. Is a little bit faster than mix,
   *  as it does no further mixing of the resulting hash. For the last element this is not
   *  necessary as the hash is thoroughly mixed during finalization anyway. */
  final def mixLast(hash: Int, data: Int): Int = {
    var k = data

    k *= 0xcc9e2d51
    k = rotl(k, 15)
    k *= 0x1b873593

    hash ^ k
  }

  /** Finalize a hash to incorporate the length and make sure all bits avalanche. */
  final def finalizeHash(hash: Int, length: Int): Int = avalanche(hash ^ length)

  /** Force all bits of the hash to avalanche. Used for finalizing the hash. */
  private final def avalanche(hash: Int): Int = {
    var h = hash

    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16

    h
  }

  private[scala] def tuple2Hash(x: Int, y: Int, seed: Int): Int = {
    var h = seed
    h = mix(h, "Tuple2".hashCode)
    h = mix(h, x)
    h = mix(h, y)
    finalizeHash(h, 2)
  }

  /** Compute the hash of a product */
  final def productHash(x: Product, seed: Int, ignorePrefix: Boolean = false): Int = {
    val arr = x.productArity
    // Case objects have the hashCode inlined directly into the
    // synthetic hashCode method, but this method should still give
    // a correct result if passed a case object.
    if (arr == 0) {
      x.productPrefix.hashCode
    } else {
      var h = seed
      if (!ignorePrefix) h = mix(h, x.productPrefix.hashCode)
      var i = 0
      while (i < arr) {
        h = mix(h, x.productElement(i).##)
        i += 1
      }
      finalizeHash(h, arr)
    }
  }

  /** Compute the hash of a string */
  final def stringHash(str: String, seed: Int): Int = {
    var h = seed
    var i = 0
    while (i + 1 < str.length) {
      val data = (str.charAt(i) << 16) + str.charAt(i + 1)
      h = mix(h, data)
      i += 2
    }
    if (i < str.length) h = mixLast(h, str.charAt(i).toInt)
    finalizeHash(h, str.length)
  }

  /** Compute a hash that is symmetric in its arguments - that is a hash
   *  where the order of appearance of elements does not matter.
   *  This is useful for hashing sets, for example.
   */
  final def unorderedHash(xs: IterableOnce[Any], seed: Int): Int = {
    var a, b, n = 0
    var c = 1
    val iterator = xs.iterator
    while (iterator.hasNext) {
      val x = iterator.next()
      val h = x.##
      a += h
      b ^= h
      c *= h | 1
      n += 1
    }
    var h = seed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }

  /** Compute a hash that depends on the order of its arguments. Potential range
    * hashes are recognized to produce a hash that is compatible with rangeHash.
    */
  final def orderedHash(xs: IterableOnce[Any], seed: Int): Int = {
    val it = xs.iterator
    var h = seed
    if(!it.hasNext) return finalizeHash(h, 0)
    val x0 = it.next()
    if(!it.hasNext) return finalizeHash(mix(h, x0.##), 1)
    val x1 = it.next()

    val initial = x0.##
    h = mix(h, initial)
    val h0 = h
    var prev = x1.##
    val rangeDiff = prev - initial
    var i = 2
    while (it.hasNext) {
      h = mix(h, prev)
      val hash = it.next().##
      if(rangeDiff != hash - prev || rangeDiff == 0) {
        h = mix(h, hash)
        i += 1
        while (it.hasNext) {
          h = mix(h, it.next().##)
          i += 1
        }
        return finalizeHash(h, i)
      }
      prev = hash
      i += 1
    }
    avalanche(mix(mix(h0, rangeDiff), prev))

  }

  /** Compute the hash of an array. Potential range hashes are recognized to produce a
    * hash that is compatible with rangeHash.
    */
  final def arrayHash[@specialized T](a: Array[T], seed: Int): Int = {
    var h = seed
    val l = a.length
    l match {
      case 0 =>
        finalizeHash(h, 0)
      case 1 =>
        finalizeHash(mix(h, a(0).##), 1)
      case _ =>
        val initial = a(0).##
        h = mix(h, initial)
        val h0 = h
        var prev = a(1).##
        val rangeDiff = prev - initial
        var i = 2
        while (i < l) {
          h = mix(h, prev)
          val hash = a(i).##
          if(rangeDiff != hash - prev || rangeDiff == 0) {
            h = mix(h, hash)
            i += 1
            while (i < l) {
              h = mix(h, a(i).##)
              i += 1
            }
            return finalizeHash(h, l)
          }
          prev = hash
          i += 1
        }
        avalanche(mix(mix(h0, rangeDiff), prev))
    }
  }

  /** Compute the hash of a Range with at least 2 elements. Ranges with fewer
    * elements need to use seqHash instead. The `last` parameter must be the
    * actual last element produced by a Range, not the nominal `end`.
    */
  final def rangeHash(start: Int, step: Int, last: Int, seed: Int): Int =
    avalanche(mix(mix(mix(seed, start), step), last))

  /** Compute the hash of a byte array. Faster than arrayHash, because
   *  it hashes 4 bytes at once. Note that the result is not compatible with
   *  arrayHash!
   */
  final def bytesHash(data: Array[Byte], seed: Int): Int = {
    var len = data.length
    var h = seed

    // Body
    var i = 0
    while(len >= 4) {
      var k = data(i + 0) & 0xFF
      k |= (data(i + 1) & 0xFF) << 8
      k |= (data(i + 2) & 0xFF) << 16
      k |= (data(i + 3) & 0xFF) << 24

      h = mix(h, k)

      i += 4
      len -= 4
    }

    // Tail
    var k = 0
    if(len == 3) k ^= (data(i + 2) & 0xFF) << 16
    if(len >= 2) k ^= (data(i + 1) & 0xFF) << 8
    if(len >= 1) {
      k ^= (data(i + 0) & 0xFF)
      h = mixLast(h, k)
    }

    // Finalization
    finalizeHash(h, data.length)
  }

  /** Compute the hash of an IndexedSeq. Potential range hashes are recognized to produce a
    * hash that is compatible with rangeHash.
    */
  final def indexedSeqHash(a: scala.collection.IndexedSeq[Any], seed: Int): Int = {
    var h = seed
    val l = a.length
    l match {
      case 0 =>
        finalizeHash(h, 0)
      case 1 =>
        finalizeHash(mix(h, a(0).##), 1)
      case _ =>
        val initial = a(0).##
        h = mix(h, initial)
        val h0 = h
        var prev = a(1).##
        val rangeDiff = prev - initial
        var i = 2
        while (i < l) {
          h = mix(h, prev)
          val hash = a(i).##
          if(rangeDiff != hash - prev || rangeDiff == 0) {
            h = mix(h, hash)
            i += 1
            while (i < l) {
              h = mix(h, a(i).##)
              i += 1
            }
            return finalizeHash(h, l)
          }
          prev = hash
          i += 1
        }
        avalanche(mix(mix(h0, rangeDiff), prev))
    }
  }

  /** Compute the hash of a List. Potential range hashes are recognized to produce a
    * hash that is compatible with rangeHash.
    */
  final def listHash(xs: scala.collection.immutable.List[_], seed: Int): Int = {
    var n = 0
    var h = seed
    var rangeState = 0 // 0 = no data, 1 = first elem read, 2 = has valid diff, 3 = invalid
    var rangeDiff = 0
    var prev = 0
    var initial = 0
    var elems = xs
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      val hash = head.##
      h = mix(h, hash)
      rangeState match {
        case 0 =>
          initial = hash
          rangeState = 1
        case 1 =>
          rangeDiff = hash - prev
          rangeState = 2
        case 2 =>
          if(rangeDiff != hash - prev || rangeDiff == 0) rangeState = 3
        case _ =>
      }
      prev = hash
      n += 1
      elems = tail
    }
    if(rangeState == 2) rangeHash(initial, rangeDiff, prev, seed)
    else finalizeHash(h, n)
  }
}

/**
 * An implementation of Austin Appleby's MurmurHash 3 algorithm
 * (MurmurHash3_x86_32). This object contains methods that hash
 * values of various types as well as means to construct `Hashing`
 * objects.
 *
 * This algorithm is designed to generate well-distributed non-cryptographic
 * hashes. It is designed to hash data in 32 bit chunks (ints).
 *
 * The mix method needs to be called at each step to update the intermediate
 * hash value. For the last chunk to incorporate into the hash mixLast may
 * be used instead, which is slightly faster. Finally finalizeHash needs to
 * be called to compute the final hash value.
 *
 * This is based on the earlier MurmurHash3 code by Rex Kerr, but the
 * MurmurHash3 algorithm was since changed by its creator Austin Appleby
 * to remedy some weaknesses and improve performance. This represents the
 * latest and supposedly final version of the algorithm (revision 136). Even
 * so, test the generated hashes in between Scala versions, even for point
 * releases, as fast, non-cryptographic hashing algorithms evolve rapidly.
 *
 * @see [[https://github.com/aappleby/smhasher]]
 */
object MurmurHash3 extends MurmurHash3 {
  final val arraySeed       = 0x3c074a61
  final val stringSeed      = 0xf7ca7fd2
  final val productSeed     = 0xcafebabe
  final val symmetricSeed   = 0xb592f7ae
  final val traversableSeed = 0xe73a8b15
  final val seqSeed         = "Seq".hashCode
  final val mapSeed         = "Map".hashCode
  final val setSeed         = "Set".hashCode

  def arrayHash[@specialized T](a: Array[T]): Int = arrayHash(a, arraySeed)
  def bytesHash(data: Array[Byte]): Int           = bytesHash(data, arraySeed)
  def orderedHash(xs: IterableOnce[Any]): Int     = orderedHash(xs, symmetricSeed)
  def productHash(x: Product): Int                = productHash(x, productSeed)
  def stringHash(x: String): Int                  = stringHash(x, stringSeed)
  def unorderedHash(xs: IterableOnce[Any]): Int   = unorderedHash(xs, traversableSeed)
  def rangeHash(start: Int, step: Int, last: Int): Int = rangeHash(start, step, last, seqSeed)

  private[scala] def arraySeqHash[@specialized T](a: Array[T]): Int = arrayHash(a, seqSeed)
  private[scala] def tuple2Hash(x: Any, y: Any): Int = tuple2Hash(x.##, y.##, productSeed)

  /** To offer some potential for optimization.
   */
  def seqHash(xs: scala.collection.Seq[_]): Int    = xs match {
    case xs: scala.collection.IndexedSeq[_] => indexedSeqHash(xs, seqSeed)
    case xs: List[_] => listHash(xs, seqSeed)
    case xs => orderedHash(xs, seqSeed)
  }

  def mapHash(xs: scala.collection.Map[_, _]): Int = {
    if (xs.isEmpty) emptyMapHash
    else {
      class accum extends Function2[Any, Any, Unit] {
        var a, b, n = 0
        var c = 1
        override def apply(k: Any, v: Any): Unit = {
          val h = tuple2Hash(k, v)
          a += h
          b ^= h
          c *= h | 1
          n += 1
        }
      }
      val accum = new accum
      var h = mapSeed
      xs.foreachEntry(accum)
      h = mix(h, accum.a)
      h = mix(h, accum.b)
      h = mixLast(h, accum.c)
      finalizeHash(h, accum.n)
    }
  }

  private[scala] val emptyMapHash = unorderedHash(Nil, mapSeed)
  def setHash(xs: scala.collection.Set[_]): Int    = unorderedHash(xs, setSeed)

  class ArrayHashing[@specialized T] extends Hashing[Array[T]] {
    def hash(a: Array[T]) = arrayHash(a)
  }

  def arrayHashing[@specialized T] = new ArrayHashing[T]

  def bytesHashing = new Hashing[Array[Byte]] {
    def hash(data: Array[Byte]) = bytesHash(data)
  }

  def orderedHashing = new Hashing[IterableOnce[Any]] {
    def hash(xs: IterableOnce[Any]) = orderedHash(xs)
  }

  def productHashing = new Hashing[Product] {
    def hash(x: Product) = productHash(x)
  }

  def stringHashing = new Hashing[String] {
    def hash(x: String) = stringHash(x)
  }

  def unorderedHashing = new Hashing[IterableOnce[Any]] {
    def hash(xs: IterableOnce[Any]) = unorderedHash(xs)
  }

//  /** All this trouble and foreach still appears faster.
//   *  Leaving in place in case someone would like to investigate further.
//   */
//  def linearSeqHash(xs: scala.collection.LinearSeq[_], seed: Int): Int = {
//    var n = 0
//    var h = seed
//    var elems = xs
//    while (elems.nonEmpty) {
//      h = mix(h, elems.head.##)
//      n += 1
//      elems = elems.tail
//    }
//    finalizeHash(h, n)
//  }
}
