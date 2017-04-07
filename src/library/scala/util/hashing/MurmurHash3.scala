/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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

  /** Compute the hash of a product */
  final def productHash(x: Product, seed: Int): Int = {
    val arr = x.productArity
    // Case objects have the hashCode inlined directly into the
    // synthetic hashCode method, but this method should still give
    // a correct result if passed a case object.
    if (arr == 0) {
      x.productPrefix.hashCode
    }
    else {
      var h = seed
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
  final def unorderedHash(xs: TraversableOnce[Any], seed: Int): Int = {
    var a, b, n = 0
    var c = 1
    xs foreach { x =>
      val h = x.##
      a += h
      b ^= h
      if (h != 0) c *= h
      n += 1
    }
    var h = seed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
  /** Compute a hash that depends on the order of its arguments.
   */
  final def orderedHash(xs: TraversableOnce[Any], seed: Int): Int = {
    var n = 0
    var h = seed
    xs foreach { x =>
      h = mix(h, x.##)
      n += 1
    }
    finalizeHash(h, n)
  }

  /** Compute the hash of an array.
   */
  final def arrayHash[@specialized T](a: Array[T], seed: Int): Int = {
    var h = seed
    var i = 0
    while (i < a.length) {
      h = mix(h, a(i).##)
      i += 1
    }
    finalizeHash(h, a.length)
  }

  /** Compute the hash of a byte array. Faster than arrayHash, because
   *  it hashes 4 bytes at once.
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

  final def listHash(xs: scala.collection.immutable.List[_], seed: Int): Int = {
    var n = 0
    var h = seed
    var elems = xs
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      h = mix(h, head.##)
      n += 1
      elems = tail
    }
    finalizeHash(h, n)
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
 * latest and supposedly final version of the algorithm (revision 136).
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

  def arrayHash[@specialized T](a: Array[T]): Int  = arrayHash(a, arraySeed)
  def bytesHash(data: Array[Byte]): Int            = bytesHash(data, arraySeed)
  def orderedHash(xs: TraversableOnce[Any]): Int   = orderedHash(xs, symmetricSeed)
  def productHash(x: Product): Int                 = productHash(x, productSeed)
  def stringHash(x: String): Int                   = stringHash(x, stringSeed)
  def unorderedHash(xs: TraversableOnce[Any]): Int = unorderedHash(xs, traversableSeed)

  private[scala] def wrappedArrayHash[@specialized T](a: Array[T]): Int  = arrayHash(a, seqSeed)
  private[scala] def wrappedBytesHash(data: Array[Byte]): Int            = bytesHash(data, seqSeed)

  /** To offer some potential for optimization.
   */
  def seqHash(xs: scala.collection.Seq[_]): Int    = xs match {
    case xs: List[_] => listHash(xs, seqSeed)
    case xs => orderedHash(xs, seqSeed)
  }

  def mapHash(xs: scala.collection.Map[_, _]): Int = unorderedHash(xs, mapSeed)
  def setHash(xs: scala.collection.Set[_]): Int    = unorderedHash(xs, setSeed)

  class ArrayHashing[@specialized T] extends Hashing[Array[T]] {
    def hash(a: Array[T]) = arrayHash(a)
  }

  def arrayHashing[@specialized T] = new ArrayHashing[T]

  def bytesHashing = new Hashing[Array[Byte]] {
    def hash(data: Array[Byte]) = bytesHash(data)
  }

  def orderedHashing = new Hashing[TraversableOnce[Any]] {
    def hash(xs: TraversableOnce[Any]) = orderedHash(xs)
  }

  def productHashing = new Hashing[Product] {
    def hash(x: Product) = productHash(x)
  }

  def stringHashing = new Hashing[String] {
    def hash(x: String) = stringHash(x)
  }

  def unorderedHashing = new Hashing[TraversableOnce[Any]] {
    def hash(xs: TraversableOnce[Any]) = unorderedHash(xs)
  }

  /** All this trouble and foreach still appears faster.
   *  Leaving in place in case someone would like to investigate further.
   */
  /**
  def linearSeqHash(xs: scala.collection.LinearSeq[_], seed: Int): Int = {
    var n = 0
    var h = seed
    var elems = xs
    while (elems.nonEmpty) {
      h = mix(h, elems.head.##)
      n += 1
      elems = elems.tail
    }
    finalizeHash(h, n)
  }

  def indexedSeqHash(xs: scala.collection.IndexedSeq[_], seed: Int): Int = {
    var n = 0
    var h = seed
    val len = xs.length
    while (n < len) {
      h = mix(h, xs(n).##)
      n += 1
    }
    finalizeHash(h, n)
  }
  */
}
