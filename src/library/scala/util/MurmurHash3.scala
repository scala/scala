package scala.util

import java.lang.Integer.{ rotateLeft => rotl }

/**
 * An implementation of Austin Appleby's MurmurHash 3 algorithm
 * (MurmurHash3_x86_32).
 *
 * An algorithm designed to generate well-distributed non-cryptographic
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
 * latest and supposedly final version of the algortihm (revision 136).
 *
 * @see http://code.google.com/p/smhasher
 */
class MurmurHash3 {
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
    if (i < str.length) h = mixLast(h, str.charAt(i))
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
}

/**
 * An instance of MurmurHash3 with predefined seeds for various
 * classes.  Used by all the scala collections and case classes.
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

  /** To offer some potential for optimization.
   */
  def seqHash(xs: collection.Seq[_]): Int    = orderedHash(xs, seqSeed)
  def mapHash(xs: collection.Map[_, _]): Int = unorderedHash(xs, mapSeed)
  def setHash(xs: collection.Set[_]): Int    = unorderedHash(xs, setSeed)

  /** All this trouble and foreach still appears faster.
   *  Leaving in place in case someone would like to investigate further.
   */
  /**
  def linearSeqHash(xs: collection.LinearSeq[_], seed: Int): Int = {
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

  def indexedSeqHash(xs: collection.IndexedSeq[_], seed: Int): Int = {
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

  @deprecated("Use unorderedHash", "2.10.0")
  final def symmetricHash[T](xs: collection.GenTraversableOnce[T], seed: Int = symmetricSeed): Int =
    unorderedHash(xs.seq, seed)

  @deprecated("Use orderedHash", "2.10.0")
  final def traversableHash[T](xs: collection.GenTraversableOnce[T], seed: Int = traversableSeed): Int =
    orderedHash(xs.seq, seed)
}
