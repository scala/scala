//package scala.tools.nsc.tasty
//
//object TastyHash {
//
//  /** Returns a non-cryptographic 64-bit hash of the array.
//   *
//   *  from https://en.wikipedia.org/wiki/PJW_hash_function#Algorithm
//   */
//  def pjwHash64(data: Array[Byte]): Long = {
//    var h = 0L
//    var i = 0
//    while (i < data.length) {
//      val d = data(i) & 0xFFL // Interpret byte as unsigned byte
//      h = (h << 8) + d
//      val high = h & 0xFF00000000000000L
//      h ^= high >>> 48L
//      h &= ~high
//      i += 1
//    }
//    h
//  }
//
//}
