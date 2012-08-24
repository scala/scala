/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.io

/**
 *  @author  Martin Odersky
 *  @version 1.0, 04/10/2004
 */
@deprecated("This class will be removed.", "2.10.0")
object UTF8Codec {
  final val UNI_REPLACEMENT_CHAR: Int = 0x0000FFFD
  final val UNI_REPLACEMENT_BYTES = Array[Byte](-17, -65, -67)

  // Note, from http://unicode.org/faq/utf_bom.html#utf8-5
  //
  // A different issue arises if an unpaired surrogate is encountered when converting
  // ill-formed UTF-16 data. By represented such an unpaired surrogate on its own as a
  // 3-byte sequence, the resulting UTF-8 data stream would become ill-formed.
  // While it faithfully reflects the nature of the input, Unicode conformance
  // requires that encoding form conversion always results in valid data stream.
  // Therefore a converter must treat this as an error.
  //
  // Some useful locations:
  //    http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
}
