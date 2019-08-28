//package scala.tools.nsc.tasty
//
//import java.io._
//import java.util.Base64
//import java.nio.charset.StandardCharsets.UTF_8
//
//import scala.runtime.quoted.Unpickler.PickledQuote
//
///** Utils for String representation of TASTY */
//object TastyString {
//
//  // Max size of a string literal in the bytecode
//  private final val maxStringSize = 65535
//
//  /** Encode TASTY bytes into a List of String */
//  def pickle(bytes: Array[Byte]): PickledQuote = {
//    val str = new String(Base64.getEncoder().encode(bytes), UTF_8)
//    str.toSeq.sliding(maxStringSize, maxStringSize).map(_.unwrap).toList
//  }
//
//  /** Decode the List of Strings into TASTY bytes */
//  def unpickle(strings: PickledQuote): Array[Byte] = {
//    val string = new StringBuilder
//    strings.foreach(string.append)
//    Base64.getDecoder().decode(string.result().getBytes(UTF_8))
//  }
//
//}
