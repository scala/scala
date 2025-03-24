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

package scala.tools.tasty

/** Represents the structure of an uncurried Scala method signature */
sealed abstract class Signature[+T] { self =>
  import Signature._

  final def show: String = mergeShow(new StringBuilder(30)).toString

  final def mergeShow(sb: StringBuilder): StringBuilder = self match {
    case MethodSignature(params, result) =>
      params.map(_.merge).addString(sb, "(", ",", ")").append(result)
  }

}

object Signature {

  /** Encodes either an `Int` which is the size of a type parameter list, or `T`, which represents a type */
  type ParamSig[T] = Either[Int, T]

  def merge[T](sb: StringBuilder, sig: Signature[T]): StringBuilder = sig.mergeShow(sb)

  def apply[T](params: List[ParamSig[T]], result: T): MethodSignature[T] = new MethodSignature(params, result)

  /** Encodes the structure of an uncurried Scala method signature, with generic type parameter lists erased to just
   *  their size and position.
    * @param params represents types of method parameters interspersed by the lengths of generic type parameter lists
    * @param result represents the type of the method result
    */
  case class MethodSignature[T] private[Signature](params: List[ParamSig[T]], result: T) extends Signature[T] {
    def map[U](f: T => U): MethodSignature[U] = MethodSignature(params.map(_.map(f)), f(result))
  }

}
