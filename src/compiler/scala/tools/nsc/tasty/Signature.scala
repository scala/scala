package scala.tools.nsc.tasty

import scala.reflect.internal.SymbolTable
import scala.tools.nsc.tasty.Signature.ParamSig

sealed trait Signature[+T] {
  def show: String
}

object Signature {
  type ParamSig[T] = Either[Int, T]

  def apply[T](paramsSig: List[ParamSig[T]], resSig: T): MethodSignature[T] = new MethodSignature(paramsSig, resSig)

  case object NotAMethod extends Signature[Nothing] {
    def show: String = "<nosig>"
  }
  case class MethodSignature[T](paramsSig: List[ParamSig[T]], resSig: T) extends Signature[T] {
    def show: String = s"""${paramsSig.map(_.merge).mkString("(", ", ", ")")}$resSig"""
  }
}
