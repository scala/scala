package scala.tools.nsc.tasty

import scala.reflect.internal.SymbolTable
import scala.tools.nsc.tasty.Signature.ParamSig

object Signature {
  type ParamSig = Either[Int, SymbolTable#TypeName]
}

case class Signature(paramsSig: List[ParamSig], resSig: SymbolTable#TypeName)