package scala.tools.nsc.tasty

import Signature._

sealed trait Signature[+T] { self =>

  final def show: String = mergeShow(new StringBuilder(30)).toString

  final def mergeShow(sb: StringBuilder): StringBuilder = self match {
    case MethodSignature(paramsSig, resSig) =>
      paramsSig.map(_.merge).addString(sb, "(", ",", ")").append(resSig)

    case NotAMethod => sb.append("<nosig>")
  }

  final def map[U](f: T => U): Signature[U] = self match {
    case MethodSignature(paramsSig, resSig) => MethodSignature(paramsSig.map(_.map(f)), f(resSig))
    case NotAMethod                         => NotAMethod
  }

}

object Signature {

  type ParamSig[T] = Either[Int, T]

  def apply[T](paramsSig: List[ParamSig[T]], resSig: T): MethodSignature[T] = new MethodSignature(paramsSig, resSig)

  case object NotAMethod extends Signature[Nothing]
  case class MethodSignature[T] private[Signature] (paramsSig: List[ParamSig[T]], resSig: T) extends Signature[T]
}
