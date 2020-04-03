package scala.tools.nsc.tasty

import annotation.unchecked.uncheckedVariance

sealed trait Signature[+T] { self =>
  import Signature._

  final def show: String = mergeShow(new StringBuilder(30)).toString

  final def mergeShow(sb: StringBuilder): StringBuilder = self match {
    case MethodSignature(params, result) =>
      params.map(_.merge).addString(sb, "(", ",", ")").append(result)

    case NotAMethod => sb.append("<nosig>")
  }

  final def map[U](f: T => U): Signature[U] = self match {
    case MethodSignature(params, result) => MethodSignature(params.map(_.map(f)), f(result))
    case NotAMethod                      => NotAMethod
  }

  final def asMethod: MethodSignature[T @uncheckedVariance] = self.asInstanceOf[MethodSignature[T]]

}

object Signature {

  type ParamSig[T] = Either[Int, T]

  def apply[T](params: List[ParamSig[T]], result: T): MethodSignature[T] = new MethodSignature(params, result)

  case object NotAMethod extends Signature[Nothing]
  case class MethodSignature[T] private[Signature](params: List[ParamSig[T]], result: T) extends Signature[T]

}
