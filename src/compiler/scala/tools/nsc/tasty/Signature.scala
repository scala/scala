package scala.tools.nsc.tasty

sealed abstract class Signature[+T] { self =>
  import Signature._

  final def show: String = mergeShow(new StringBuilder(30)).toString

  final def mergeShow(sb: StringBuilder): StringBuilder = self match {
    case MethodSignature(params, result) =>
      params.map(_.merge).addString(sb, "(", ",", ")").append(result)
  }

}

object Signature {

  type ParamSig[T] = Either[Int, T]

  def apply[T](params: List[ParamSig[T]], result: T): MethodSignature[T] = new MethodSignature(params, result)

  case class MethodSignature[T] private[Signature](params: List[ParamSig[T]], result: T) extends Signature[T] {
    def map[U](f: T => U): MethodSignature[U] = this match {
      case MethodSignature(params, result) => MethodSignature(params.map(_.map(f)), f(result))
    }
  }

}
