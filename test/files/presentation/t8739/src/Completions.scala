
package wedens

object t8739 {
  implicit class OptionW[T](opt: Option[T]) {
    def cata[A](some: T => A, none: A) = opt.map(some) getOrElse none
  }
  Option("").cata(x => x./*!*/)
}
