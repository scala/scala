// a.scala
// Sat Jun 30 19:51:17 PDT 2012

trait Exp[T]

object Test {
  def f[T](exp: Exp[T]): Exp[T] = (
    f[T] _
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]      // 4s
      compose f[T]      // 5s
      compose f[T]      // 5s
      compose f[T]      // 6s
      compose f[T]      // 7s
      compose f[T]      // 8s
      compose f[T]      // 11s
      compose f[T]      // 17s
      compose f[T]      // 29s
      compose f[T]      // 54s
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
      compose f[T]
    )(exp)
}
