import language.higherKinds

trait Tycon[A]
trait MatcherFactory1X[TC1[_]]
trait Matcher[T]{
  class X {
        def be = {
      def inferTycon[TC1[_]](other: MatcherFactory1X[TC1]): MatcherFactory1X[TC1] = ???
          inferTycon(??? : MatcherFactory1X[Tycon])
        }
  }
}
