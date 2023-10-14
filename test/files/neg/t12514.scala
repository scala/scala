import scala.language.existentials

object Test {
  trait T {
    trait Y {
      def x = 0
    }
  }

  val y = new (t.type forSome { val t: T })#Y {}
}