// test well-kindedness checks
abstract class WellKinded[s <: Throwable] {
      val foo: s[Int]
}
