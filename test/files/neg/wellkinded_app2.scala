// test well-kindedness checks
class WellKinded[s <: Throwable] {
      val foo: s[Int]
}
