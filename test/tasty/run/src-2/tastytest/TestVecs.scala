package tastytest

/** test references to type parameters in an opaque type alias */
object TestVecs extends Suite("TestVecs") {
  import Vecs._, Vec._

  test {
    val v = Vec.single("Hello")
    assert(v.safeHead === "Hello")
  }

}
