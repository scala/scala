object Test {
  object O1 { implicit def f(s: String): Int = 1 }
  object O2 { implicit def f(s: String): Int = 2 }
  object O3 {          def f(s: String): Int = 3 }

  // Import two implicits with the same name in the same scope.
  def m1 = {
    import O1._
    import O2._

    // Implicit usage compiles.
    "a": Int
  }

  // Import one implict and one non-implicit method with the
  // same name in the same scope.
  def m2 = {
    import O1._
    import O3._

    // Implicit usage compiles.
    "a": Int
  }
}