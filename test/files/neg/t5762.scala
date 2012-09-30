class D[-A]

object Test {
  var bippy: Boolean = true
  def f1(x: D[Int with String]) = x match {
    case _: D[Int]    if bippy => 1
    case _: D[String]          => 2
  }
  // Correctly warns:
  //
  // a.scala:5: warning: non variable type-argument Int in type pattern D[Int] is unchecked since it is eliminated by erasure
  //     case _: D[Int]    => 1
  //             ^
  // a.scala:6: warning: non variable type-argument String in type pattern D[String] is unchecked since it is eliminated by erasure
  //     case _: D[String] => 2
  //             ^
  // two warnings found

  def f2(x: D[D[Int] with D[String]]) = x match {
    case _: D[D[Int]]    if bippy => 1
    case _: D[D[String]]          => 2
  }
  // No warnings!
}
