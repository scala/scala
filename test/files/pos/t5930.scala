//> using options -Ywarn-dead-code -Xfatal-warnings
//
// should not warn about dead code (`matchEnd(throw new MatchError)`)
 class Test {
  0 match { case x: Int => }
}
