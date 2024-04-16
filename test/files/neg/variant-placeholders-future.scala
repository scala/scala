//> using options -Xsource:3
//
object Test {
  type -_ = Int // error -_ not allowed as a type def name without backticks
  type +_ = Int // error +_ not allowed as a type def name without backticks
}
