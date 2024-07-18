
//> using options -Werror -Xlint

object Test {
  def doesntWarn(input: String) = input match {
    case "a" =>
    case "b" =>
  }
  def warnsNoLonger(input: String) = input match {
    case "c" =>
  }
  def control(input: T) = input match {
    case _: A =>
  }
  def any(input: Any) = input match {
    case "a" =>
    case 42  =>
    case "b" =>
    case "c" =>
  }
}

sealed trait T
final class A extends T
final class B extends T
