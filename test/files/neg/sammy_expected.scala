trait F[A, B] { def apply(x: A): B }

class MustMeetExpected {
  def wrong: F[Object, Int] = (x: String) => 1
}