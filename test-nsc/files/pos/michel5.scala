trait A[Ta] { }

class B() extends Object with A[Int] {
  val x : Int = 2
}