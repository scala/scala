trait A[Ta] { }

class B() extends Object with A[Int] with {
  val x : Int = 2
}