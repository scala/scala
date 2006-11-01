trait A[Ta] { }

class B() extends AnyRef with A[Int] {
  val x : Int = 2
}
