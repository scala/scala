class A[K] {
  def receive: PartialFunction[Any, Unit] = {
    case ds: List[Double] =>  // unchecked warning
      println("* List[Double]")
    case kx: Vector[K] =>      // no unchecked warning
      println("* Vector[K]")
    case ak: A[K] => 
      println("* A[K]")
  }
}
