object Append extends Application {

  def append[A](l1: List[A], l2: List[A]):List[A] =
    l1 match {
      case Nil => l2
      case x::xs => x :: append(xs, l2)
    }

  println(scala.reflect.Code.lift(append _).tree)
}

