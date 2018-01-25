

trait T {

  def f = List(1) map { case i if i > 0 => implicit j: Int => i + implicitly[Int]  case _ => implicit j: Int => 42 }

  def g = List(1) map { case i if i > 0 => import concurrent._  case _ => implicit j: Int => 42 }

  def h = List(1) map { case i if i > 0 => val x = 42  case _ => implicit j: Int => () }

  // separator is optional
  def k = List(1) map { case i if i > 0 => implicit j: Int => i + implicitly[Int] ; case _ => implicit j: Int => 42 }
}
