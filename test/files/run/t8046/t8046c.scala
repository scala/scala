import language._

trait One {
  type Op[A]
  type Alias[A] = Op[A]
}

trait Three extends One {
  trait Op[A] extends (A => A)

  def f4[T <: Alias[Int]](f: T) = 0
}

