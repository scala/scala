trait One {
  type Op[A]
  type Alias[A] = Op[A]
}

trait Three extends One {
  trait Op[A] extends (A => A)

  def f1(f: Op[Int])            = f(5)
  def f2(f: Alias[Int])         = f(5)
  def f3[T <: Op[Int]](f: T)    = f(5)
  def f4[T <: Alias[Int]](f: T) = f(5)
  // ./a.scala:12: error: type mismatch;
  //  found   : Int(5)
  //  required: T1
  //   def f4[T <: Alias[Int]](f: T) = f(5)
  //                                     ^
}

