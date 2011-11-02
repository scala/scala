object Test {
  trait T[M[_]]
  
  def f1 = classOf[T[X] forSome { type X[_] } ]
  
  def main(args: Array[String]): Unit = println(f1)
}
