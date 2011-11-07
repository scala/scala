trait T[A] {
  def f(): A
}
class C extends T[Char] {
  def f(): Char = 'a'
}

object Test {
  val c1: Class[_] = classOf[T[_]]
  val c2: Class[_] = classOf[C]
  
  val c1m = c1.getMethods.toList filter (_.getName == "f") map (_.getGenericReturnType.toString)
  val c2m = c2.getMethods.toList filter (_.getName == "f") map (_.getGenericReturnType.toString)

  def main(args: Array[String]): Unit = {
    println(c2.getGenericInterfaces.map(_.toString).sorted mkString " ")
    println(c1m ++ c2m sorted)
    println(new C f)
  }
}
