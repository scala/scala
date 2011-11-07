object Test {
  def f1 = ("": Any) match { case List(x @ _*) => x ; case _ => false }
  def f2 = (5: Any) match { case List(x @ _*) => x ; case _ => false }
  def f3 = (Nil: Any) match { case List(x @ _*) => x ; case _ => false }
  def f4 = (Array(1): Any) match { case List(x @ _*) => x ; case _ => false }
  
  def f5 = ("": Any) match { case Array(x @ _*) => x ; case _ => false }
  def f6 = (5: Any) match { case Array(x @ _*) => x ; case _ => false }
  def f7 = (Nil: Any) match { case Array(x @ _*) => x ; case _ => false }
  def f8 = (Array(1): Any) match { case Array(x @ _*) => x ; case _ => false }
  
  def f9 = ("": Any) match { case x @ List(_*) => x ; case _ => false }
  def f10 = ("": Any) match { case List(_*) => true ; case _ => false }
  def f11 = (Nil: Any) match { case List(_*) => true ; case _ => false }
  def f12 = ("": Any) match { case x @ Array(_*) => x ; case _ => false }
  def f13 = ("": Any) match { case Array(_*) => true ; case _ => false }
  def f14 = (Nil: Any) match { case Array(_*) => true ; case _ => false }
  
  
  def main(args: Array[String]): Unit = {
    println(f1)
    println(f2)
    println(f3)
    println(f4)
    println(f5)
    println(f6)
    println(f7)
    println(f8)
    println(f9)
    println(f10)
    println(f11)
    println(f12)
    println(f13)
    println(f14)
  }
}
