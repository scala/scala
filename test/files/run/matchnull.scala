object Test
{
  def f1 = null match { case x: AnyRef => 1 case _ => -1 }
  def f2(x: Any) = x match { case 52 => 1 ; case null => -1 ; case _ => 0 }
  def f3(x: AnyRef) = x match { case x: String => 1 ; case List(_) => 0 ; case null => -1 ; case _ => -2 }

  def main(args: Array[String]): Unit = {
    println(f1)
    println(f2(null))
    println(f3(null))
  }
}
