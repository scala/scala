object O
{
  def d(t: Top) = t match {
    case s: Sub => true
    case _ => false
  }

  def main(args: Array[String]): Unit = {
    val c = new AnyRef with C

    c.bob.toString + c.bob2.toString
  }
}
