case class C();

object arrays2 {

  def main(args: Array[String]): Unit = {
    val a: Array[Array[C]] = new Array[Array[C]](2);
    a(0) = new Array[C](2);
    a(0)(0) = new C();
  }
}

