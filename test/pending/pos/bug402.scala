object Main {
  def main(args: Array[String]): Unit = {
    val x: Any = 2
    System.out.println((0.5).isInstanceOf[Int]);
    System.out.println(x.isInstanceOf[Int]);
  }
}
