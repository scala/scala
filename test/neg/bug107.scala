trait C {
  def foo: Int;
}
object O extends C {
  def main(args: Array[String]) = {
    System.out.println(foo);
  }
}
