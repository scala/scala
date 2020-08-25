// scalac: -Ydelambdafy:method-ref
case object Test {
  def f0(f: Function0[String])      = ()
  def f1(f: Function1[Any, String]) = ()

  def main(args: Array[String]): Unit = {
    f0(() => toString())
    f1(_.toString())
  }
}
