object Test {

  def foo[t](fun: Function0[t]): t = fun();

  def bar(x: Int): Unit = {
    foo(() => 0);
    ()
  }

  def main(args: Array[String]): Unit = bar(32);

}

