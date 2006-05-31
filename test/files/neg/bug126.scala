class O {
  val Bar:Any => Any = ((x:Any) => Bar(x));
  val Tuple2(foo:(Any => Any), bar) = Tuple2((x:Any) => foo(x), 1);
  {
    val Tuple1(foo2:(Any => Any)) = Tuple1((x:Any) => foo2(x));
    Console.println(foo2)
  }
}
