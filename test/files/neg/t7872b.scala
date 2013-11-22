object coinv {
  def up[F[+_]](fa: F[String]): F[Object] = fa
  def down[F[-_]](fa: F[Object]): F[String] = fa
 
  up(List("hi"))
 
  // should not compile; `l' is unsound
  def oops1 = down[({type l[-a] = List[a]})#l](List('whatever: Object)).head + "oops"
  // scala> oops1
  // java.lang.ClassCastException: scala.Symbol cannot be cast to java.lang.String
  //         at com.nocandysw.coinv$.oops1(coinv.scala:12)
 
  type Stringer[-A] = A => String
  down[Stringer](_.toString)
  // [error] type A is contravariant, but type _ is declared covariant
  // up[Stringer]("printed: " + _)
 
  // should not compile; `l' is unsound
  def oops2 = up[({type l[+a] = Stringer[a]})#l]("printed: " + _)
  // scala> oops2(Some(33))
  // java.lang.ClassCastException: scala.Some cannot be cast to java.lang.String
  //         at com.nocandysw.coinv$$anonfun$oops2$1.apply(coinv.scala:20)
}
