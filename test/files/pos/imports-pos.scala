package test;

import java.lang.{System => S}

object test {
  import S.out.{print => p, println => print}

  val foo = 1;

  p("hello"); print("world"); S.out.println("!"); 
  S.out.flush();
}
object test1 {
  import test._;
  foo
} 
