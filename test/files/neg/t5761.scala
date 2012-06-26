class D[-A](x: A) { }

object Test1 {
  println(new D[Int]{})   // crash
}

object Test2 {
  println(new D[Int]())   // no crash
  println(new D[Int]{})   // crash
}

object Test3 {
  new Tread("sth") { }.run()
}


