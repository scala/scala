object Test {
  trait T { trait U { val x = 3 } }
  val x = new AnyRef with T#U { }
}
