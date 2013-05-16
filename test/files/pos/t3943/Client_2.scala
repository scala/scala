object Test {
  val x: Child = new Child
  x.getInner.foo("meh")
// error: type mismatch;
//  found   : java.lang.String("meh")
//  required: E
}
