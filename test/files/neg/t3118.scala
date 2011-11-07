class O1 {
  private[this] case class C()
  
  val x = new O1
  
  println(x.C())   // should not be accessible
  println(new x.C) // is correctly not accessible
}
