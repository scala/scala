object Bug460 {
  def testFun(x : Int, y : Int) = x + y
  val fn = testFun _ 
  
  fn(1, 2) // Ok 
  (testFun(_, _))(1, 2) // Ok
  (testFun _).apply(1, 2)
  (testFun _)(1, 2) // Error! (but no longer)
}
