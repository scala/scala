object Test extends App
{
  val a1 = Array(0,1,2,3).toArray[Any]
//  val a1 = x1.toArray[Any]
  val a2 = Array('a','b','c','d').toArray[Any]
  val a3 = Array("e","f","g","h").toArray[Any]
  
  Array.copy(a3, 0, a1, 0, 4)
  Array.copy(a2, 0, a3, 0, 4)
  Array.copy(a2, 0, a1, 0, 4)
  
  println(a1.mkString + a2.mkString + a3.mkString)
}
