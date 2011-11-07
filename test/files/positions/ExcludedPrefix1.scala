object ExcludedPrefix1 {
  case
  class Foo

  case
  object
  BLAH
  
  val
  a = 1
  
  var
  b = 2
  
  def
  c = 23
  
  private
  def
  d = 23

  lazy
  val
  e = 23
  
  private
  type
  f = Int
  
  val
  g,
  h = 23
  
  val
  (i,
   j) = (0, 0)
   
  val Pair(
   k,
   l) = (0, 0)
}
