package test

object NotEnclosing {
  def main(args : Array[String]) : Unit = {}
  def compare[T](x: Ordered[T], y: Ordered[T]) = error("")
  def mkEx: Ordered[_] = error("")                        
  compare(mkEx, mkEx)      
}
