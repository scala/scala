object Test extends App{
  def matchesNull[T](mightBeNull: Array[T]): Boolean = mightBeNull match { 
    case null => true
    case x => false
  }

  val nullArray: Array[String] = null
  println(matchesNull(nullArray)) 
}

