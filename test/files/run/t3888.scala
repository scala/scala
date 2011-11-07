object Test {

  val T1 = new P
  private[this] val T2 = T1

  def m1 = 
    (1, 2) match {
      case T1 => true
      case _ => false
    }

  def m2 = 
    (1, 2) match {
      case T2 => true
      case _ => false
    }

  def main(args: Array[String]) = {
    assert( m1 )
    assert( m2 )
  }
}

class P extends Pair(1, 1) {
  override def equals(x: Any) = true
}