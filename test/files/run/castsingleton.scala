object Test extends App {
  case class L();
  object N extends L();

  def empty(xs : L) : Unit = xs match {
    case x@N  => println(x); println(x);
    case x    => println(x); println(x);
  }

  empty(L())
} 
