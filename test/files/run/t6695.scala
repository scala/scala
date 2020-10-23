object Test extends App {
	try {
	  Array("a", "b", "c") match {
	  	case Array("a", "x", "c") => println("x")
	  	case Array("a", "b", "x") => println("a");
	  	case Array("a", "d", _*) => println("wrongly positive")
	  	case x                   => throw new MatchError(x)
	  }
	  assert(false, "match succeeded")
	} catch {
		case _: MatchError => // okay
	}

  Array("a", "b", "c") match {
  	case Array("a", "x", "c") => println("x")
  	case Array("a", "b", "x") => println("a");
  	case Array("a", "b", _*) => // okay
  	case x                   => throw new MatchError(x)
  }
}
