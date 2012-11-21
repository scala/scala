object Test extends App {
	try {
	  Array("a", "b", "c") match {
	  	case Array("a", "x", "c") => println("x")
	  	case Array("a", "b", "x") => println("a");
	  	case Array("a", "d", _*) => println("wrongly positive")
	  }
	  assert(false, "match succeeded")
	} catch {
		case _: MatchError => // okay
	}

  Array("a", "b", "c") match {
  	case Array("a", "x", "c") => println("x")
  	case Array("a", "b", "x") => println("a");
  	case Array("a", "b", _*) => // okay
  }
}
