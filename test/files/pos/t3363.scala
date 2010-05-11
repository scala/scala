object TestCase {

        //now matter if you put (abstract) class or trait it will fail in all cases
        trait MapOps[T]

        //if fs was reduced to List (generic type with one parameter) then the code compiles
        //if you inherit from MapOps[T] instead of MapOps[F] then code compiles fine
        implicit def map2ops[T,F](fs: Map[T,F]) = new MapOps[F] {
          //if you remove this line, then code compiles
	    lazy val m: Manifest[T] = error("just something to make it compile")
	    def is(xs: List[T]) = List(xs)
	  }

	  def main(args: Array[String]) {
	    println(Map(1 -> "2") is List(2))
	  }

	}
