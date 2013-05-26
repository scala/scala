object Test extends App {
	lazy val odds: Stream[Int] = Stream(1) append ( odds flatMap {x => Stream(x + 2)} )
	Console println (odds take 42).force
}
