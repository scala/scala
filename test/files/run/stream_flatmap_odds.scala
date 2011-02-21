object Test extends App {
	lazy val odds: Stream[Int] = Stream(1) append ( odds flatMap {x => Stream(x + 2)} )
	println(odds take 42 force)
}
