object Test extends App {
// the function passed to flatMap produces lots of empty streams, but this should not overflow the stack
	val res = Stream.from(1).flatMap(i => if (i < 3000) Stream.empty else List(1))
	println(res take 42 force)
}
