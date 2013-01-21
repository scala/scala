object Test extends App {
	val o: Option[(Int, Int)] = None
	// can't elide withFilter (easily) as we would need to
	// retypecheck the subsequent call to `WithFilter#map`
	// to 'Option#map'.
	for ((i, j) <- o) yield i
}