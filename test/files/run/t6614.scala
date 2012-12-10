object Test extends App {
	import scala.collection.mutable.ArrayStack

	println((for (i <- 0 to 10) yield {
		val in = ArrayStack.tabulate(i)(_.toString)
		(in, (in filter (_ => true)) == in)
	}).mkString("\n"))
}
