object Test extends App {
	import scala.collection.mutable.Stack

	println((for (i <- 0 to 10) yield {
		val in = Stack.tabulate(i)(_.toString)
		(in, (in filter (_ => true)) == in)
	}).mkString("\n"))
}
