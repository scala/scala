object Test extends App {
	lazy val odds: LazyList[Int] = LazyList(1) lazyAppendedAll ( odds flatMap {x => LazyList(x + 2)} )
	Console println (odds take 42).force
}
