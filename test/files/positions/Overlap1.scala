object Overlap1 {
	val l = List("foo")
	val p = true
	for(e <- l if p) yield e.length
}
