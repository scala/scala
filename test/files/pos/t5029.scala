object Test {
	(Vector(): Seq[_]) match { case List() => true; case Nil => false }
}