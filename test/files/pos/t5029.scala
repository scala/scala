//> using options -Xfatal-warnings
//
object Test {
	(Vector(): Seq[_]) match { case List() => true; case Nil => false; case x => throw new MatchError(x) }
}
