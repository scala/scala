//> using options -Ydebug -Xfatal-warnings
//
object bar {
	def foo: Unit = {
    lazy val x = 42

    def f = {() => x}
  }
}
