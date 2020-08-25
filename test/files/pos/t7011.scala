// scalac: -Ydebug -Xfatal-warnings
object bar {
	def foo {
    lazy val x = 42

    {()=>x}
  }
}
