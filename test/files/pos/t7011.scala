object bar {
	def foo: Unit = {
    lazy val x = 42

    {()=>x}
  }
}