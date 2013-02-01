object bar {
	def foo {
    lazy val x = 42

    {()=>x}
  }
}