class C {
  def foo = {
    class C { private def x = 0 }

    {
      val a = 0
      object C {
        new C().x
      }
    }
  }
}
