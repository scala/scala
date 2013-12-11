class ann extends annotation.StaticAnnotation

class Use {
  def foo: (Int @ann) = ???
  def bar: (Int @ann) = (new Use{}).foo
}
