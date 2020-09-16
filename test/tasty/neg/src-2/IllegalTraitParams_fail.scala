package tastytest

class Illegal extends TraitParams.Foo("?")
class IllegalMulti extends TraitParams.Bar(0, "")
class IllegalUseParam extends TraitParams.Baz {
  def foo = println(baz)
}
class IllegalIgnoreParam extends TraitParams.Qux
