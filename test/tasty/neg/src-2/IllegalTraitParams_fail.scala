package tastytest

class Illegal extends TraitParams.Foo("?")
class IllegalMulti extends TraitParams.Bar(0, "")
class IllegalUseParam extends TraitParams.Baz("baz") {
  def foo = println(baz)
}
class IllegalIgnoreParam extends TraitParams.Qux
class IllegalOverrideParamField extends TraitParams.ForOverride {
  override val myParam: String = ""
}
class IllegalOverrideParamParam(override val myParam: String) extends TraitParams.ForOverride
class IllegalOverrideFinalParamField extends TraitParams.ForOverrideFinal {
  override val myFinalParam: String = ""
}
class IllegalOverrideFinalParamParam(override val myFinalParam: String) extends TraitParams.ForOverrideFinal

class IllegalTopLevel extends TopLevelTraitParams1("?")
class IllegalTopLevelMulti extends TopLevelTraitParams2(0, "")
class IllegalTopLevelUseParam extends TopLevelTraitParams3(true) {
  def bar: Unit = {
    if (b) {
      println(23)
    }
  }
}

class IllegalTopLevelIgnoreParam extends TopLevelTraitParams4

trait OkTraitExtendsTraitWithParams extends TopLevelTraitParams4 // ok, it is a trait
class IllegalMixinFromTraitWithParams extends OkTraitExtendsTraitWithParams // error - mixin indirectly parameterised trait
