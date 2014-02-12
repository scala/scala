trait HasElem { type A }
trait View[AIn] {
  val tc: HasElem { type A = AIn }
  def f2(p: tc.A): tc.A = p
}

object Test {
  val view: View[Int] = null

  view f2 5  // fails
}


/**
typeOf[Test.view.tc.A].dealias

AIn.asSeenFrom(
  View.<refinement>.type, (underlying = HasElem{type A = Int}, baseTypeSeq = BTS(View.<refinement>.type,HasElem{type A = Int},HasElem,Object,Any))
  <refinement of HasElem> (.info      = HasElem{type A = AIn})
)

During this ASF, we tried:

pre                = View.<refinement>.type (underlying = HasElem{type A = Int}, baseTypeSeq = BTS(View.<refinement>.type,HasElem{type A = Int},HasElem,Object,Any)
clazz              = `<refinement of HasElem>(.info =HasElem{type A = AIn})`
pre baseType clazz = NoType
nextBase           = NoType

After this patch, we continue with:

clazz.isRefinementClass = true
loop(Test.view.type, trait View)

From which we find:

nextBase = Test.view.type baseType 'trait View'
nextBase = View[Int]
AInt = Int

*/