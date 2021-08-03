// here we test unpickling a sealed child in another tasty file
package dottyi3149

import tastytest._

object TestFooChildren {
  compiletimeHasNestedChildren[Foo](
    "dottyi3149.Foo.Bar",
    "dottyi3149.Foo.dottyi3149$Foo$$localSealedChildProxy", // workaround to represent "dottyi3149.Test.Bar$1",k
    "dottyi3149.Test.O.Bar",
    "dottyi3149.Test.C.Bar"
  )
}
