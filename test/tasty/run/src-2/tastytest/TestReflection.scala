package tastytest

import tastytest.reflectshims.impl.Context
import Context.universe.EmptyTree
import Context.TreeShim

object TestReflection extends Suite("TestReflection") {

  test(assert(Reflection.reflectionInvokerIdentity(Context)(EmptyTree) === (EmptyTree: TreeShim)))
  test(assert(new Reflection.Invoker(Context)(EmptyTree).tree === (EmptyTree: TreeShim)))

  // TODO [tasty]: enable due to missing type ctx.TreeShim
  // test {
  //   val invoker = new Reflection.InvokerSAM(Context)
  //   val id: invoker.TreeFn = x => x
  //   assert(id(EmptyTree) === (EmptyTree: TreeShim))
  // }
}
