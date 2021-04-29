package tastytest

import tastytest.reflectshims

object Reflection {

  def reflectionInvokerIdentity(ctx: reflectshims.Context)(tree: ctx.TreeShim): ctx.TreeShim = tree

  class Invoker[C <: reflectshims.Context with Singleton](val ctx: C)(root: ctx.TreeShim) {
    def tree: ctx.TreeShim = root
  }

  class InvokerSAM[C <: reflectshims.Context with Singleton](val ctx: C) {

    @FunctionalInterface
    trait TreeFn {
      def apply(tree: ctx.TreeShim): ctx.TreeShim
    }

  }

}
