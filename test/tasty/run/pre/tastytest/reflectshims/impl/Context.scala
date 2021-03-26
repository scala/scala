package tastytest.reflectshims.impl

import tastytest.reflectshims

object Context extends reflectshims.Context {

  object universe extends reflectshims.Universe {

    abstract class TreeShimImpl extends TreeShimApi with Product

    type TreeShim = TreeShimImpl

    case object EmptyTree extends TreeShimImpl

  }

}
