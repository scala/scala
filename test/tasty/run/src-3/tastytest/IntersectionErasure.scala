package tastytest

object IntersectionErasure {

  trait Universe {

    type TreeShimSAM >: Null <: AnyRef with TreeShimSAMApi
    trait TreeShimSAMApi extends Product { this: TreeShimSAM => }

    val EmptyTree: TreeShimSAM

    @FunctionalInterface
    abstract class IntersectionSAM {
      def apply(tree: TreeShimSAM): TreeShimSAM
    }

  }

  object universe extends Universe {

    abstract class TreeShimSAMImpl extends TreeShimSAMApi with Product
    type TreeShimSAM = TreeShimSAMImpl
    case object EmptyTree extends TreeShimSAMImpl

  }


}
