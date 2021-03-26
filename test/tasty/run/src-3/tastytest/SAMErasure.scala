package tastytest

object SAMErasure {

  trait TreeShimSAMApi extends Product

  type TreeShimSAM >: Null <: AnyRef with TreeShimSAMApi

  implicit val TreeShimSAMTag: reflect.ClassTag[TreeShimSAM] =
    reflect.classTag[TreeShimSAMApi].asInstanceOf[reflect.ClassTag[TreeShimSAM]]

  @FunctionalInterface
  trait FunTreeShimSAM { def apply(a: TreeShimSAM): TreeShimSAM }

  @FunctionalInterface
  trait FunTreeShimSAM2 { def apply(a: Array[TreeShimSAM]): Array[TreeShimSAM] }

}
