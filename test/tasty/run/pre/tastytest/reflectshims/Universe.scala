package tastytest.reflectshims

abstract class Universe {
  type TreeShim >: Null <: AnyRef with TreeShimApi
  trait TreeShimApi extends Product { this: TreeShim => }

  val EmptyTree: TreeShim
}
