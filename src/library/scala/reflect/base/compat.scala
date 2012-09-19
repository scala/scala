package scala.reflect.base

// should be removed once I re-deploy the starr
trait Exprs {
  case class Expr
}
trait TypeTags {
  case class TypeTag
  case class WeakTypeTag
}
trait Universe {
  def reify: Nothing = ???
}
trait MirrorOf
trait TypeCreator
trait TreeCreator