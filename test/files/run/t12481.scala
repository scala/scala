object Test extends App {
  trait Txn[T <: Txn[T]]
  trait Universe[T <: Txn[T]]
  println(implicitly[Manifest[Universe[_]]])
  println(implicitly[OptManifest[Universe[_]]])
}
