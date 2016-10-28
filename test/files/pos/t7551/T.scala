package p

@A(subInterface = classOf[T.S])
trait T {
}

object T {
  private[p] trait S extends T { }
}
