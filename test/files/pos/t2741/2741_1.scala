trait Partial {
  type Apply[XYZ] = List[XYZ]
}
trait MA[M[_]]
trait MAs {
  val a: MA[Partial#Apply] = null // after compilation, the type is pickled as `MA[ [B] List[B] ]`
}

object Scalaz extends MAs