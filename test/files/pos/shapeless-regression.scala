class W[T <: AnyRef](val t: T) {
  val v: T {} = t
}

object W {
  def apply[T <: AnyRef](t: T) = new W[t.type](t)
}

object RightAssoc {
  def ra_:[T](t: T): Unit = ()
}

object Boom {
  W("fooo").v ra_: RightAssoc
}

