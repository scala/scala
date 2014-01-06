object Exts {
  implicit class AnyExts[T](val o: T) extends AnyVal {
    def moo = "moo!"
  }
}

trait Exts {
  import language.implicitConversions
  implicit def AnyExts[T](o: T) = Exts.AnyExts(o)
}
