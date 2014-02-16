
trait X {
  def f = Seq(
    f"a\10c",
    f"a\11c",
    f"a\12c",
    f"a\15c",
    f"a\42c",
    f"a\134c",
    f"a\15151515c"
  )
}

object Test extends App with X {
  f foreach println
}
