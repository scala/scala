trait X {
  final val x = "hello, world"
  def f = Seq(
    f"""a\""",
    f"a\xc",
    // following could suggest \u000b for vertical tab, similar for \a alert
    f"a\vc",
    f"\v$x%.4s, Fred",
  )
  def s = Seq(
    s"She said, $x. And then \s.",
  )
}
