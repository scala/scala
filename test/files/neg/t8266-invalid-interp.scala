
trait X {
  def f = Seq(
    // f"a\",    // escaped quote and unterminated string
    f"a\xc",
    // following could suggest \u000b for vertical tab, similar for \a alert
    f"a\vc"
  )
}
