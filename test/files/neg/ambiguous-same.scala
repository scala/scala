
// When faced with ambiguities between imports,
// an attempt is made to see if the imports intend
// identical types.
//
// Here, no attempt is made to notice that x
// names the same thing.
//
object X {
  val x = 42
  def f = {
    import X.x
    x
  }
}
