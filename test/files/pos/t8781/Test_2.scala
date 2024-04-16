//> using options -Ymacro-expand:discard -Ystop-after:typer
//
object Test {
  implicit class RichT(t: T) { def augmented = "" }

  Macros.fresh.augmented
}
