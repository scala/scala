object Test {
  implicit class RichT(t: T) { def augmented = "" }

  Macros.fresh.augmented
}
