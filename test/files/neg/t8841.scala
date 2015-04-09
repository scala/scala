class Cell(val ambiguousName: Option[String])
 
class Test {
  def wrap(f: Any): Nothing = ???
 
  wrap {
    // the namer for these two ValDefs is created when typing the argument expression
    // of wrap. This happens to be in a silent context (tryTypedApply). Therefore, the
    // cyclic reference will not be thrown, but transformed into a NormalTypeError by
    // `silent`. This requires different handling in NamesDefaults.

    val c = new Cell(ambiguousName = Some("bla"))
    val ambiguousName = c.ambiguousName
  }
}
