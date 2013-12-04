class C {
  val x: ((Int, Int) => Int) = (((a, b)) => a)
  val y: ((Int, Int, Int) => Int) = (((a, !!)) => a)
  val z: ((Int, Int, Int) => Int) = (((a, NotAPatternVariableName, c)) => a)
}
