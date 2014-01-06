class C {

  val x: ((Int, Int)) => Int = (a, b) => 0

  val y: ((Int, Int, Int)) => Int = (a, b, !!) => 0

  val z: ((Int, Int, Int)) => Int = (a, NotAVariablePatternName, c) => 0
}
