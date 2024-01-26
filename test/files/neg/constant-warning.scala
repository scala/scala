//> using options -Werror -Xlint:constant
//-Vprint:cleanup (bytecode test to ensure warnable constants are folded)
object Test {
  val fails = 1 + 2 / (3 - 2 - 1)

  val addi: Int = Int.MaxValue + 42
  val subi: Int = Int.MinValue - 42
  val muli: Int = Int.MaxValue * 2
  val divi: Int = Int.MinValue / -1
  val divz: Int = Int.MinValue / 0

  val long: Long = 100 * 1024 * 1024 * 1024
  val addl: Long = Long.MaxValue + 42
  val subl: Long = Long.MinValue - 42
  val mull: Long = Long.MaxValue * 2
  val divl: Long = Long.MinValue / -1
  val divlz: Long = Long.MinValue / 0
}
