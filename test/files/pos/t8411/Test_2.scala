object Test extends App {
  val pf = Macros.defaultZeroCase { case 1 => 2 }
  assert(pf(2) == 0)
}
