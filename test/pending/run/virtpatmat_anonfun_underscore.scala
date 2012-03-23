object Test extends App {
  List(1,2,3) map (_ match { case x => x + 1} ) // `_ match` is redundant but shouldn't crash the compiler
  List((1,2)) map (_ match { case (x, z) => x + z})
}