sealed trait CL3Literal
case object IntLit extends CL3Literal
case object CharLit extends CL3Literal
case object BooleanLit extends CL3Literal
case object UnitLit extends CL3Literal
 
 
sealed trait Tree
case class LetL(value: CL3Literal) extends Tree
case object LetP extends Tree
case object LetC extends Tree
case object LetF extends Tree
 
object Test {
  (tree: Tree) => tree match {case LetL(CharLit) => ??? }
  (tree: Tree) => tree match {case LetL(CharLit) => ??? }
  (tree: Tree) => tree match {case LetL(CharLit) => ??? }
  (tree: Tree) => tree match {case LetL(CharLit) => ??? }
  (tree: Tree) => tree match {case LetL(CharLit) => ??? }
  (tree: Tree) => tree match {case LetL(CharLit) => ??? }
  // After the first patch for SI-8430, we achieve stability: all of 
  // these get the same warning:
  //
  // ??, LetC, LetF, LetL(IntLit), LetP
  //
  // Before, it was non-deterministic.
  //
  // However, we our list of counter examples is itself non-exhaustive.
  // We need to rework counter example generation to fix that.
  //
  // That work is the subject of SI-7746
}
