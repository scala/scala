package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

/** The main attribution phase.
 */
abstract class Analyzer extends Contexts with Namers with Typers with TypeCheckers {
  val global: Global;

  import global._;

  case class ImportType(tree: Import) extends Type;
}
