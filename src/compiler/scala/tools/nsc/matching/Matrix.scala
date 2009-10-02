/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import util.Position

trait Matrix extends PatternOptimizer {
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import analyzer.Typer
  import CODE._

  case class MatrixInit(
    roots: List[Symbol],
    cases: List[CaseDef],
    default: Tree
  )

  case class MatrixContext(
    handleOuter: TreeFunction1,   // Tree => Tree function
    typer: Typer,                 // a local typer
    owner: Symbol,                // the current owner
    matchResultType: Type)        // the expected result type of the whole match
      extends Squeezer
  {
    def newVar(
      pos: Position,
      tpe: Type,
      flags: List[Long] = Nil,
      name: Name = null): Symbol =
    {
      val n: Name = if (name == null) newName(pos, "temp") else name
      // careful: pos has special meaning
      owner.newVariable(pos, n) setInfo tpe setFlag (0L /: flags)(_|_)
    }

    def typedValDef(x: Symbol, rhs: Tree) = {
      val finalRhs = x.tpe match {
        case WildcardType   =>
          rhs setType null
          x setInfo (typer typed rhs).tpe
          rhs
        case _              =>
          typer.typed(rhs, x.tpe)
      }
      typer typed (VAL(x) === finalRhs)
    }
  }

}