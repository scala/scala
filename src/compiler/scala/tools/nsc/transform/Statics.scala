/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

abstract class Statics extends Transform with ast.TreeDSL {
  import global._

  trait StaticsTransformer extends AstTransformer {
    /** generate a static constructor with symbol fields inits, or an augmented existing static ctor
      */
    def staticConstructor(body: List[Tree], localTyper: analyzer.Typer, pos: Position)(newStaticInits: List[Tree]): Tree =
      body.collectFirst {
        // If there already was a static ctor - augment existing one
        // currently, however, static ctors aren't being generated anywhere else (!!!)
        case ctor@DefDef(_, nme.CONSTRUCTOR, _, _, _, _) if ctor.symbol.hasStaticFlag =>
          // modify existing static ctor
          deriveDefDef(ctor) {
            case block@Block(stats, expr) =>
              // need to add inits to existing block
              treeCopy.Block(block, newStaticInits ::: stats, expr)
            case term: TermTree           =>
              // need to create a new block with inits and the old term
              treeCopy.Block(term, newStaticInits, term)
            case x => throw new MatchError(x)
          }
      } getOrElse {
        // create new static ctor
        val staticCtorSym = currentClass.newStaticConstructor(pos)
        val rhs = Block(newStaticInits, Literal(Constant(())))

        localTyper.typedPos(pos)(DefDef(staticCtorSym, rhs))
      }
  }
}
