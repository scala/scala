/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab
package classfile

import java.io.IOException
import java.lang.{Float, Double}

import Flags._
import scala.reflect.generic.PickleFormat._
import collection.mutable.{HashMap, ListBuffer}
import annotation.switch

/** @author Martin Odersky
 *  @version 1.0
 */
abstract class UnPickler extends reflect.generic.UnPickler {
  val global: Global
  import global._

  def scan(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) =
    new CompileScan(bytes, offset, classRoot, moduleRoot, filename).run()

  class CompileScan(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String)
  extends Scan(bytes, offset, classRoot, moduleRoot, filename) {

    protected override def debug = settings.debug.value

    override def noSuchTypeTag(tag: Int, end: Int): Type = {
      tag match {
        case DEBRUIJNINDEXtpe =>
          DeBruijnIndex(readNat(), readNat())
        case _ =>
          super.noSuchTypeTag(tag, end)
      }
    }

    override protected def errorMissingRequirement(name: Name, owner: Symbol) =
      errorMissingRequirement(
        "reference " + (if (name.isTypeName) "type " else "value ") +
        name.decode + " of " + owner.tpe.widen + " refers to nonexisting symbol.")

    def inferMethodAlternative(fun: Tree, argtpes: List[Type], restpe: Type) =
      typer.infer.inferMethodAlternative(fun, List(), argtpes, restpe)

    def newLazyTypeRef(i: Int): LazyType = new LazyTypeRef(i)
    def newLazyTypeRefAndAlias(i: Int, j: Int): LazyType = new LazyTypeRefAndAlias(i, j)

    /** A lazy type which when completed returns type at index `i`. */
    private class LazyTypeRef(i: Int) extends LazyType {
      private val definedAtRunId = currentRunId
      private val p = phase
      override def complete(sym: Symbol) : Unit = {
        val tp = at(i, readType)
        if (p != phase) atPhase(p) (sym setInfo tp)
        else sym setInfo tp
        if (currentRunId != definedAtRunId) sym.setInfo(adaptToNewRunMap(tp))
      }
      override def load(sym: Symbol) { complete(sym) }
    }

    /** A lazy type which when completed returns type at index `i` and sets alias
     *  of completed symbol to symbol at index `j`.
     */
    private class LazyTypeRefAndAlias(i: Int, j: Int) extends LazyTypeRef(i) {
      override def complete(sym: Symbol) {
        super.complete(sym)
        var alias = at(j, readSymbol)
        if (alias hasFlag OVERLOADED) {
          atPhase(currentRun.picklerPhase) {
            alias = alias suchThat (alt => sym.tpe =:= sym.owner.thisType.memberType(alt))
          }
        }
        sym.asInstanceOf[TermSymbol].setAlias(alias)
      }
    }
  }
}
