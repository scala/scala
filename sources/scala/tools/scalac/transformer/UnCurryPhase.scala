/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id: UnCurryPhase.scala

import scalac.{Global => scalac_Global, _};
import scalac.symtab._;
import scalac.checkers._;

package scala.tools.scalac.transformer {

import scalac.util.NewArray;

class UnCurryPhase(global: scalac_Global, descriptor: PhaseDescriptor) extends Phase(global, descriptor) {

  import Modifiers._;

  /** Applies this phase to the given compilation units. */
  override def apply(units: Array[CompilationUnit]) = {
    var i = 0; while (i < units.length) {
      new UnCurry(global, this).apply(units(i));
      i = i + 1
    }
  }

  /** - return symbol's transformed type,
   *  - if symbol is a def parameter with transformed type T, return () => T
   */
  override def transformInfo(sym: Symbol, tp0: Type): Type = {
    val tp1 = uncurry(tp0);
    if (sym.isDefParameter()) global.definitions.FUNCTION_TYPE(Type.EMPTY_ARRAY, tp1);
    else tp1
  }

  /** - (ps_1)...(ps_n)T ==> (ps_1,...,ps_n)T
   */
  def uncurry(tp: Type): Type = tp match {
    case Type$MethodType(params, tp1) =>
      uncurry(tp1) match {
	case Type$MethodType(params1, tp2) =>
	  val newparams = new Array[Symbol](params.length + params1.length);
	  System.arraycopy(params, 0, newparams, 0, params.length);
	  System.arraycopy(params1, 0, newparams, params.length, params1.length);
	  new Type.MethodType(newparams, tp2);
	case newtp1 =>
	  if (newtp1 == tp1) tp;
	  else new Type.MethodType(params, newtp1);
      }
    case Type$PolyType(tparams, tp1) =>
      uncurry(tp1) match {
        case newtp1 @ Type$MethodType(_, _) =>
	  if (newtp1 == tp1) tp
	  else new Type.PolyType(tparams, newtp1)
	case newtp1 =>
	  val newtp2 = Type.MethodType(Symbol.EMPTY_ARRAY, newtp1);
	  if (tparams.length == 0) newtp2;
	  else Type.PolyType(tparams, newtp2)
      }
    case Type$OverloadedType(_, _) =>
      new Type$Map() {
	override def apply(t: Type) = uncurry(t)
      }.map(tp);
    case Type$ConstantType(base, _) =>
      base
    case Type$CompoundType(parents, scope) =>
      val symbol = tp.symbol();
      if (!symbol.isClass() || symbol.isCompoundSym()) tp
      else {
        val clone = new Scope();
        val it = scope.iterator(true);
        while (it.hasNext()) {
          val member = it.next();
          if (!isUnaccessedConstant(member) &&
              (!member.isCaseFactory() || member.isModule()))
            clone.enterOrOverload(member);
        }
        Type.compoundType(parents, clone, symbol)
      }
    case _ =>
      tp
  }

  def isUnaccessedConstant(symbol: Symbol): boolean =
    symbol.isTerm() &&
    (symbol.flags & ACCESSED) == 0 &&
    (symbol.getType() match {
      case Type$PolyType(params, Type$ConstantType(_, _)) =>
        params.length == 0
      case Type$ConstantType(_, _) =>
        true
      case _ =>
        false
    });

  override def postCheckers(global: scalac_Global): Array[Checker] =
    NewArray.Checker(new CheckSymbols(global), new CheckTypes(global));
}
}
