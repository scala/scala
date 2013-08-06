/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.language.implicitConversions
import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.runtime.{universe => ru}

/** A class which the repl utilizes to expose predefined objects.
 *  The base implementation is empty; the standard repl implementation
 *  is StdReplVals.
 */
abstract class ReplVals { }

class StdReplVals(final val r: ILoop) extends ReplVals {
  final lazy val repl                     = r
  final lazy val intp                     = r.intp
  final lazy val power                    = r.power
  final lazy val reader                   = r.in
  final lazy val vals                     = this
  final lazy val global: intp.global.type = intp.global
  final lazy val isettings                = intp.isettings
  final lazy val completion               = reader.completion
  final lazy val history                  = reader.history
  final lazy val phased                   = power.phased
  final lazy val analyzer                 = global.analyzer

  object treedsl extends { val global: intp.global.type = intp.global } with ast.TreeDSL { }

  final lazy val typer = analyzer.newTyper(
    analyzer.rootContext(
      power.unit("").asInstanceOf[analyzer.global.CompilationUnit]
    )
  )
  def lastRequest = intp.lastRequest

  class ReplImplicits extends power.Implicits2 {
    import intp.global.Symbol

    private val tagFn = ReplVals.mkCompilerTypeFromTag[intp.global.type](global)
    implicit def mkCompilerTypeFromTag(sym: Symbol) = tagFn(sym)
  }

  final lazy val replImplicits = new ReplImplicits

  def typed[T <: analyzer.global.Tree](tree: T): T = typer.typed(tree).asInstanceOf[T]
}

object ReplVals {
  /** Latest attempt to work around the challenge of foo.global.Type
   *  not being seen as the same type as bar.global.Type even though
   *  the globals are the same.  Dependent method types to the rescue.
   */
  def mkCompilerTypeFromTag[T <: Global](global: T) = {
    import global._

    /** We can't use definitions.compilerTypeFromTag directly because we're passing
     *  it to map and the compiler refuses to perform eta expansion on a method
     *  with a dependent return type.  (Can this be relaxed?) To get around this
     *  I have this forwarder which widens the type and then cast the result back
     *  to the dependent type.
     */
    def compilerTypeFromTag(t: ApiUniverse # WeakTypeTag[_]): Global#Type =
      definitions.compilerTypeFromTag(t)

    class AppliedTypeFromTags(sym: Symbol) {
      def apply[M](implicit m1: ru.TypeTag[M]): Type =
        if (sym eq NoSymbol) NoType
        else appliedType(sym, compilerTypeFromTag(m1).asInstanceOf[Type])

      def apply[M1, M2](implicit m1: ru.TypeTag[M1], m2: ru.TypeTag[M2]): Type =
        if (sym eq NoSymbol) NoType
        else appliedType(sym, compilerTypeFromTag(m1).asInstanceOf[Type], compilerTypeFromTag(m2).asInstanceOf[Type])
    }

    (sym: Symbol) => new AppliedTypeFromTags(sym)
  }
}
