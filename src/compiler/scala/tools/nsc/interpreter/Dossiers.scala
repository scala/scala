/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

// Coming soon
trait Dossiers {
  val intp: IMain

  import intp._
  import intp.global._
  import definitions._

  trait Dossier {
    def symbol: Symbol
    def staticType: Type

    def id               = name.toString
    def name             = symbol.name
    def normalizedType   = staticType.typeSymbolDirect.tpe.normalize
    def simpleNameOfType = staticType.typeSymbol.simpleName
    def staticTypeString = staticType.toString

    override def toString = "Dossier on %s:\n  static type %s (normalized %s)".format(
      symbol, staticType, normalizedType
    )
  }

  class TypeDossier(val symbol: TypeSymbol, val staticType: Type) extends Dossier {
    override def toString = super.toString
  }

  class TermDossier(val symbol: TermSymbol, val staticType: Type, val value: AnyRef) extends Dossier {
    def runtimeClass: JClass = value.getClass
    def runtimeSymbol: Symbol  = getClassIfDefined(runtimeClass.getName)
    def runtimeType: Type      = runtimeSymbol.tpe
    def runtimeTypeString      = TypeStrings.fromClazz(runtimeClass)

    def runtimeTypedParam = NamedParamClass(id, runtimeTypeString, value)
    def staticTypedParam  = NamedParamClass(id, staticTypeString, value)

    def isRuntimeTypeTighter = runtimeSymbol.ancestors contains normalizedType.typeSymbol

    override def toString = super.toString + (
      "\n  runtime type %s/%s\n  value %s".format(
        runtimeType, runtimeTypeString, value
      )
    )
  }
}

