package scala.reflect
package api

/** A mirror establishes connections of
 *  runtime entities such as class names and object instances
 *  with a refexive universe.
 */
private[reflect] trait RuntimeTypes extends Universe {

  type InstanceRefSymbol >: Null <: Symbol

  val InstanceRefSymbol: InstanceRefSymbolExtractor

  private[reflect] def namedType(pre: Type, sym: Symbol, args: List[Type]): Type

  abstract class InstanceRefSymbolExtractor {
    def apply(value: AnyRef): InstanceRefSymbol
    def unapply(tpe: InstanceRefSymbol): Option[AnyRef]
  }
}
