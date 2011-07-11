package scala.reflect
package api

trait Types { self: Universe =>

  abstract class AbsType {
    def typeSymbol: Symbol
    def decl(name: Name): Symbol
  }

  type Type >: Null <: AbsType
  type SingletonType >: Null <: Type

  val NoType: Type
  val NoPrefix: Type

  type ThisType <: SingletonType
  val ThisType: ThisTypeExtractor

  type TypeRef <: Type
  val TypeRef: TypeRefExtractor

  type SingleType <: SingletonType
  val SingleType: SingleTypeExtractor

  type SuperType <: SingletonType
  val SuperType: SuperTypeExtractor

  type TypeBounds <: Type
  val TypeBounds: TypeBoundsExtractor

  type CompoundType <: Type

  type RefinedType <: CompoundType
  val RefinedType: RefinedTypeExtractor

  type ClassInfoType <: CompoundType
  val ClassInfoType: ClassInfoTypeExtractor

  type ConstantType <: Type
  val ConstantType: ConstantTypeExtractor

  type MethodType <: Type
  val MethodType: MethodTypeExtractor

  type NullaryMethodType <: Type
  val NullaryMethodType: NullaryMethodTypeExtractor

  type PolyType <: Type
  val PolyType: PolyTypeExtractor

  type ExistentialType <: Type
  val ExistentialType: ExistentialTypeExtractor

  type AnnotatedType <: Type
  val AnnotatedType: AnnotatedTypeExtractor

  abstract class ThisTypeExtractor {
    def apply(sym: Symbol): Type
    def unapply(tpe: ThisType): Option[Symbol]
  }

  abstract class SingleTypeExtractor {
    def apply(pre: Type, sym: Symbol): Type
    def unapply(tpe: SingleType): Option[(Type, Symbol)]
  }

  abstract class SuperTypeExtractor {
    def apply(thistpe: Type, supertpe: Type): Type
    def unapply(tpe: SuperType): Option[(Type, Type)]
  }

  abstract class TypeRefExtractor {
    def apply(pre: Type, sym: Symbol, args: List[Type]): Type
    def unapply(tpe: TypeRef): Option[(Type, Symbol, List[Type])]
  }

  abstract class TypeBoundsExtractor {
    def apply(lo: Type, hi: Type): TypeBounds
    def unapply(tpe: TypeBounds): Option[(Type, Type)]
  }

  abstract class RefinedTypeExtractor {
    def apply(parents: List[Type], decls: Scope): RefinedType
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType
    def unapply(tpe: RefinedType): Option[(List[Type], Scope)]
  }

  abstract class ClassInfoTypeExtractor {
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): ClassInfoType
    def unapply(tpe: ClassInfoType): Option[(List[Type], Scope, Symbol)]
  }

  abstract class ConstantTypeExtractor {
    def apply(value: Constant): ConstantType
    def unapply(tpe: ConstantType): Option[Constant]
  }

  abstract class MethodTypeExtractor {
    def apply(params: List[Symbol], resultType: Type): MethodType
    def unapply(tpe: MethodType): Option[(List[Symbol], Type)]
  }

  abstract class NullaryMethodTypeExtractor {
    def apply(resultType: Type): NullaryMethodType
    def unapply(tpe: NullaryMethodType): Option[(Type)]
  }

  abstract class PolyTypeExtractor {
    def apply(typeParams: List[Symbol], resultType: Type): PolyType
    def unapply(tpe: PolyType): Option[(List[Symbol], Type)]
  }

  abstract class ExistentialTypeExtractor {
    def apply(quantified: List[Symbol], underlying: Type): ExistentialType
    def unapply(tpe: ExistentialType): Option[(List[Symbol], Type)]
  }

  abstract class AnnotatedTypeExtractor {
    def apply(annotations: List[AnnotationInfo], underlying: Type, selfsym: Symbol): AnnotatedType
    def unapply(tpe: AnnotatedType): Option[(List[AnnotationInfo], Type, Symbol)]
  }
}

