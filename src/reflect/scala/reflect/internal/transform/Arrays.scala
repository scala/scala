package scala
package reflect
package internal
package transform

trait Arrays {
  self: Erasure =>

  val global: SymbolTable
  import global._
  import definitions._

  trait ErasureContext {
    def erase(tp: Type): Type
    def erasedTypeRef(sym: Symbol): Type = typeRef(erase(sym.owner.tpe), sym, Nil)

    def bestErasureForApparentArray(tp: Type): Type = arrayElementType(tp) match {
      case NoType => ObjectTpe
      case elem   => bestErasureForArrayElement(elem)
    }
    def bestErasureForArrayElements(elems: List[Type]): Type = (
      (elems map bestErasureForArrayElement).distinct match {
        case best :: Nil                            => best
        case bests if bests forall isReferenceArray => ObjectArrayTpe
        case _                                      => ObjectTpe
      }
    )
    def bestErasureForArrayElement(elem: Type): Type = logResult(s"bestErasureForArrayElement($elem)")(elem.dealiasWiden match {
      case et @ ExistentialType(_, tp) =>
        val best = bestErasureForArrayElement(tp)
        if (isReferenceArray(best)) best else ObjectTpe
      case RefinedType(parents, decls) if decls.isEmpty => bestErasureForArrayElements(parents)
      case RefinedType(parents, _)                      => bestErasureForArrayElements(ObjectTpe :: parents)
      case TypeRef(_, ArrayClass, elem :: Nil)          => arrayType(bestErasureForArrayElement(elem))
      case TypeRef(_, UnitClass, _)                     => arrayType(BoxedUnitClass.tpe)
      case TypeRef(_, sym, _) if isPhantomClass(sym)    => ObjectArrayTpe
      case TypeRef(_, sym, _) if isScalaAbstract(sym)   => ObjectTpe
      case TypeRef(pre, sym, _) if sym.isAbstractType   => bestErasureForArrayElement(erase((pre memberInfo sym).bounds.hi))
      case tr @ TypeRef(_, sym, _)                      => arrayType(erasedTypeRef(sym))
      case _                                            =>
        devWarning(s"Not sure what this is: an Array of $elem")
        ObjectTpe
    })
  }

  /** Does this type represent an Array which must be erased to Object?
   *  Such is the case when the concrete manifestation may be array types
   *  which appear differently to the jvm. Of special note is that any
   *  existential, such as Array[_ <: Int], must be represented as Object,
   *  even though one might naively think that is limited to Array[Int].
   *  Example types which conform to that yet will erase differently are
   *  Array[Nothing] and Array[String with Int].
   */

  /** An extractor object for generic arrays */
  object GenericArray {

    /** Is `tp` an unbounded generic type (i.e. which could be instantiated
     *  with primitive as well as class types)?.
     */
    private def genericCore(tp: Type): Type = tp.dealiasWiden match {
      /* A Java Array<T> is erased to Array[Object] (T can only be a reference type), where as a Scala Array[T] is
       * erased to Object. However, there is only symbol for the Array class. So to make the distinction between
       * a Java and a Scala array, we check if the owner of T comes from a Java class.
       * This however caused issue SI-5654. The additional test for EXSITENTIAL fixes it, see the ticket comments.
       * In short, members of an existential type (e.g. `T` in `forSome { type T }`) can have pretty arbitrary
       * owners (e.g. when computing lubs, <root> is used). All packageClass symbols have `isJavaDefined == true`.
       */
      case TypeRef(_, sym, _) if sym.isAbstractType && (!sym.owner.isJavaDefined || sym.hasFlag(Flags.EXISTENTIAL)) =>
        tp
      case ExistentialType(tparams, restp) =>
        genericCore(restp)
      case _ =>
        NoType
    }

    /** If `tp` is of the form Array[...Array[T]...] where `T` is an abstract type
     *  then Some((N, T)) where N is the number of Array constructors enclosing `T`,
     *  otherwise None. Existentials on any level are ignored.
     */
    def unapply(tp: Type): Option[(Int, Type)] = tp.dealiasWiden match {
      case TypeRef(_, ArrayClass, List(arg)) =>
        genericCore(arg) match {
          case NoType =>
            unapply(arg) match {
              case Some((level, core)) => Some((level + 1, core))
              case None => None
            }
          case core =>
            Some((1, core))
        }
      case ExistentialType(tparams, restp) =>
        unapply(restp)
      case _ =>
        None
    }
  }

  protected def unboundedGenericArrayLevel(tp: Type): Int = tp match {
    case GenericArray(level, core) if !(core <:< AnyRefClass.tpe) => level
    case _ => 0
  }
}
