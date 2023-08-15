package scala.tools.xsbt

import xsbti.api._

import Function.tupled
import scala.annotation.tailrec
import scala.collection.{ immutable, mutable }

/** Checks the API of two source files for equality.*/
object SameAPI {
  def apply(a: AnalyzedClass, b: AnalyzedClass): Boolean =
    a.apiHash == b.apiHash

  def hasSameExtraHash(a: AnalyzedClass, b: AnalyzedClass): Boolean =
    a.extraHash() == b.extraHash()

  def apply(a: Definition, b: Definition): Boolean =
    new SameAPI(false, true).sameDefinitions(List(a), List(b), topLevel = true)

  def apply(a: Companions, b: Companions): Boolean = {
    val sameClasses = apply(a.classApi, b.classApi)
    val sameObjects = apply(a.objectApi, b.objectApi)
    sameClasses && sameObjects
  }

  def apply(a: ClassLike, b: ClassLike): Boolean = new SameAPI(false, true).check(a, b)

  def separateDefinitions(s: Seq[Definition]): (Seq[Definition], Seq[Definition]) =
    s.partition(isValueDefinition)
  def isValueDefinition(d: Definition): Boolean =
    d match {
      case _: FieldLike | _: Def => true
      case c: ClassLikeDef       => isValue(c.definitionType)
      case _                     => false
    }
  def isValue(d: DefinitionType): Boolean =
    d == DefinitionType.Module || d == DefinitionType.PackageModule

  /** Puts the given definitions in a map according to their names.*/
  def byName(s: Seq[Definition]): Map[String, List[Definition]] = {
    var map = Map[String, List[Definition]]()
    for (d <- s; name = d.name)
      map = map.updated(name, d :: map.getOrElse(name, Nil))
    map
  }

  /**
   * Removes definitions that should not be considered for API equality.
   * All top-level definitions are always considered: 'private' only means package-private.
   * Other definitions are considered if they are not qualified with 'private[this]' or 'private'.
   */
  def filterDefinitions(ds: Seq[Definition], topLevel: Boolean, includePrivate: Boolean) =
    if (topLevel || includePrivate) ds else ds.filter(!_.access().isInstanceOf[Private])

}

/**
 * Used to implement API equality.
 *
 * If `includePrivate` is true, `private` and `private[this]` members are included in the comparison.  Otherwise, those members are excluded.
 */
class SameAPI(includePrivate: Boolean, includeParamNames: Boolean) {
  import SameAPI._

  private val pending = new mutable.HashSet[AnyRef]
  private[this] val debugEnabled = java.lang.Boolean.getBoolean("xsbt.api.debug")
  def debug(flag: Boolean, msg: => String): Boolean = {
    if (debugEnabled && !flag) println(msg)
    flag
  }

  /** Returns true if source `a` has the same API as source `b`.*/
  def check(a: ClassLike, b: ClassLike): Boolean = {
    debug(a.name == b.name, "Class names differed") &&
      debug(sameDefinitionContent(a, b), "Classes differed")
  }

  def sameTopLevel(a: ClassLike, b: ClassLike): Boolean =
    a.topLevel == b.topLevel

  def sameDefinitions(a: Seq[Definition], b: Seq[Definition], topLevel: Boolean): Boolean = {
    val (avalues, atypes) = separateDefinitions(filterDefinitions(a, topLevel, includePrivate))
    val (bvalues, btypes) = separateDefinitions(filterDefinitions(b, topLevel, includePrivate))
    debug(sameDefinitions(byName(avalues), byName(bvalues)), "Value definitions differed") &&
      debug(sameDefinitions(byName(atypes), byName(btypes)), "Type definitions differed")
  }
  def sameDefinitions(
                       a: scala.collection.Map[String, List[Definition]],
                       b: scala.collection.Map[String, List[Definition]]
                     ): Boolean =
    debug(
      sameStrings(a.keySet, b.keySet),
      "\tDefinition strings differed (a: " + (a.keySet.toSet -- b.keySet) + ", b: " + (b.keySet.toSet -- a.keySet) + ")"
    ) &&
      zippedEntries(a, b).forall(tupled(sameNamedDefinitions))

  /**
   * Checks that the definitions in `a` are the same as those in `b`, ignoring order.
   * Each list is assumed to have already been checked to have the same names (by `sameDefinitions`, for example).
   */
  def sameNamedDefinitions(a: List[Definition], b: List[Definition]): Boolean = {
    def sameDefs(a: List[Definition], b: List[Definition]): Boolean = {
      a match {
        case adef :: atail =>
          @tailrec
          def sameDef(seen: List[Definition], remaining: List[Definition]): Boolean =
            remaining match {
              case Nil => debug(flag = false, "Definition different in new API: \n" + adef.name)
              case bdef :: btail =>
                val eq = sameDefinitionContent(adef, bdef)
                if (eq) sameDefs(atail, seen ::: btail) else sameDef(bdef :: seen, btail)
            }
          sameDef(Nil, b)
        case Nil => true
      }
    }
    debug(
      a.length == b.length,
      "\t\tLength differed for " + a.headOption.map(_.name).getOrElse("empty")
    ) && sameDefs(a, b)
  }

  /** Checks that the two definitions are the same, other than their name.*/
  def sameDefinitionContent(a: Definition, b: Definition): Boolean =
    samePending(a, b)(sameDefinitionContentDirect)
  def sameDefinitionContentDirect(a: Definition, b: Definition): Boolean = {
    // a.name == b.name &&
    debug(sameAccess(a.access, b.access), "Access differed") &&
      debug(sameModifiers(a.modifiers, b.modifiers), "Modifiers differed") &&
      debug(sameAnnotations(a.annotations, b.annotations), "Annotations differed") &&
      debug(sameDefinitionSpecificAPI(a, b), "Definition-specific differed")
  }

  def sameAccess(a: Access, b: Access): Boolean =
    (a, b) match {
      case (_: Public, _: Public)         => true
      case (qa: Protected, qb: Protected) => sameQualifier(qa, qb)
      case (qa: Private, qb: Private)     => sameQualifier(qa, qb)
      case _                              => debug(flag = false, "Different access categories")
    }
  def sameQualifier(a: Qualified, b: Qualified): Boolean =
    sameQualifier(a.qualifier, b.qualifier)
  def sameQualifier(a: Qualifier, b: Qualifier): Boolean =
    (a, b) match {
      case (_: Unqualified, _: Unqualified)     => true
      case (_: ThisQualifier, _: ThisQualifier) => true
      case (ia: IdQualifier, ib: IdQualifier) =>
        debug(ia.value == ib.value, "Different qualifiers")
      case _ =>
        debug(
          flag = false,
          "Different qualifier categories: " + a.getClass.getName + " -- " + b.getClass.getName
        )
    }

  def sameModifiers(a: Modifiers, b: Modifiers): Boolean =
    bitSet(a) == bitSet(b)

  def bitSet(m: Modifiers): immutable.BitSet = {
    import m._
    val modifiers =
      List(isAbstract, isOverride, isFinal, isSealed, isImplicit, isLazy, isMacro).zipWithIndex
    (modifiers foldLeft immutable.BitSet.empty) { case (bs, (mod, i)) => if (mod) bs + i else bs }
  }
  def setIf(bs: mutable.BitSet, flag: Boolean, i: Int): Unit = {
    if (flag) bs += i
    ()
  }

  def sameAnnotations(a: Array[Annotation], b: Array[Annotation]): Boolean =
    sameAnnotations(a.toIndexedSeq, b.toIndexedSeq)
  def sameAnnotations(a: Seq[Annotation], b: Seq[Annotation]): Boolean =
    sameSeq(a, b)(sameAnnotation)
  def sameAnnotation(a: Annotation, b: Annotation): Boolean =
    debug(sameType(a.base, b.base), "Annotation base type differed") &&
      debug(
        sameAnnotationArguments(a.arguments, b.arguments),
        "Annotation arguments differed (" + a + ") and (" + b + ")"
      )
  def sameAnnotationArguments(a: Array[AnnotationArgument], b: Array[AnnotationArgument]): Boolean =
    sameAnnotationArguments(a.toIndexedSeq, b.toIndexedSeq)
  def sameAnnotationArguments(a: Seq[AnnotationArgument], b: Seq[AnnotationArgument]): Boolean =
    argumentMap(a) == argumentMap(b)
  def argumentMap(a: Seq[AnnotationArgument]): Map[String, String] =
    Map() ++ a.map(arg => (arg.name, arg.value))

  def sameDefinitionSpecificAPI(a: Definition, b: Definition): Boolean =
    (a, b) match {
      case (fa: FieldLike, fb: FieldLike) => sameFieldSpecificAPI(fa, fb)
      case (pa: ParameterizedDefinition, pb: ParameterizedDefinition) =>
        sameParameterizedDefinition(pa, pb)
      case (ca: ClassLike, cb: ClassLike) => sameClassLikeSpecificAPI(ca, cb)
      case _                              => false
    }

  def sameParameterizedDefinition(a: ParameterizedDefinition, b: ParameterizedDefinition): Boolean =
    debug(
      sameTypeParameters(a.typeParameters, b.typeParameters),
      "Different type parameters for " + a.name
    ) &&
      sameParameterizedSpecificAPI(a, b)

  def sameParameterizedSpecificAPI(
                                    a: ParameterizedDefinition,
                                    b: ParameterizedDefinition
                                  ): Boolean =
    (a, b) match {
      case (da: Def, db: Def)                         => sameDefSpecificAPI(da, db)
      case (ca: ClassLikeDef, cb: ClassLikeDef)       => sameClassLikeDefSpecificAPI(ca, cb)
      case (ta: TypeAlias, tb: TypeAlias)             => sameAliasSpecificAPI(ta, tb)
      case (ta: TypeDeclaration, tb: TypeDeclaration) => sameDeclarationSpecificAPI(ta, tb)
      case _                                          => false
    }

  def sameDefSpecificAPI(a: Def, b: Def): Boolean =
    debug(
      sameValueParameters(a.valueParameters, b.valueParameters),
      "Different def value parameters for " + a.name
    ) &&
      debug(sameType(a.returnType, b.returnType), "Different def return type for " + a.name)
  def sameAliasSpecificAPI(a: TypeAlias, b: TypeAlias): Boolean =
    debug(sameType(a.tpe, b.tpe), "Different alias type for " + a.name)
  def sameDeclarationSpecificAPI(a: TypeDeclaration, b: TypeDeclaration): Boolean =
    debug(
      sameType(a.lowerBound, b.lowerBound),
      "Different lower bound for declaration " + a.name
    ) &&
      debug(sameType(a.upperBound, b.upperBound), "Different upper bound for declaration " + a.name)
  def sameFieldSpecificAPI(a: FieldLike, b: FieldLike): Boolean =
    debug(
      sameFieldCategory(a, b),
      "Different field categories (" + a.name + "=" + a.getClass.getName + " -- " + a.name + "=" + a.getClass.getName + ")"
    ) &&
      debug(sameType(a.tpe, b.tpe), "Different field type for " + a.name)

  def sameFieldCategory(a: FieldLike, b: FieldLike): Boolean =
    (a, b) match {
      case (_: Val, _: Val) => true
      case (_: Var, _: Var) => true
      case _                => false
    }

  def sameClassLikeSpecificAPI(a: ClassLike, b: ClassLike): Boolean = {
    debug(sameTopLevel(a, b), "Top level flag differs") &&
      sameDefinitionType(a.definitionType, b.definitionType) &&
      sameType(a.selfType, b.selfType) &&
      sameSeq(a.childrenOfSealedClass, b.childrenOfSealedClass)(sameType) &&
      sameStructure(a.structure, b.structure)
  }

  def sameClassLikeDefSpecificAPI(a: ClassLikeDef, b: ClassLikeDef): Boolean =
    sameDefinitionType(a.definitionType, b.definitionType)

  def sameValueParameters(a: Array[ParameterList], b: Array[ParameterList]): Boolean =
    sameValueParameters(a.toIndexedSeq, b.toIndexedSeq)
  def sameValueParameters(a: Seq[ParameterList], b: Seq[ParameterList]): Boolean =
    sameSeq(a, b)(sameParameterList)

  def sameParameterList(a: ParameterList, b: ParameterList): Boolean =
    (a.isImplicit == b.isImplicit) &&
      sameParameters(a.parameters, b.parameters)
  def sameParameters(a: Array[MethodParameter], b: Array[MethodParameter]): Boolean =
    sameParameters(a.toIndexedSeq, b.toIndexedSeq)
  def sameParameters(a: Seq[MethodParameter], b: Seq[MethodParameter]): Boolean =
    sameSeq(a, b)(sameMethodParameter)
  def sameMethodParameter(a: MethodParameter, b: MethodParameter): Boolean =
    (!includeParamNames || a.name == b.name) &&
      sameType(a.tpe, b.tpe) &&
      (a.hasDefault == b.hasDefault) &&
      sameParameterModifier(a.modifier, b.modifier)
  def sameParameterModifier(a: ParameterModifier, b: ParameterModifier) =
    a == b
  def sameDefinitionType(a: DefinitionType, b: DefinitionType): Boolean =
    a == b
  def sameVariance(a: Variance, b: Variance): Boolean =
    a == b

  def sameTypeParameters(a: Array[TypeParameter], b: Array[TypeParameter]): Boolean =
    sameTypeParameters(a.toIndexedSeq, b.toIndexedSeq)
  def sameTypeParameters(a: Seq[TypeParameter], b: Seq[TypeParameter]): Boolean =
    debug(sameSeq(a, b)(sameTypeParameter), "Different type parameters")
  def sameTypeParameter(a: TypeParameter, b: TypeParameter): Boolean = {
    sameTypeParameters(a.typeParameters, b.typeParameters) &&
      debug(sameAnnotations(a.annotations, b.annotations), "Different type parameter annotations") &&
      debug(sameVariance(a.variance, b.variance), "Different variance") &&
      debug(sameType(a.lowerBound, b.lowerBound), "Different lower bound") &&
      debug(sameType(a.upperBound, b.upperBound), "Different upper bound") &&
      sameTags(a.id, b.id)
  }
  def sameTags(a: String, b: String): Boolean =
    debug(a == b, "Different type parameter bindings: " + a + ", " + b)

  def sameType(a: Type, b: Type): Boolean =
    samePending(a, b)(sameTypeDirect)
  def sameTypeDirect(a: Type, b: Type): Boolean = {
    (a, b) match {
      case (pa: Projection, pb: Projection) =>
        debug(sameProjection(pa, pb), "Different projection")
      case (pa: ParameterRef, pb: ParameterRef) =>
        debug(sameParameterRef(pa, pb), "Different parameter ref")
      case (pa: Polymorphic, pb: Polymorphic) =>
        debug(samePolymorphicType(pa, pb), "Different polymorphic type")
      case (pa: Parameterized, pb: Parameterized) =>
        debug(sameParameterized(pa, pb), "Different parameterized")
      case (sa: Singleton, sb: Singleton) => debug(sameSingleton(sa, sb), "Different singleton")
      case (ca: Constant, cb: Constant) =>
        debug(
          sameConstantType(ca, cb),
          "Different constant types: " + ca + " and " + cb
        )
      case (aa: Annotated, ab: Annotated) =>
        debug(sameAnnotatedType(aa, ab), "Different annotated types")
      case (sa: Structure, sb: Structure) =>
        debug(sameStructureDirect(sa, sb), "Different structure type")
      case (ea: Existential, eb: Existential) =>
        debug(sameExistentialType(ea, eb), "Different existential type")
      case (_: EmptyType, _: EmptyType) => true
      case _                            => differentCategory("type", a, b)
    }
  }

  def sameConstantType(ca: Constant, cb: Constant): Boolean =
    sameType(ca.baseType, cb.baseType) &&
      ca.value == cb.value
  def sameExistentialType(a: Existential, b: Existential): Boolean =
    sameTypeParameters(a.clause, b.clause) &&
      sameType(a.baseType, b.baseType)
  def samePolymorphicType(a: Polymorphic, b: Polymorphic): Boolean =
    sameTypeParameters(a.parameters, b.parameters) &&
      sameType(a.baseType, b.baseType)
  def sameAnnotatedType(a: Annotated, b: Annotated): Boolean =
    sameType(a.baseType, b.baseType) &&
      sameAnnotations(a.annotations, b.annotations)
  def sameStructure(a: Structure, b: Structure): Boolean =
    samePending(a, b)(sameStructureDirect)

  private[this] def samePending[T](a: T, b: T)(f: (T, T) => Boolean): Boolean =
    if (pending add ((a, b))) f(a, b) else true

  def sameStructureDirect(a: Structure, b: Structure): Boolean = {
    sameSeq(a.parents, b.parents)(sameType) &&
      sameMembers(a.declared, b.declared) &&
      sameMembers(a.inherited, b.inherited)
  }

  def sameMembers[D <: Definition](a: Array[D], b: Array[D]): Boolean =
    sameMembers(a.toIndexedSeq, b.toIndexedSeq)
  def sameMembers(a: Seq[Definition], b: Seq[Definition]): Boolean =
    sameDefinitions(a, b, topLevel = false)

  def differentCategory(label: String, a: AnyRef, b: AnyRef): Boolean =
    debug(
      flag = false,
      "Different category of " + label + " (" + a.getClass.getName + " and " + b.getClass.getName + ") for (" + a + " and " + b + ")"
    )

  def sameParameterized(a: Parameterized, b: Parameterized): Boolean =
    sameType(a.baseType, b.baseType) &&
      sameSeq(a.typeArguments, b.typeArguments)(sameType)
  def sameParameterRef(a: ParameterRef, b: ParameterRef): Boolean = sameTags(a.id, b.id)
  def sameSingleton(a: Singleton, b: Singleton): Boolean =
    samePath(a.path, b.path)
  def sameProjection(a: Projection, b: Projection): Boolean =
    sameType(a.prefix, b.prefix) &&
      (a.id == b.id)

  def samePath(a: Path, b: Path): Boolean =
    samePathComponents(a.components, b.components)
  def samePathComponents(a: Array[PathComponent], b: Array[PathComponent]): Boolean =
    samePathComponents(a.toIndexedSeq, b.toIndexedSeq)
  def samePathComponents(a: Seq[PathComponent], b: Seq[PathComponent]): Boolean =
    sameSeq(a, b)(samePathComponent)
  def samePathComponent(a: PathComponent, b: PathComponent): Boolean =
    (a, b) match {
      case (_: This, _: This)     => true
      case (sa: Super, sb: Super) => samePathSuper(sa, sb)
      case (ia: Id, ib: Id)       => samePathId(ia, ib)
      case _                      => false
    }
  def samePathSuper(a: Super, b: Super): Boolean =
    samePath(a.qualifier, b.qualifier)
  def samePathId(a: Id, b: Id): Boolean =
    a.id == b.id

  // precondition: a.keySet == b.keySet
  protected def zippedEntries[A, B](
                                     a: scala.collection.Map[A, B],
                                     b: scala.collection.Map[A, B]
                                   ): Iterable[(B, B)] =
    for ((key, avalue) <- a) yield (avalue, b(key))

  def sameStrings(a: scala.collection.Set[String], b: scala.collection.Set[String]): Boolean =
    a == b
  final def sameSeq[T](a: Array[T], b: Array[T])(eq: (T, T) => Boolean): Boolean =
    sameSeq(a.toIndexedSeq, b.toIndexedSeq)(eq)
  final def sameSeq[T](a: Seq[T], b: Seq[T])(eq: (T, T) => Boolean): Boolean =
    (a.length == b.length) && (a zip b).forall(tupled(eq))
}
