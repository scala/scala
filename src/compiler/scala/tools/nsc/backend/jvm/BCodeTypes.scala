/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend.jvm

import scala.tools.asm
import scala.collection.{ immutable, mutable }

/*
 *  Utilities to mediate between types as represented in Scala ASTs and ASM trees.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
abstract class BCodeTypes extends BCodeIdiomatic {

  import global._

  // when compiling the Scala library, some assertions don't hold (e.g., scala.Boolean has null superClass although it's not an interface)
  val isCompilingStdLib = !(settings.sourcepath.isDefault)

  // special names
  var StringReference             : BType = null
  var ThrowableReference          : BType = null
  var jlCloneableReference        : BType = null // java/lang/Cloneable
  var jioSerializableReference    : BType = null // java/io/Serializable
  var classCastExceptionReference : BType = null // java/lang/ClassCastException

  /* A map from scala primitive type-symbols to BTypes */
  var primitiveTypeMap: Map[Symbol, BType] = null
  /* A map from scala type-symbols for Nothing and Null to (runtime version) BTypes */
  var phantomTypeMap:   Map[Symbol, BType] = null
  /* Maps the method symbol for a box method to the boxed type of the result.
   *  For example, the method symbol for `Byte.box()`) is mapped to the BType `Ljava/lang/Integer;`. */
  var boxResultType:    Map[Symbol, BType] = null
  /* Maps the method symbol for an unbox method to the primitive type of the result.
   *  For example, the method symbol for `Byte.unbox()`) is mapped to the BType BYTE. */
  var unboxResultType:  Map[Symbol, BType] = null

  var hashMethodSym: Symbol = null // scala.runtime.ScalaRunTime.hash

  var AndroidParcelableInterface: Symbol = null
  var AndroidCreatorClass       : Symbol = null // this is an inner class, use asmType() to get hold of its BType while tracking in innerClassBufferASM

  var BeanInfoAttr: Symbol = null

  /* The Object => String overload. */
  var String_valueOf: Symbol = null

  // scala.FunctionX and scala.runtim.AbstractFunctionX
  val FunctionReference                 = new Array[Tracked](definitions.MaxFunctionArity + 1)
  val AbstractFunctionReference         = new Array[Tracked](definitions.MaxFunctionArity + 1)
  val abstractFunctionArityMap = mutable.Map.empty[BType, Int]

  var PartialFunctionReference:         BType = null // scala.PartialFunction
  var AbstractPartialFunctionReference: BType = null // scala.runtime.AbstractPartialFunction

  var BoxesRunTime: BType = null

  /*
   * must-single-thread
   */
  def initBCodeTypes() {
    import definitions._

    primitiveTypeMap =
      Map(
        UnitClass     -> UNIT,
        BooleanClass  -> BOOL,
        CharClass     -> CHAR,
        ByteClass     -> BYTE,
        ShortClass    -> SHORT,
        IntClass      -> INT,
        LongClass     -> LONG,
        FloatClass    -> FLOAT,
        DoubleClass   -> DOUBLE
      )

    phantomTypeMap =
      Map(
        NothingClass -> RT_NOTHING,
        NullClass    -> RT_NULL,
        NothingClass -> RT_NOTHING, // we map on purpose to RT_NOTHING, getting rid of the distinction compile-time vs. runtime for NullClass.
        NullClass    -> RT_NULL     // ditto.
      )

    boxResultType =
      for(Pair(csym, msym) <- definitions.boxMethod)
      yield (msym -> classLiteral(primitiveTypeMap(csym)))

    unboxResultType =
      for(Pair(csym, msym) <- definitions.unboxMethod)
      yield (msym -> primitiveTypeMap(csym))

    // boxed classes are looked up in the `exemplars` map by jvmWiseLUB().
    // Other than that, they aren't needed there (e.g., `isSubtypeOf()` special-cases boxed classes, similarly for others).
    val boxedClasses = List(BoxedBooleanClass, BoxedCharacterClass, BoxedByteClass, BoxedShortClass, BoxedIntClass, BoxedLongClass, BoxedFloatClass, BoxedDoubleClass)
    for(csym <- boxedClasses) {
      val key = brefType(csym.javaBinaryName.toTypeName)
      val tr  = buildExemplar(key, csym)
      symExemplars.put(csym, tr)
      exemplars.put(tr.c, tr)
    }

    // reversePrimitiveMap = (primitiveTypeMap map { case (s, pt) => (s.tpe, pt) } map (_.swap)).toMap

    hashMethodSym = getMember(ScalaRunTimeModule, nme.hash_)

    // TODO avoiding going through through missingHook for every line in the REPL: https://github.com/scala/scala/commit/8d962ed4ddd310cc784121c426a2e3f56a112540
    AndroidParcelableInterface = rootMirror.getClassIfDefined("android.os.Parcelable")
    AndroidCreatorClass        = rootMirror.getClassIfDefined("android.os.Parcelable$Creator")

    // the following couldn't be an eager vals in Phase constructors:
    // that might cause cycles before Global has finished initialization.
    BeanInfoAttr = rootMirror.getRequiredClass("scala.beans.BeanInfo")

    String_valueOf = {
      getMember(StringModule, nme.valueOf) filter (sym =>
        sym.info.paramTypes match {
          case List(pt) => pt.typeSymbol == ObjectClass
          case _        => false
        }
      )
    }

    exemplar(JavaCloneableClass).c
    exemplar(JavaSerializableClass).c
    exemplar(SerializableClass).c

    StringReference             = exemplar(StringClass).c
    StringBuilderReference      = exemplar(StringBuilderClass).c
    ThrowableReference          = exemplar(ThrowableClass).c
    jlCloneableReference        = exemplar(JavaCloneableClass).c
    jioSerializableReference    = exemplar(JavaSerializableClass).c
    classCastExceptionReference = exemplar(ClassCastExceptionClass).c

    /*
     *  The bytecode emitter special-cases String concatenation, in that three methods of `JCodeMethodN`
     *  ( `genStartConcat()` , `genStringConcat()` , and `genEndConcat()` )
     *  don't obtain the method descriptor of the callee via `asmMethodType()` (as normally done)
     *  but directly emit callsites on StringBuilder using literal constant for method descriptors.
     *  In order to make sure those method descriptors are available as BTypes, they are initialized here.
     */
    BType.getMethodType("()V")                   // necessary for JCodeMethodN.genStartConcat
    BType.getMethodType("()Ljava/lang/String;")  // necessary for JCodeMethodN.genEndConcat

    PartialFunctionReference    = exemplar(PartialFunctionClass).c
    for(idx <- 0 to definitions.MaxFunctionArity) {
      FunctionReference(idx)           = exemplar(FunctionClass(idx))
      AbstractFunctionReference(idx)   = exemplar(AbstractFunctionClass(idx))
      abstractFunctionArityMap        += (AbstractFunctionReference(idx).c -> idx)
      AbstractPartialFunctionReference = exemplar(AbstractPartialFunctionClass).c
    }

    // later a few analyses (e.g. refreshInnerClasses) will look up BTypes based on descriptors in instructions
    // we make sure those BTypes can be found via lookup as opposed to creating them on the fly.
    BoxesRunTime = brefType("scala/runtime/BoxesRunTime")
    asmBoxTo.values   foreach { mnat: MethodNameAndType => BType.getMethodType(mnat.mdesc) }
    asmUnboxTo.values foreach { mnat: MethodNameAndType => BType.getMethodType(mnat.mdesc) }

  }

  /*
   * must-single-thread
   */
  def clearBCodeTypes() {
    symExemplars.clear()
    exemplars.clear()
  }

  val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL

  val strMODULE_INSTANCE_FIELD = nme.MODULE_INSTANCE_FIELD.toString

  // ------------------------------------------------
  // accessory maps tracking the isInterface, innerClasses, superClass, and supportedInterfaces relations,
  // allowing answering `conforms()` without resorting to typer.
  // ------------------------------------------------

  val exemplars       = new java.util.concurrent.ConcurrentHashMap[BType,  Tracked]
  val symExemplars    = new java.util.concurrent.ConcurrentHashMap[Symbol, Tracked]

  /*
   *  Typically, a question about a BType can be answered only by using the BType as lookup key in one or more maps.
   *  A `Tracked` object saves time by holding together information required to answer those questions:
   *
   *    - `sc`     denotes the bytecode-level superclass if any, null otherwise
   *
   *    - `ifaces` denotes the interfaces explicitly declared.
   *               Not included are those transitively supported, but the utility method `allLeafIfaces()` can be used for that.
   *
   *    - `innersChain` denotes the containing classes for a non-package-level class `c`, null otherwise.
   *               Note: the optimizer may inline anonymous closures, thus eliding those inner classes
   *               (no physical class file is emitted for elided classes).
   *               Before committing `innersChain` to bytecode, cross-check with the list of elided classes (SI-6546).
   *
   *  All methods of this class can-multi-thread
   */
  case class Tracked(c: BType, flags: Int, sc: Tracked, ifaces: Array[Tracked], innersChain: Array[InnerClassEntry]) {
    /* `isCompilingStdLib` saves the day when compiling:
     *     (1) scala.Nothing (the test `c.isNonSpecial` fails for it)
     *     (2) scala.Boolean (it has null superClass and is not an interface)
     */
    assert(c.isNonSpecial || isCompilingStdLib /*(1)*/, s"non well-formed plain-type: $this")
    assert(
        if (sc == null) { (c == ObjectReference) || isInterface || isCompilingStdLib /*(2)*/ }
        else            { (c != ObjectReference) && !sc.isInterface }
      , "non well-formed plain-type: " + this
    )
    assert(ifaces.forall(i => i.c.isNonSpecial && i.isInterface), s"non well-formed plain-type: $this")

    import asm.Opcodes._
    def hasFlags(mask: Int) = (flags & mask) != 0
    def isInterface  = hasFlags(ACC_INTERFACE)
    def isFinal      = hasFlags(ACC_FINAL)
    def isInnerClass = { innersChain != null }
    def isLambda = {
      // ie isLCC || isTraditionalClosureClass
      isFinal && (c.getSimpleName.contains(tpnme.ANON_FUN_NAME.toString)) && isFunctionType(c)
    }

    /* can-multi-thread */
    def superClasses: List[Tracked] = {
      if (sc == null) Nil else sc :: sc.superClasses
    }

    /* can-multi-thread */
    def isSubtypeOf(other: BType): Boolean = {
      assert(other.isNonSpecial, "so called special cases have to be handled in BCodeTypes.conforms()")

      if (c == other) return true;

      val otherIsIface = exemplars.get(other).isInterface

      if (this.isInterface) {
        if (other == ObjectReference) return true;
        if (!otherIsIface) return false;
      }
      else {
        if (sc != null && sc.isSubtypeOf(other)) return true;
        if (!otherIsIface) return false;
      }

      var idx = 0
      while (idx < ifaces.length) {
        if (ifaces(idx).isSubtypeOf(other)) return true;
        idx += 1
      }

      false
    }

    /*
     *  The `ifaces` field lists only those interfaces declared by `c`
     *  From the set of all supported interfaces, this method discards those which are supertypes of others in the set.
     */
    def allLeafIfaces: Set[Tracked] = {
      if (sc == null) { ifaces.toSet }
      else { minimizeInterfaces(ifaces.toSet ++ sc.allLeafIfaces) }
    }

    /*
     *  This type may not support in its entirety the interface given by the argument, however it may support some of its super-interfaces.
     *  We visualize each such supported subset of the argument's functionality as a "branch". This method returns all such branches.
     *
     *  In other words, let Ri be a branch supported by `ib`,
     *  this method returns all Ri such that this <:< Ri, where each Ri is maximally deep.
     */
    def supportedBranches(ib: Tracked): Set[Tracked] = {
      assert(ib.isInterface, s"Non-interface argument: $ib")

      val result: Set[Tracked] =
        if (this.isSubtypeOf(ib.c)) { Set(ib) }
        else { ib.ifaces.toSet[Tracked].flatMap( bi => supportedBranches(bi) ) }

      checkAllInterfaces(result)

      result
    }

    override def toString = { c.toString }

  }

  /* must-single-thread */
  final def isDeprecated(sym: Symbol): Boolean = { sym.annotations exists (_ matches definitions.DeprecatedAttr) }

  /* must-single-thread */
  final def hasInternalName(sym: Symbol) = { sym.isClass || (sym.isModule && !sym.isMethod) }

  /* must-single-thread */
  def getSuperInterfaces(csym: Symbol): List[Symbol] = {

    // Additional interface parents based on annotations and other cues
    def newParentForAttr(ann: AnnotationInfo): Symbol = ann.symbol match {
      case definitions.RemoteAttr => definitions.RemoteInterfaceClass
      case _                      => NoSymbol
    }

    /* Drop redundant interfaces (which are implemented by some other parent) from the immediate parents.
     *  In other words, no two interfaces in the result are related by subtyping.
     *  This method works on Symbols, a similar one (not duplicate) works on Tracked instances.
     */
    def minimizeInterfaces(lstIfaces: List[Symbol]): List[Symbol] = {
      var rest   = lstIfaces
      var leaves = List.empty[Symbol]
      while (!rest.isEmpty) {
        val candidate = rest.head
        val nonLeaf = leaves exists { lsym => lsym isSubClass candidate }
        if (!nonLeaf) {
          leaves = candidate :: (leaves filterNot { lsym => candidate isSubClass lsym })
        }
        rest = rest.tail
      }

      leaves
    }

    val superInterfaces0: List[Symbol] = csym.mixinClasses
    val superInterfaces = existingSymbols(superInterfaces0 ++ csym.annotations.map(newParentForAttr)).distinct

    assert(!superInterfaces.contains(NoSymbol), s"found NoSymbol among: ${superInterfaces.mkString}")
    assert(superInterfaces.forall(s => s.isInterface || s.isTrait), s"found non-interface among: ${superInterfaces.mkString}")

    minimizeInterfaces(superInterfaces)
  }

  /*
   * Records the superClass and supportedInterfaces relations,
   * so that afterwards queries can be answered without resorting to typer.
   * This method does not add to `innerClassBufferASM`, use `internalName()` or `asmType()` or `toTypeKind()` for that.
   * On the other hand, this method does record the inner-class status of the argument, via `buildExemplar()`.
   *
   * must-single-thread
   */
  final def exemplar(csym0: Symbol): Tracked = {
    assert(csym0 != NoSymbol, "NoSymbol can't be tracked")

    val csym = {
      if (csym0.isJavaDefined && csym0.isModuleClass) csym0.linkedClassOfClass
      else if (csym0.isModule) csym0.moduleClass
      else csym0 // we track only module-classes and plain-classes
    }

    assert(!primitiveTypeMap.contains(csym) || isCompilingStdLib, s"primitive types not tracked here: ${csym.fullName}")
    assert(!phantomTypeMap.contains(csym), s"phantom types not tracked here: ${csym.fullName}")

    val opt = symExemplars.get(csym)
    if (opt != null) {
      return opt
    }

    val key = brefType(csym.javaBinaryName.toTypeName)
    assert(key.isNonSpecial || isCompilingStdLib, s"Not a class to track: ${csym.fullName}")

    // TODO accomodate the fix for SI-5031 of https://github.com/scala/scala/commit/0527b2549bcada2fda2201daa630369b377d0877
    // TODO Weaken this assertion? buildExemplar() needs to be updated, too. In the meantime, pos/t5031_3 has been moved to test/disabled/pos.
    val whatWasInExemplars = exemplars.get(key)
    assert(whatWasInExemplars == null, "Maps `symExemplars` and `exemplars` got out of synch.")
    val tr = buildExemplar(key, csym)
    symExemplars.put(csym, tr)
    if (csym != csym0) { symExemplars.put(csym0, tr) }
    exemplars.put(tr.c, tr) // tr.c is the hash-consed, internalized, canonical representative for csym's key.
    tr
  }

  val EMPTY_TRACKED_ARRAY  = Array.empty[Tracked]

  /*
   * must-single-thread
   */
  private def buildExemplar(key: BType, csym: Symbol): Tracked = {
    val sc =
     if (csym.isImplClass) definitions.ObjectClass
     else csym.superClass
    assert(
      if (csym == definitions.ObjectClass)
        sc == NoSymbol
      else if (csym.isInterface)
        sc == definitions.ObjectClass
      else
        ((sc != NoSymbol) && !sc.isInterface) || isCompilingStdLib,
      "superClass out of order"
    )
    val ifaces    = getSuperInterfaces(csym) map exemplar;
    val ifacesArr =
     if (ifaces.isEmpty) EMPTY_TRACKED_ARRAY
     else {
      val arr = new Array[Tracked](ifaces.size)
      ifaces.copyToArray(arr)
      arr
     }

    val flags = mkFlags(
      javaFlags(csym),
      if (isDeprecated(csym)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo access flag
    )

    val tsc = if (sc == NoSymbol) null else exemplar(sc)

    val innersChain = saveInnerClassesFor(csym, key)

    Tracked(key, flags, tsc, ifacesArr, innersChain)
  }

  // ---------------- utilities around interfaces represented by Tracked instances. ----------------

  /*  Drop redundant interfaces (those which are implemented by some other).
   *  In other words, no two interfaces in the result are related by subtyping.
   *  This method works on Tracked elements, a similar one (not duplicate) works on Symbols.
   */
  def minimizeInterfaces(lstIfaces: Set[Tracked]): Set[Tracked] = {
    checkAllInterfaces(lstIfaces)
    var rest   = lstIfaces.toList
    var leaves = List.empty[Tracked]
    while (!rest.isEmpty) {
      val candidate = rest.head
      val nonLeaf = leaves exists { leaf => leaf.isSubtypeOf(candidate.c) }
      if (!nonLeaf) {
        leaves = candidate :: (leaves filterNot { leaf => candidate.isSubtypeOf(leaf.c) })
      }
      rest = rest.tail
    }

    leaves.toSet
  }

  def allInterfaces(is: Iterable[Tracked]): Boolean = { is forall { i => i.isInterface } }
  def nonInterfaces(is: Iterable[Tracked]): Iterable[Tracked] = { is filterNot { i => i.isInterface } }

  def checkAllInterfaces(ifaces: Iterable[Tracked]) {
    assert(allInterfaces(ifaces), s"Non-interfaces: ${nonInterfaces(ifaces).mkString}")
  }

  /*
   * Subtype check `a <:< b` on BTypes that takes into account the JVM built-in numeric promotions (e.g. BYTE to INT).
   * Its operation can be visualized more easily in terms of the Java bytecode type hierarchy.
   * This method used to be called, in the ICode world, TypeKind.<:<()
   *
   * can-multi-thread
   */
  final def conforms(a: BType, b: BType): Boolean = {
    if (a.isArray) { // may be null
      /* Array subtyping is covariant here, as in Java bytecode. Also necessary for Java interop. */
      if ((b == jlCloneableReference)     ||
          (b == jioSerializableReference) ||
          (b == AnyRefReference))    { true  }
      else if (b.isArray)            { conforms(a.getComponentType, b.getComponentType) }
      else                           { false }
    }
    else if (a.isBoxed) { // may be null
      if (b.isBoxed)                 { a == b }
      else if (b == AnyRefReference) { true   }
      else if (!(b.hasObjectSort))   { false  }
      else                           { exemplars.get(a).isSubtypeOf(b) } // e.g., java/lang/Double conforms to java/lang/Number
    }
    else if (a.isNullType) { // known to be null
      if (b.isNothingType)      { false }
      else if (b.isValueType)   { false }
      else                      { true  }
    }
    else if (a.isNothingType) { // known to be Nothing
      true
    }
    else if (a.isUnitType) {
      b.isUnitType
    }
    else if (a.hasObjectSort) { // may be null
      if (a.isNothingType)      { true  }
      else if (b.hasObjectSort) { exemplars.get(a).isSubtypeOf(b) }
      else if (b.isArray)       { a.isNullType } // documentation only, because `if(a.isNullType)` (above) covers this case already.
      else                      { false }
    }
    else {

      def msg = s"(a: $a, b: $b)"

      assert(a.isNonUnitValueType, s"a isn't a non-Unit value type. $msg")
      assert(b.isValueType, s"b isn't a value type. $msg")

      (a eq b) || (a match {
        case BOOL | BYTE | SHORT | CHAR => b == INT || b == LONG // TODO Actually, BOOL does NOT conform to LONG. Even with adapt().
        case _                          => a == b
      })
    }
  }

  /* The maxValueType of (Char, Byte) and of (Char, Short) is Int, to encompass the negative values of Byte and Short. See ticket #2087.
   *
   * can-multi-thread
   */
  def maxValueType(a: BType, other: BType): BType = {
    assert(a.isValueType, "maxValueType() is defined only for 1st arg valuetypes (2nd arg doesn't matter).")

    def uncomparable: Nothing = {
      abort(s"Uncomparable BTypes: $a with $other")
    }

    if (a.isNothingType)      return other;
    if (other.isNothingType)  return a;
    if (a == other)           return a;

    a match {

      case UNIT => uncomparable
      case BOOL => uncomparable

      case BYTE =>
        if (other == CHAR)             INT
        else if (other.isNumericType)  other
        else                           uncomparable

      case SHORT =>
        other match {
          case BYTE                          => SHORT
          case CHAR                          => INT
          case INT  | LONG  | FLOAT | DOUBLE => other
          case _                             => uncomparable
        }

      case CHAR =>
        other match {
          case BYTE | SHORT                 => INT
          case INT  | LONG | FLOAT | DOUBLE => other
          case _                            => uncomparable
        }

      case INT =>
        other match {
          case BYTE | SHORT | CHAR   => INT
          case LONG | FLOAT | DOUBLE => other
          case _                     => uncomparable
        }

      case LONG =>
        if (other.isIntegralType)   LONG
        else if (other.isRealType)  DOUBLE
        else                        uncomparable

      case FLOAT =>
        if (other == DOUBLE)           DOUBLE
        else if (other.isNumericType)  FLOAT
        else                           uncomparable

      case DOUBLE =>
        if (other.isNumericType)  DOUBLE
        else                      uncomparable

      case _ => uncomparable
    }
  }

  /* Takes promotions of numeric primitives into account.
   *
   *  can-multi-thread
   */
  final def maxType(a: BType, other: BType): BType = {
    if (a.isValueType) { maxValueType(a, other) }
    else {
      if (a.isNothingType)     return other;
      if (other.isNothingType) return a;
      if (a == other)          return a;
       // Approximate `lub`. The common type of two references is always AnyRef.
       // For 'real' least upper bound wrt to subclassing use method 'lub'.
      assert(a.isArray || a.isBoxed || a.hasObjectSort, s"This is not a valuetype and it's not something else, what is it? $a")
      // TODO For some reason, ICode thinks `REFERENCE(...).maxType(BOXED(whatever))` is `uncomparable`. Here, that has maxType AnyRefReference.
      //      BTW, when swapping arguments, ICode says BOXED(whatever).maxType(REFERENCE(...)) == AnyRefReference, so I guess the above was an oversight in REFERENCE.maxType()
      if (other.isRefOrArrayType) { AnyRefReference }
      else                        { abort(s"Uncomparable BTypes: $a with $other") }
    }
  }

  /*
   *  Whether the argument is a subtype of
   *    scala.PartialFunction[-A, +B] extends (A => B)
   *  N.B.: this method returns true for a scala.runtime.AbstractPartialFunction
   *
   *  can-multi-thread
   */
  def isPartialFunctionType(t: BType): Boolean = {
    (t.hasObjectSort) && exemplars.get(t).isSubtypeOf(PartialFunctionReference)
  }

  /*
   *  Whether the argument is a subtype of scala.FunctionX where 0 <= X <= definitions.MaxFunctionArity
   *
   *  can-multi-thread
   */
  def isFunctionType(t: BType): Boolean = {
    if (!t.hasObjectSort) return false
    var idx = 0
    val et: Tracked = exemplars.get(t)
    while (idx <= definitions.MaxFunctionArity) {
      if (et.isSubtypeOf(FunctionReference(idx).c)) {
        return true
      }
      idx += 1
    }
    false
  }

  /*
   * must-single-thread
   */
  def isTopLevelModule(sym: Symbol): Boolean = {
    exitingPickler { sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass }
  }

  /*
   * must-single-thread
   */
  def isStaticModule(sym: Symbol): Boolean = {
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
  }

  // ---------------------------------------------------------------------
  // ---------------- InnerClasses attribute (JVMS 4.7.6) ----------------
  // ---------------------------------------------------------------------

  val INNER_CLASSES_FLAGS =
    (asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_PRIVATE   | asm.Opcodes.ACC_PROTECTED |
     asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_INTERFACE | asm.Opcodes.ACC_ABSTRACT  | asm.Opcodes.ACC_FINAL)

  /*
   * @param name the internal name of an inner class.
   * @param outerName the internal name of the class to which the inner class belongs.
   *                  May be `null` for non-member inner classes (ie for a Java local class or a Java anonymous class).
   * @param innerName the (simple) name of the inner class inside its enclosing class. It's `null` for anonymous inner classes.
   * @param access the access flags of the inner class as originally declared in the enclosing class.
   */
  case class InnerClassEntry(name: String, outerName: String, innerName: String, access: Int) {
    assert(name != null, "Null isn't good as class name in an InnerClassEntry.")
  }

  /* For given symbol return a symbol corresponding to a class that should be declared as inner class.
   *
   *  For example:
   *  class A {
   *    class B
   *    object C
   *  }
   *
   *  then method will return:
   *    NoSymbol for A,
   *    the same symbol for A.B (corresponding to A$B class), and
   *    A$C$ symbol for A.C.
   *
   * must-single-thread
   */
  def innerClassSymbolFor(s: Symbol): Symbol =
    if (s.isClass) s else if (s.isModule) s.moduleClass else NoSymbol

  /*
   *  Computes the chain of inner-class (over the is-member-of relation) for the given argument.
   *  The resulting chain will be cached in `exemplars`.
   *
   *  The chain thus cached is valid during this compiler run, see in contrast
   *  `innerClassBufferASM` for a cache that is valid only for the class being emitted.
   *
   *  The argument can be any symbol, but given that this method is invoked only from `buildExemplar()`,
   *  in practice it has been vetted to be a class-symbol.
   *
   *  Returns:
   *
   *    - a non-empty array of entries for an inner-class argument.
   *      The array's first element is the outermost top-level class,
   *      the array's last element corresponds to csym.
   *
   *    - null otherwise.
   *
   *  This method does not add to `innerClassBufferASM`, use instead `exemplar()` for that.
   *
   *  must-single-thread
   */
  final def saveInnerClassesFor(csym: Symbol, csymTK: BType): Array[InnerClassEntry] = {

    val ics = innerClassSymbolFor(csym)
    if (ics == NoSymbol) {
      return null
    }
    assert(ics == csym, s"Disagreement between innerClassSymbolFor() and exemplar()'s tracked symbol for the same input: ${csym.fullName}")

    var chain: List[Symbol] = Nil
    var x = ics
    while (x ne NoSymbol) {
      assert(x.isClass, s"not a class symbol: ${x.fullName}")
      val isInner = !x.rawowner.isPackageClass
      if (isInner) {
        chain ::= x
        x = innerClassSymbolFor(x.rawowner)
      } else {
        x = NoSymbol
      }
    }

    // now that we have all of `ics` , `csym` , and soon the inner-classes-chain, it's too tempting not to cache.
    if (chain.isEmpty) { null }
    else {
      val arr = new Array[InnerClassEntry](chain.size)
      (chain map toInnerClassEntry).copyToArray(arr)

      arr
    }
  }

  /*
   * must-single-thread
   */
  private def toInnerClassEntry(innerSym: Symbol): InnerClassEntry = {

    /* The outer name for this inner class. Note that it returns null
     *  when the inner class should not get an index in the constant pool.
     *  That means non-member classes (anonymous). See Section 4.7.5 in the JVMS.
     */
    def outerName(innerSym: Symbol): Name = {
      if (innerSym.originalEnclosingMethod != NoSymbol)
        null
      else {
        val outerName = innerSym.rawowner.javaBinaryName
        if (isTopLevelModule(innerSym.rawowner)) nme.stripModuleSuffix(outerName)
        else outerName
      }
    }

    def innerName(innerSym: Symbol): String = {
      if (innerSym.isAnonymousClass || innerSym.isAnonymousFunction)
        null
      else
        innerSym.rawname + innerSym.moduleSuffix
    }

    val flagsWithFinal: Int = mkFlags(
      if (innerSym.rawowner.hasModuleFlag) asm.Opcodes.ACC_STATIC else 0,
      javaFlags(innerSym),
      if (isDeprecated(innerSym)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo-access flag
    ) & (INNER_CLASSES_FLAGS | asm.Opcodes.ACC_DEPRECATED)
    val flags = if (innerSym.isModuleClass) flagsWithFinal & ~asm.Opcodes.ACC_FINAL else flagsWithFinal // For SI-5676, object overriding.

    val jname = innerSym.javaBinaryName.toString // never null
    val oname = { // null when method-enclosed
      val on = outerName(innerSym)
      if (on == null) null else on.toString
    }
    val iname = { // null for anonymous inner class
      val in = innerName(innerSym)
      if (in == null) null else in.toString
    }

    InnerClassEntry(jname, oname, iname, flags)
  }

  // --------------------------------------------
  // ---------------- Java flags ----------------
  // --------------------------------------------

  /*
   * can-multi-thread
   */
  final def hasPublicBitSet(flags: Int) = ((flags & asm.Opcodes.ACC_PUBLIC) != 0)

  /*
   * must-single-thread
   */
  final def isRemote(s: Symbol) = (s hasAnnotation definitions.RemoteAttr)

  /*
   * Return the Java modifiers for the given symbol.
   * Java modifiers for classes:
   *  - public, abstract, final, strictfp (not used)
   * for interfaces:
   *  - the same as for classes, without 'final'
   * for fields:
   *  - public, private (*)
   *  - static, final
   * for methods:
   *  - the same as for fields, plus:
   *  - abstract, synchronized (not used), strictfp (not used), native (not used)
   *
   *  (*) protected cannot be used, since inner classes 'see' protected members,
   *      and they would fail verification after lifted.
   *
   * must-single-thread
   */
  def javaFlags(sym: Symbol): Int = {
    // constructors of module classes should be private
    // PP: why are they only being marked private at this stage and not earlier?
    val privateFlag =
      sym.isPrivate || (sym.isPrimaryConstructor && isTopLevelModule(sym.owner))

    // Final: the only fields which can receive ACC_FINAL are eager vals.
    // Neither vars nor lazy vals can, because:
    //
    // Source: http://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.5.3
    // "Another problem is that the specification allows aggressive
    // optimization of final fields. Within a thread, it is permissible to
    // reorder reads of a final field with those modifications of a final
    // field that do not take place in the constructor."
    //
    // A var or lazy val which is marked final still has meaning to the
    // scala compiler. The word final is heavily overloaded unfortunately;
    // for us it means "not overridable". At present you can't override
    // vars regardless; this may change.
    //
    // The logic does not check .isFinal (which checks flags for the FINAL flag,
    // and includes symbols marked lateFINAL) instead inspecting rawflags so
    // we can exclude lateFINAL. Such symbols are eligible for inlining, but to
    // avoid breaking proxy software which depends on subclassing, we do not
    // emit ACC_FINAL.
    // Nested objects won't receive ACC_FINAL in order to allow for their overriding.

    val finalFlag = (
         (((sym.rawflags & symtab.Flags.FINAL) != 0) || isTopLevelModule(sym))
      && !sym.enclClass.isInterface
      && !sym.isClassConstructor
      && !sym.isMutable // lazy vals and vars both
    )

    // Primitives are "abstract final" to prohibit instantiation
    // without having to provide any implementations, but that is an
    // illegal combination of modifiers at the bytecode level so
    // suppress final if abstract if present.
    import asm.Opcodes._
    mkFlags(
      if (privateFlag) ACC_PRIVATE else ACC_PUBLIC,
      if (sym.isDeferred || sym.hasAbstractFlag) ACC_ABSTRACT else 0,
      if (sym.isInterface) ACC_INTERFACE else 0,
      if (finalFlag && !sym.hasAbstractFlag) ACC_FINAL else 0,
      if (sym.isStaticMember) ACC_STATIC else 0,
      if (sym.isBridge) ACC_BRIDGE | ACC_SYNTHETIC else 0,
      if (sym.isArtifact) ACC_SYNTHETIC else 0,
      if (sym.isClass && !sym.isInterface) ACC_SUPER else 0,
      if (sym.isVarargsMethod) ACC_VARARGS else 0,
      if (sym.hasFlag(symtab.Flags.SYNCHRONIZED)) ACC_SYNCHRONIZED else 0
    )
  }

  /*
   * must-single-thread
   */
  def javaFieldFlags(sym: Symbol) = {
    javaFlags(sym) | mkFlags(
      if (sym hasAnnotation definitions.TransientAttr) asm.Opcodes.ACC_TRANSIENT else 0,
      if (sym hasAnnotation definitions.VolatileAttr)  asm.Opcodes.ACC_VOLATILE  else 0,
      if (sym.isMutable) 0 else asm.Opcodes.ACC_FINAL
    )
  }

} // end of class BCodeTypes
