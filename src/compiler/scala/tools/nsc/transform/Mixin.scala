/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.annotation.tailrec
import scala.collection.mutable

trait AccessorSynthesis extends Transform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  val EmptyThicket = EmptyTree
  def Thicket(trees: List[Tree]) = if (trees.isEmpty) EmptyTree else Block(trees, EmptyTree)
  def mustExplodeThicket(tree: Tree): Boolean =
    tree match {
      case EmptyTree => true
      case Block(_, EmptyTree) => true
      case _ => false
    }
  def explodeThicket(tree: Tree): List[Tree] = tree match {
    case EmptyTree                 => Nil
    case Block(thicket, EmptyTree) => thicket
    case stat                      => stat :: Nil
  }


  trait AccessorTreeSynthesis {
    protected def typedPos(pos: Position)(tree: Tree): Tree

    // used while we still need to synthesize some accessors in mixins: paramaccessors and presupers
    class UncheckedAccessorSynth(protected val clazz: Symbol){
      protected val _newDefs = mutable.ListBuffer[Tree]()

      def newDefs = _newDefs.toList

      /** Add tree at given position as new definition */
      protected def addDef(tree: ValOrDefDef): Unit = _newDefs += typedPos(position(tree.symbol))(tree)

      /** The position of given symbol, or, if this is undefined,
        * the position of the current class.
        */
      private def position(sym: Symbol) = if (sym.pos == NoPosition) clazz.pos else sym.pos

      /** Add new method definition.
        *
        * @param sym The method symbol.
        * @param rhs The method body.
        */
      def addDefDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(DefDef(sym, rhs))
      def addValDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(ValDef(sym, rhs))

      /** Complete `stats` with init checks and bitmaps,
        * removing any abstract method definitions in `stats` that are
        * matched by some symbol defined by a tree previously passed to `addDef`.
        */
      def implementWithNewDefs(stats: List[Tree]): List[Tree] = {
        val newDefs = _newDefs.toList
        val newSyms = newDefs map (_.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol
            !(sym.isDeferred &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ => true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: (stats filter isNotDuplicate)
      }

      def accessorBody(sym: Symbol) =
        if (sym.isSetter) setterBody(sym, sym.getterIn(clazz)) else getterBody(sym)

      protected def getterBody(getter: Symbol): Tree = {
        assert(getter.isGetter)
        assert(getter.hasFlag(PARAMACCESSOR))

        fieldAccess(getter)
      }

      protected def setterBody(setter: Symbol, getter: Symbol): Tree = {
        assert(getter.hasFlag(PARAMACCESSOR), s"missing implementation for non-paramaccessor $setter in $clazz")

        Assign(fieldAccess(setter), Ident(setter.firstParam))
      }

      private def fieldAccess(accessor: Symbol) =
        Select(This(clazz), accessor.accessed)

    }
  }

  case class BitmapInfo(symbol: Symbol, mask: Literal) {
    def storageClass: ClassSymbol = symbol.info.typeSymbol.asClass
  }


  // TODO: better way to communicate from info transform to tree transfor?
  private[this] val _bitmapInfo  = perRunCaches.newMap[Symbol, BitmapInfo]
  private[this] val _slowPathFor = perRunCaches.newMap[Symbol, Symbol]()

  def checkedAccessorSymbolSynth(clz: Symbol) =
    if (settings.checkInit) new CheckInitAccessorSymbolSynth { val clazz = clz }
    else new CheckedAccessorSymbolSynth { val clazz = clz }

  // base trait, with enough functionality for lazy vals -- CheckInitAccessorSymbolSynth adds logic for -Xcheckinit
  trait CheckedAccessorSymbolSynth {
    protected val clazz: Symbol

    protected def defaultPos = clazz.pos.focus
    protected def isTrait    = clazz.isTrait
    protected def hasTransientAnnot(field: Symbol) = field.accessedOrSelf hasAnnotation TransientAttr

    def needsBitmap(sym: Symbol): Boolean = !(isTrait || sym.isDeferred) && sym.isMethod && sym.isLazy && !sym.isSpecialized


    /** Examines the symbol and returns a name indicating what brand of
      * bitmap it requires.  The possibilities are the BITMAP_* vals
      * defined in StdNames.  If it needs no bitmap, nme.NO_NAME.
      *
      * bitmaps for checkinit fields are not inherited
      */
    protected def bitmapCategory(sym: Symbol): Name = {
      // ensure that nested objects are transformed TODO: still needed?
      sym.initialize

      import nme._

      if (needsBitmap(sym) && sym.isLazy)
        if (hasTransientAnnot(sym)) BITMAP_TRANSIENT else BITMAP_NORMAL
      else NO_NAME
    }


    def bitmapFor(sym: Symbol): BitmapInfo = _bitmapInfo(sym)
    protected def hasBitmap(sym: Symbol): Boolean = _bitmapInfo isDefinedAt sym


    /** Fill the map from fields to bitmap infos.
      *
      * Instead of field symbols, the map keeps their getter symbols. This makes code generation easier later.
      */
    def computeBitmapInfos(decls: List[Symbol]): List[Symbol] = {
      def doCategory(fields: List[Symbol], category: Name) = {
        val nbFields = fields.length // we know it's > 0
        val (bitmapClass, bitmapCapacity) =
        if (nbFields == 1)       (BooleanClass, 1)
        else if (nbFields <= 8)  (ByteClass, 8)
        else if (nbFields <= 32) (IntClass, 32)
        else (LongClass, 64)

        // 0-based index of highest bit, divided by bits per bitmap
        // note that this is only ever > 0 when bitmapClass == LongClass
        val maxBitmapNumber = (nbFields - 1) / bitmapCapacity

        // transient fields get their own category
        val isTransientCategory = fields.head hasAnnotation TransientAttr

        val bitmapSyms =
          (0 to maxBitmapNumber).toArray map { bitmapNumber =>
            val bitmapSym = (
              clazz.newVariable(nme.newBitmapName(category, bitmapNumber).toTermName, defaultPos)
                setInfo bitmapClass.tpe
                setFlag PrivateLocal | NEEDS_TREES
              )

            bitmapSym addAnnotation VolatileAttr

            if (isTransientCategory) bitmapSym addAnnotation TransientAttr

            bitmapSym
          }

        fields.zipWithIndex foreach { case (f, idx) =>
          val bitmapIdx = idx / bitmapCapacity
          val offsetInBitmap = idx % bitmapCapacity
          val mask =
            if (bitmapClass == LongClass) Constant(1L << offsetInBitmap)
            else Constant(1 << offsetInBitmap)

          _bitmapInfo(f) = BitmapInfo(bitmapSyms(bitmapIdx), Literal(mask))
        }

        bitmapSyms
      }

      decls groupBy bitmapCategory flatMap {
        case (category, fields) if category != nme.NO_NAME && fields.nonEmpty => doCategory(fields, category)
        case _ => Nil
      } toList
    }

    def slowPathFor(lzyVal: Symbol): Symbol = _slowPathFor(lzyVal)

    def newSlowPathSymbol(lzyVal: Symbol): Symbol = {
      val pos = if (lzyVal.pos != NoPosition) lzyVal.pos else defaultPos // TODO: is the else branch ever taken?
      val sym = clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), pos, PRIVATE) setInfo MethodType(Nil, lzyVal.tpe.resultType)
      _slowPathFor(lzyVal) = sym
      sym
    }

  }

  trait CheckInitAccessorSymbolSynth extends CheckedAccessorSymbolSynth {
    /** Does this field require an initialized bit?
      * Note: fields of classes inheriting DelayedInit are not checked.
      * This is because they are neither initialized in the constructor
      * nor do they have a setter (not if they are vals anyway). The usual
      * logic for setting bitmaps does therefore not work for such fields.
      * That's why they are excluded.
      * Note: The `checkinit` option does not check if transient fields are initialized.
      */
    protected def needsInitFlag(sym: Symbol): Boolean =
    sym.isGetter &&
      !( sym.isInitializedToDefault
        || isConstantType(sym.info.finalResultType) // SI-4742
        || sym.hasFlag(PARAMACCESSOR | SPECIALIZED | LAZY)
        || sym.accessed.hasFlag(PRESUPER)
        || sym.isOuterAccessor
        || (sym.owner isSubClass DelayedInitClass)
        || (sym.accessed hasAnnotation TransientAttr))

    /** Examines the symbol and returns a name indicating what brand of
      * bitmap it requires.  The possibilities are the BITMAP_* vals
      * defined in StdNames.  If it needs no bitmap, nme.NO_NAME.
      *
      * bitmaps for checkinit fields are not inherited
      */
    override protected def bitmapCategory(sym: Symbol): Name = {
      import nme._

      super.bitmapCategory(sym) match {
        case NO_NAME if needsInitFlag(sym) && !sym.isDeferred =>
          if (hasTransientAnnot(sym)) BITMAP_CHECKINIT_TRANSIENT else BITMAP_CHECKINIT
        case category => category
      }
    }

    override def needsBitmap(sym: Symbol): Boolean = super.needsBitmap(sym) || !(isTrait || sym.isDeferred) && needsInitFlag(sym)
  }


  // synthesize trees based on info gathered during info transform
  // (which are known to have been run because the tree transform runs afterOwnPhase)
  // since we can't easily share all info via symbols and flags, we have two maps above
  // (they are persisted even between phases because the -Xcheckinit logic runs during constructors)
  // TODO: can we use attachments instead of _bitmapInfo and _slowPathFor?
  trait CheckedAccessorTreeSynthesis extends AccessorTreeSynthesis {

    // note: we deal in getters here, not field symbols
    trait SynthCheckedAccessorsTreesInClass extends CheckedAccessorSymbolSynth {
      def isUnitGetter(sym: Symbol) = sym.tpe.resultType.typeSymbol == UnitClass
      def thisRef = gen.mkAttributedThis(clazz)

      /** Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
        * precise comparison operator depending on the value of 'equalToZero'.
        */
      def mkTest(field: Symbol, equalToZero: Boolean = true): Tree = {
        val bitmap = bitmapFor(field)
        val bitmapTree = thisRef DOT bitmap.symbol

        if (bitmap.storageClass == BooleanClass) {
          if (equalToZero) NOT(bitmapTree) else bitmapTree
        } else {
          val lhs = bitmapTree GEN_&(bitmap.mask, bitmap.storageClass)
          if (equalToZero) lhs GEN_==(ZERO, bitmap.storageClass)
          else lhs GEN_!=(ZERO, bitmap.storageClass)
        }
      }

      /** Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      def mkSetFlag(valSym: Symbol): Tree = {
        val bitmap = bitmapFor(valSym)
        def x = thisRef DOT bitmap.symbol

        Assign(x,
          if (bitmap.storageClass == BooleanClass) TRUE
          else {
            val or = Apply(Select(x, getMember(bitmap.storageClass, nme.OR)), List(bitmap.mask))
            // NOTE: bitwise or (`|`) on two bytes yields and Int (TODO: why was this not a problem when this ran during mixins?)
            // TODO: need this to make it type check -- is there another way??
            if (bitmap.storageClass != LongClass) Apply(Select(or, newTermName("to" + bitmap.storageClass.name)), Nil)
            else or
          }
        )
      }
    }

    class SynthLazyAccessorsIn(protected val clazz: Symbol) extends SynthCheckedAccessorsTreesInClass {
      /**
        * The compute method (slow path) looks like:
        *
        * ```
        * def l$compute() = {
        *   synchronized(this) {
        *     if ((bitmap$n & MASK) == 0) {
        *      init // l$ = <rhs>
        *      bitmap$n = bimap$n | MASK
        *     }
        *   }
        *   ...
        *   this.f1 = null
        *   ...
        *   this.fn = null
        *   l$
        * }
        * ```
        *
        * `bitmap$n` is a byte, int or long value acting as a bitmap of initialized values.
        * The kind of the bitmap determines how many bit indicators for lazy vals are stored in it.
        * For Int bitmap it is 32 and then 'n' in the above code is: (offset / 32),
        * the MASK is (1 << (offset % 32)).
        *
        * If the class contains only a single lazy val then the bitmap is
        * represented as a Boolean and the condition checking is a simple bool test.
        *
        * Private fields used only in this initializer are subsequently set to null.
        *
        * For performance reasons the double-checked locking is split into two parts,
        * the first (fast) path checks the bitmap without synchronizing, and if that
        * fails it initializes the lazy val within the synchronization block (slow path).
        *
        * This way the inliner should optimize the fast path because the method body is small enough.
        */
      def expandLazyClassMember(lazyVar: Symbol, lazyAccessor: Symbol, transformedRhs: Tree, nullables: Map[Symbol, List[Symbol]]): Tree = {
        // use cast so that specialization can turn null.asInstanceOf[T] into null.asInstanceOf[Long]
        def nullify(sym: Symbol) =
          Select(thisRef, sym.accessedOrSelf) === gen.mkAsInstanceOf(NULL, sym.info.resultType)

        val nulls = nullables.getOrElse(lazyAccessor, Nil) map nullify

        if (nulls.nonEmpty)
          log("nulling fields inside " + lazyAccessor + ": " + nulls)

        val slowPathSym  = slowPathFor(lazyAccessor)
        val rhsAtSlowDef = transformedRhs.changeOwner(lazyAccessor -> slowPathSym)

        val isUnit    = isUnitGetter(lazyAccessor)
        val selectVar = if (isUnit) UNIT         else Select(thisRef, lazyVar)
        val storeRes  = if (isUnit) rhsAtSlowDef else Assign(selectVar, rhsAtSlowDef)

        val synchedStats = storeRes :: mkSetFlag(lazyAccessor) :: Nil
        val slowPathRhs =
          Block(List(gen.mkSynchronizedCheck(thisRef, mkTest(lazyAccessor), synchedStats, nulls)), selectVar)

        // The lazy accessor delegates to the compute method if needed, otherwise just accesses the var (it was initialized previously)
        // `if ((bitmap&n & MASK) == 0) this.l$compute() else l$`
        val accessorRhs = If(mkTest(lazyAccessor), Apply(Select(thisRef, slowPathSym), Nil), selectVar)

        afterOwnPhase { // so that we can assign to vals
          Thicket(List((DefDef(slowPathSym, slowPathRhs)), DefDef(lazyAccessor, accessorRhs)) map typedPos(lazyAccessor.pos.focus))
        }
      }
    }

    /** Map lazy values to the fields they should null after initialization. */
    // TODO: fix
    def lazyValNullables(clazz: Symbol, templStats: List[Tree]): Map[Symbol, List[Symbol]] = {
      // if there are no lazy fields, take the fast path and save a traversal of the whole AST
      if (!clazz.info.decls.exists(_.isLazy)) Map()
      else {
        // A map of single-use fields to the lazy value that uses them during initialization.
        // Each field has to be private and defined in the enclosing class, and there must
        // be exactly one lazy value using it.
        //
        // Such fields will be nulled after the initializer has memoized the lazy value.
        val singleUseFields: Map[Symbol, List[Symbol]] = {
          val usedIn = mutable.HashMap[Symbol, List[Symbol]]() withDefaultValue Nil

          object SingleUseTraverser extends Traverser {
            override def traverse(tree: Tree) {
              tree match {
                // assignment targets don't count as a dereference -- only check the rhs
                case Assign(_, rhs) => traverse(rhs)
                case tree: RefTree if tree.symbol != NoSymbol =>
                  val sym = tree.symbol
                  // println(s"$sym in ${sym.owner} from $currentOwner ($tree)")
                  if ((sym.hasAccessorFlag || (sym.isTerm && !sym.isMethod)) && sym.isPrivate && !sym.isLazy // non-lazy private field or its accessor
                    && !definitions.isPrimitiveValueClass(sym.tpe.resultType.typeSymbol) // primitives don't hang on to significant amounts of heap
                    && sym.owner == currentOwner.enclClass && !(currentOwner.isGetter && currentOwner.accessed == sym)) {

                    // println("added use in: " + currentOwner + " -- " + tree)
                    usedIn(sym) ::= currentOwner
                  }
                  super.traverse(tree)
                case _ => super.traverse(tree)
              }
            }
          }
          templStats foreach SingleUseTraverser.apply
          // println("usedIn: " + usedIn)

          // only consider usages from non-transient lazy vals (SI-9365)
          val singlyUsedIn = usedIn filter { case (_, member :: Nil) => member.isLazy && !member.accessed.hasAnnotation(TransientAttr) case _ => false } toMap

          // println("singlyUsedIn: " + singlyUsedIn)
          singlyUsedIn
        }

        val map = mutable.Map[Symbol, Set[Symbol]]() withDefaultValue Set()
        // invert the map to see which fields can be nulled for each non-transient lazy val
        for ((field, users) <- singleUseFields; lazyFld <- users) map(lazyFld) += field

        map.mapValues(_.toList sortBy (_.id)).toMap
      }
    }


    class SynthInitCheckedAccessorsIn(protected val clazz: Symbol) extends SynthCheckedAccessorsTreesInClass with CheckInitAccessorSymbolSynth {
      private object addInitBitsTransformer extends Transformer {
        private def checkedGetter(lhs: Tree)(pos: Position) = {
          val getter = clazz.info decl lhs.symbol.getterName suchThat (_.isGetter)
          if (hasBitmap(getter) && needsInitFlag(getter)) {
            debuglog("adding checked getter for: " + getter + " " + lhs.symbol.flagString)
            List(typedPos(pos)(mkSetFlag(getter)))
          }
          else Nil
        }
        override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
          // !!! Ident(self) is never referenced, is it supposed to be confirming
          // that self is anything in particular?
          super.transformStats(
            stats flatMap {
              case stat@Assign(lhs@Select(This(_), _), rhs) => stat :: checkedGetter(lhs)(stat.pos.focus)
              // remove initialization for default values -- TODO is this case ever hit? constructors does not generate Assigns with EmptyTree for the rhs AFAICT
              case Apply(lhs@Select(Ident(self), _), EmptyTree.asList) if lhs.symbol.isSetter => Nil
              case stat                                                                       => List(stat)
            },
            exprOwner
          )
        }
      }

      /** Make getters check the initialized bit, and the class constructor & setters are changed to set the initialized bits. */
      def wrapRhsWithInitChecks(sym: Symbol)(rhs: Tree): Tree = {
        // Add statements to the body of a constructor to set the 'init' bit for each field initialized in the constructor
        if (sym.isConstructor) addInitBitsTransformer transform rhs
        else if (isTrait || rhs == EmptyTree) rhs
        else if (needsInitFlag(sym)) // getter
          mkCheckedAccessorRhs(if (isUnitGetter(sym)) UNIT else rhs, rhs.pos, sym)
        else if (sym.isSetter) {
          val getter = sym.getterIn(clazz)
          if (needsInitFlag(getter)) Block(List(rhs, typedPos(rhs.pos.focus)(mkSetFlag(getter))), UNIT)
          else rhs
        }
        else rhs
      }

      private def mkCheckedAccessorRhs(retVal: Tree, pos: Position, getter: Symbol): Tree = {
        val msg = s"Uninitialized field: ${clazz.sourceFile}: ${pos.line}"
        val result =
          IF(mkTest(getter, equalToZero = false)).
            THEN(retVal).
            ELSE(Throw(NewFromConstructor(UninitializedFieldConstructor, LIT(msg))))

        typedPos(pos)(BLOCK(result, retVal))
      }
    }
  }
}

abstract class Mixin extends InfoTransform with ast.TreeDSL with AccessorSynthesis {
  import global._
  import definitions._
  import CODE._


  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** Some trait methods need to be implemented in subclasses, so they cannot be private.
    *
    * We used to publicize during explicitouter (for some reason), so the condition is a bit more involved now it's done here
    * (need to exclude lambdaLIFTED methods, as they do no exist during explicitouter and thus did not need to be excluded...)
    *
    * They may be protected, now that traits are compiled 1:1 to interfaces.
    * The same disclaimers about mapping Scala's notion of visibility to Java's apply:
    * we cannot emit PROTECTED methods in interfaces on the JVM,
    * but knowing that these trait methods are protected means we won't emit static forwarders.
    *
    * JVMLS: "Methods of interfaces may have any of the flags in Table 4.6-A set
    * except ACC_PROTECTED, ACC_FINAL, ACC_SYNCHRONIZED, and ACC_NATIVE (JLS §9.4)."
    *
    * TODO: can we just set the right flags from the start??
    *  could we use the final flag to indicate a private method is really-really-private?
    */
  def publicizeTraitMethod(sym: Symbol): Unit = {
    if ((sym hasFlag PRIVATE) && !(sym hasFlag LIFTED) && ( // lambdalifted methods can remain private
        // super accessors by definition must be implemented in a subclass, so can't be private
        // TODO: why are they ever private in a trait to begin with!?!? (could just name mangle them to begin with)
        // TODO: can we add the SYNTHESIZE_IMPL_IN_SUBCLASS flag to super accessors symbols?
        (sym hasFlag SUPERACCESSOR)
        // an accessor / module *may* need to be implemented in a subclass, and thus cannot be private
        // TODO: document how we get here (lambdalift? fields has already made accessors not-private)
        || (sym hasFlag ACCESSOR | MODULE) && (sym hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS)))
      sym.makeNotPrivate(sym.owner)

    // no need to make trait methods not-protected
    // (we used to have to move them to another class when interfaces could not have concrete methods)
    // see note in `synthFieldsAndAccessors` in Fields.scala
    // if (sym hasFlag PROTECTED) sym setFlag notPROTECTED
  }

  /** This map contains a binding (class -> info) if
   *  the class with this info at phase mixinPhase has been treated for mixin composition
   */
  private val treatedClassInfos = perRunCaches.newMap[Symbol, Type]() withDefaultValue NoType


// --------- helper functions -----------------------------------------------

  /** A member of a trait is implemented statically if its implementation after the
   *  mixin transform is RHS of the method body (destined to be in a interface default method)
   *
   *  To be statically implemented, a member must be a method that belonged to the trait's implementation class
   *  before (i.e. it is not abstract). Not statically implemented are
   *   - non-private modules: these are implemented directly in the mixin composition class
   *     (private modules, on the other hand, are implemented statically, but their
   *      module variable is not. all such private modules are lifted, because
   *      non-lifted private modules have been eliminated in ExplicitOuter)
   *   - field accessors and superaccessors, except for lazy value accessors which become initializer
   *     methods in the impl class (because they can have arbitrary initializers)
   */
  private def isImplementedStatically(sym: Symbol) = (
    (sym.isMethod || ((sym hasFlag MODULE) && !sym.isStatic))
    && notDeferred(sym)
    && sym.owner.isTrait
    && (!sym.isModule || sym.hasFlag(PRIVATE | LIFTED))
    && (!(sym hasFlag (ACCESSOR | SUPERACCESSOR)) || sym.isLazy)
    && !sym.isPrivate
    && !sym.hasAllFlags(LIFTED | MODULE | METHOD)
    && !sym.isConstructor
    && (!sym.hasFlag(notPRIVATE | LIFTED) || sym.hasFlag(ACCESSOR | SUPERACCESSOR | MODULE))
  )



  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param member     The symbol statically referred to by the superaccessor in the trait
   *  @param mixinClass The mixin class that produced the superaccessor
   */
  private def rebindSuper(base: Symbol, member: Symbol, mixinClass: Symbol): Symbol =
    exitingSpecialize {
      var bcs = base.info.baseClasses.dropWhile(mixinClass != _).tail
      var sym: Symbol = NoSymbol
      debuglog("starting rebindsuper " + base + " " + member + ":" + member.tpe +
            " " + mixinClass + " " + base.info.baseClasses + "/" + bcs)
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (settings.debug) {
          val other = bcs.head.info.nonPrivateDecl(member.name)
          debuglog("rebindsuper " + bcs.head + " " + other + " " + other.tpe +
              " " + other.isDeferred)
        }
        sym = member.matchingSymbol(bcs.head, base.thisType).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE))
        bcs = bcs.tail
      }
      sym
    }

// --------- type transformation -----------------------------------------------

  @inline final def notDeferred(sym: Symbol) = fields.notDeferredOrSynthImpl(sym)

  /** Is member overridden (either directly or via a bridge) in base class sequence `bcs`? */
  def isOverriddenAccessor(member: Symbol, bcs: List[Symbol]): Boolean = beforeOwnPhase {
    def hasOverridingAccessor(clazz: Symbol) = {
      clazz.info.nonPrivateDecl(member.name).alternatives.exists(
        sym =>
          sym.hasFlag(ACCESSOR) &&
          !sym.hasFlag(MIXEDIN) &&
          notDeferred(sym) &&
          matchesType(sym.tpe, member.tpe, alwaysMatchSimple = true))
    }
    (    bcs.head != member.owner
      && (hasOverridingAccessor(bcs.head) || isOverriddenAccessor(member, bcs.tail))
    )
  }


  /** Add given member to given class, and mark member as mixed-in.
   */
  def addMember(clazz: Symbol, member: Symbol): Symbol = {
    debuglog(s"mixing into $clazz: ${member.defString}")
    // This attachment is used to instruct the backend about which methids in traits require
    // a static trait impl method. We remove this from the new symbol created for the method
    // mixed into the subclass.
    member.removeAttachment[NeedStaticImpl.type]
    clazz.info.decls enter member setFlag MIXEDIN resetFlag JAVA_DEFAULTMETHOD
  }
  def cloneAndAddMember(mixinClass: Symbol, mixinMember: Symbol, clazz: Symbol): Symbol =
    addMember(clazz, cloneBeforeErasure(mixinClass, mixinMember, clazz))

  def cloneBeforeErasure(mixinClass: Symbol, mixinMember: Symbol, clazz: Symbol): Symbol = {
    val newSym = enteringErasure {
      // since we used `mixinMember` from the interface that represents the trait that's
      // being mixed in, have to instantiate the interface type params (that may occur in mixinMember's
      // info) as they are seen from the class.  We can't use the member that we get from the
      // implementation class, as it's a clone that was made after erasure, and thus it does not
      // know its info at the beginning of erasure anymore.
      val sym = mixinMember cloneSymbol clazz

      val erasureMap = erasure.erasure(mixinMember)
      val erasedInterfaceInfo: Type = erasureMap(mixinMember.info)
      val specificForwardInfo       = (clazz.thisType baseType mixinClass) memberInfo mixinMember
      val forwarderInfo =
        if (erasureMap(specificForwardInfo) =:= erasedInterfaceInfo)
          specificForwardInfo
        else {
          erasedInterfaceInfo
        }
      // Optimize: no need if mixinClass has no typeparams.
      // !!! JZ Really? What about the effect of abstract types, prefix?
      if (mixinClass.typeParams.isEmpty) sym
      else sym modifyInfo (_ => forwarderInfo)
    }
    newSym
  }

  def publicizeTraitMethods(clazz: Symbol) {
    if (treatedClassInfos(clazz) != clazz.info) {
      treatedClassInfos(clazz) = clazz.info
      assert(phase == currentRun.mixinPhase, phase)

      for (member <- clazz.info.decls) {
        if (member.isMethod) publicizeTraitMethod(member)
        else {
          assert(member.isTerm && !member.isDeferred, member)
          // disable assert to support compiling against code compiled by an older compiler (until we re-starr)
          // assert(member hasFlag LAZY | PRESUPER, s"unexpected $member in $clazz ${member.debugFlagString}")
          // lazy vals still leave field symbols lying around in traits -- TODO: never emit them to begin with
          // ditto for early init vals
          clazz.info.decls.unlink(member)
        }

      }
      debuglog("new defs of " + clazz + " = " + clazz.info.decls)
    }
  }

  /** Add all members to be mixed in into a (non-trait-) class
   *  These are:
   *    for every mixin trait T that is not also inherited by the superclass:
   *     add late interface members to T and then:
   *      - if a member M of T is forwarded to the implementation class, add
   *        a forwarder for M unless one exists already.
   *        The alias of the forwarder is the static member it forwards to.
   *      - for every abstract accessor in T, add a field and an implementation for that accessor
   *      - for every super accessor in T, add an implementation of that accessor
   *      - for every module in T, add a module
   */
  def addMixedinMembers(clazz: Symbol, unit: CompilationUnit) {
    def cloneAndAddMixinMember(mixinClass: Symbol, mixinMember: Symbol): Symbol = (
      cloneAndAddMember(mixinClass, mixinMember, clazz)
           setPos clazz.pos
        resetFlag DEFERRED
    )

    /* Mix in members of implementation class mixinClass into class clazz */
    def mixinTraitForwarders(mixinClass: Symbol) {
      for (member <- mixinClass.info.decls ; if isImplementedStatically(member)) {
        member overridingSymbol clazz match {
          case NoSymbol =>
            val isMemberOfClazz = clazz.info.findMember(member.name, 0, 0L, stableOnly = false).alternatives.contains(member)
            if (isMemberOfClazz) {
              def genForwarder(): Unit = {
                cloneAndAddMixinMember(mixinClass, member).asInstanceOf[TermSymbol] setAlias member
              }

              if (settings.XgenMixinForwarders) genForwarder()
              else {

                // `member` is a concrete method defined in `mixinClass`, which is a base class of
                // `clazz`, and the method is not overridden in `clazz`. A forwarder is needed if:
                //
                //   - A non-trait base class of `clazz` defines a matching method. Example:
                //       class C {def f: Int}; trait T extends C {def f = 1}; class D extends T
                //     Even if C.f is abstract, the forwarder in D is needed, otherwise the JVM would
                //     resolve `D.f` to `C.f`, see jvms-6.5.invokevirtual.
                //
                //   - There exists another concrete, matching method in a parent interface `p` of
                //     `clazz`, and the `mixinClass` does not itself extend `p`. In this case the
                //     forwarder is needed to disambiguate. Example:
                //       trait T1 {def f = 1}; trait T2 extends T1 {override def f = 2}; class C extends T2
                //     In C we don't need a forwarder for f because T2 extends T1, so the JVM resolves
                //     C.f to T2.f non-ambiguously. See jvms-5.4.3.3, "maximally-specific method".
                //       trait U1 {def f = 1}; trait U2 {self:U1 => override def f = 2}; class D extends U2
                //     In D the forwarder is needed, the interfaces U1 and U2 are unrelated at the JVM
                //     level.

                @tailrec
                def existsCompetingMethod(baseClasses: List[Symbol]): Boolean = baseClasses match {
                  case baseClass :: rest =>
                    if (baseClass ne mixinClass) {
                      val m = member.overriddenSymbol(baseClass)
                      val isCompeting = m.exists && {
                        !m.owner.isTraitOrInterface ||
                          (!m.isDeferred && !mixinClass.isNonBottomSubClass(m.owner))
                      }
                      isCompeting || existsCompetingMethod(rest)
                    } else existsCompetingMethod(rest)

                  case _ => false
                }

                if (existsCompetingMethod(clazz.baseClasses))
                  genForwarder()
                else if (!settings.nowarnDefaultJunitMethods && JUnitTestClass.exists && member.hasAnnotation(JUnitTestClass))
                  warning(member.pos, "JUnit tests in traits that are compiled as default methods are not executed by JUnit 4. JUnit 5 will fix this issue.")
              }
            }

          case _        =>
        }
      }
    }

    /* Mix in members of trait mixinClass into class clazz.
     */
    def mixinTraitMembers(mixinClass: Symbol) {
      // For all members of a trait's interface do:
      for (mixinMember <- mixinClass.info.decls) {
        if (mixinMember.hasFlag(SUPERACCESSOR)) { // mixin super accessors
          val superAccessor = addMember(clazz, mixinMember.cloneSymbol(clazz)) setPos clazz.pos
          assert(superAccessor.alias != NoSymbol, superAccessor)

          rebindSuper(clazz, mixinMember.alias, mixinClass) match {
            case NoSymbol =>
              reporter.error(clazz.pos, "Member %s of mixin %s is missing a concrete super implementation.".format(
                mixinMember.alias, mixinClass))
            case alias1 =>
              if (alias1.owner.isJavaDefined && alias1.owner.isInterface && !clazz.parentSymbols.contains(alias1.owner)) {
                val suggestedParent = exitingTyper(clazz.info.baseType(alias1.owner))
                reporter.error(clazz.pos, s"Unable to implement a super accessor required by trait ${mixinClass.name} unless $suggestedParent is directly extended by $clazz.")
              }
              superAccessor.asInstanceOf[TermSymbol] setAlias alias1
          }
        }
        else if (mixinMember.hasFlag(ACCESSOR) && notDeferred(mixinMember)
                 && (mixinMember hasFlag PARAMACCESSOR)
                 && !isOverriddenAccessor(mixinMember, clazz.info.baseClasses)) {
          // mixin accessor for constructor parameter
          // (note that a paramaccessor cannot have a constant type as it must have a user-defined type)
          cloneAndAddMixinMember(mixinClass, mixinMember)

          val name = mixinMember.name

          if (!nme.isSetterName(name)) {
            // enteringPhase: the private field is moved to the implementation class by erasure,
            // so it can no longer be found in the mixinMember's owner (the trait)
            val accessed = enteringPickler(mixinMember.accessed)
            // #3857, need to retain info before erasure when cloning (since cloning only
            // carries over the current entry in the type history)
            val sym = enteringErasure {
              // so we have a type history entry before erasure
              clazz.newValue(mixinMember.localName, mixinMember.pos).setInfo(mixinMember.tpe.resultType)
            }
            sym updateInfo mixinMember.tpe.resultType // info at current phase

            val newFlags = (
              (PrivateLocal)
              | (mixinMember getFlag MUTABLE)
              | (if (mixinMember.hasStableFlag) 0 else MUTABLE)
              )

            addMember(clazz, sym setFlag newFlags setAnnotations accessed.annotations)
          }
        }
      }
    }

    if (clazz.isJavaDefined || treatedClassInfos(clazz) == clazz.info)
      return

    treatedClassInfos(clazz) = clazz.info
    assert(!clazz.isTrait && clazz.info.parents.nonEmpty, clazz)

    // first complete the superclass with mixed in members
    addMixedinMembers(clazz.superClass, unit)

    for (mc <- clazz.mixinClasses ; if mc.isTrait) {
      // @SEAN: adding trait tracking so we don't have to recompile transitive closures
      unit.depends += mc
      publicizeTraitMethods(mc)
      mixinTraitMembers(mc)
      mixinTraitForwarders(mc)
    }
  }

  override def transformInfo(sym: Symbol, tp: Type): Type = tp

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer(unit)

  class MixinTransformer(unit : CompilationUnit) extends Transformer with AccessorTreeSynthesis {
    /** The typer */
    private var localTyper: erasure.Typer = _
    protected def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, rootMirror.RootClass, newScope)


    /** The first transform; called in a pre-order traversal at phase mixin
     *  (that is, every node is processed before its children).
     *  What transform does:
     *   - For every non-trait class, add all mixed in members to the class info.
     */
    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Template(parents, self, body) =>
          localTyper = erasure.newTyper(rootContext.make(tree, currentOwner))
          exitingMixin(currentOwner.owner.info)//todo: needed?

          if (!currentOwner.isTrait && !isPrimitiveValueClass(currentOwner))
            addMixedinMembers(currentOwner, unit)
          else if (currentOwner.isTrait)
            publicizeTraitMethods(currentOwner)

          tree

        case _ => tree
      }
    }


    /** Add all new definitions to a non-trait class
      *
      * These fall into the following categories:
      *    - for a trait interface:
      *       - abstract accessors for all paramaccessor or early initialized fields
      *    - for a non-trait class:
      *       - field and accessor implementations for each inherited paramaccessor or early initialized field
      *       - A super accessor for every super accessor in a mixin class
      *       - Forwarders for all methods that are implemented statically
      *
      * All superaccessors are completed with right-hand sides (@see completeSuperAccessor)
      *
      * @param clazz The class to which definitions are added
      */
    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val accessorSynth = new UncheckedAccessorSynth(clazz)
      import accessorSynth._

      // for all symbols `sym` in the class definition, which are mixed in by mixinTraitMembers
      for (sym <- clazz.info.decls ; if sym hasFlag MIXEDIN) {
        // if current class is a trait, add an abstract method for accessor `sym`
        // ditto for a super accessor (will get an RHS in completeSuperAccessor)
        if (clazz.isTrait || sym.isSuperAccessor) addDefDef(sym)
        // implement methods mixed in from a supertrait (the symbols were created by mixinTraitMembers)
        else if (sym.hasFlag(ACCESSOR) && !sym.hasFlag(DEFERRED)) {
          assert(sym hasFlag (PARAMACCESSOR), s"mixed in $sym from $clazz is not lazy/param?!?")

          // add accessor definitions
          addDefDef(sym, accessorBody(sym))
        }
        else if (!sym.isMethod) addValDef(sym) // field
        else if (!sym.isMacro) { // forwarder
          assert(sym.alias != NoSymbol, (sym, sym.debugFlagString, clazz))
          // debuglog("New forwarder: " + sym.defString + " => " + sym.alias.defString)
          addDefDef(sym, Apply(SuperSelect(clazz, sym.alias), sym.paramss.head.map(Ident(_))))
        }
      }

      val implementedAccessors = implementWithNewDefs(stats)

      if (clazz.isTrait)
        implementedAccessors filter {
          case vd: ValDef => assert(vd.symbol.hasFlag(PRESUPER | PARAMACCESSOR), s"unexpected valdef $vd in trait $clazz"); false
          case _ => true
        }
      else {
        /* If `stat` is a superaccessor, complete it by adding a right-hand side.
         * Note: superaccessors are always abstract until this point.
         *  The method to call in a superaccessor is stored in the accessor symbol's alias field.
         * The rhs is:
         *   super.A(xs)  where A is the super accessor's alias and xs are its formal parameters.
         * This rhs is typed and then mixin transformed.
         */
        def completeSuperAccessor(stat: Tree) = stat match {
          case DefDef(_, _, _, vparams :: Nil, _, EmptyTree) if stat.symbol.isSuperAccessor =>
            val body = atPos(stat.pos)(Apply(SuperSelect(clazz, stat.symbol.alias), vparams map (v => Ident(v.symbol))))
            val pt   = stat.symbol.tpe.resultType

            copyDefDef(stat)(rhs = enteringMixin(transform(localTyper.typed(body, pt))))
          case _ =>
            stat
        }

        implementedAccessors map completeSuperAccessor
      }
    }

    /** The transform that gets applied to a tree after it has been completely
     *  traversed and possible modified by a preTransform.
     *  This step will
     *    - change parents of templates to conform to parents in the symbol info
     *    - add all new definitions to a class or interface
     *    - remove widening casts
     *    - change calls to methods which are defined only in implementation classes
     *      to static calls of methods in implementation modules (@see staticCall)
     *    - change super calls to methods in implementation classes to static calls
     *      (@see staticCall)
     */
    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol

      tree match {
        case templ @ Template(parents, self, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)

          // add all new definitions to current class or interface
          val statsWithNewDefs = addNewDefs(currentOwner, body)
          statsWithNewDefs foreach {
            case dd: DefDef if isTraitMethodRequiringStaticImpl(dd) =>
              dd.symbol.updateAttachment(NeedStaticImpl)
            case _ =>
          }
          treeCopy.Template(tree, parents1, self, statsWithNewDefs)

        case Select(qual, name) if sym.owner.isTrait && !sym.isMethod =>
          assert(sym.hasFlag(PARAMACCESSOR | PRESUPER), s"!!! Unexpected reference to field $sym in trait $currentOwner")

          // refer to fields in some trait an abstract getter in the interface.
          val ifaceGetter = sym getterIn sym.owner

          if (ifaceGetter == NoSymbol) abort("No getter for " + sym + " in " + sym.owner)
          else typedPos(tree.pos)((qual DOT ifaceGetter)())

        case Assign(Apply(lhs @ Select(qual, _), List()), rhs) =>
          // assign to fields in some trait via an abstract setter in the interface.
          // Note that the case above has added the empty application.
          val setter = lhs.symbol.setterIn(lhs.symbol.owner.tpe.typeSymbol) setPos lhs.pos

          typedPos(tree.pos)((qual DOT setter)(rhs))

        case _ =>
          tree
      }
    }

    /** The main transform method.
     *  This performs pre-order traversal preTransform at mixin phase;
     *  when coming back, it performs a postTransform at phase after.
     */
    override def transform(tree: Tree): Tree = {
      val saved = localTyper
      val tree1 = super.transform(preTransform(tree))
      // localTyper needed when not flattening inner classes. parts after an
      // inner class will otherwise be typechecked with a wrong scope
      try exitingMixin(postTransform(tree1))
      finally localTyper = saved
    }
  }

  private def isTraitMethodRequiringStaticImpl(dd: DefDef): Boolean = {
    val sym = dd.symbol
    dd.rhs.nonEmpty &&
      sym.owner.isTrait &&
      !sym.isPrivate && // no need to put implementations of private methods into a static method
      !sym.hasFlag(Flags.STATIC)
  }

  case object NeedStaticImpl extends PlainAttachment
}
