/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL and Lightbend, Inc
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
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
      def expandLazyClassMember(lazyVar: global.Symbol, lazyAccessor: global.Symbol, transformedRhs: global.Tree): Tree = {
        val slowPathSym  = slowPathFor(lazyAccessor)
        val rhsAtSlowDef = transformedRhs.changeOwner(lazyAccessor -> slowPathSym)

        val isUnit    = isUnitGetter(lazyAccessor)
        val selectVar = if (isUnit) UNIT         else Select(thisRef, lazyVar)
        val storeRes  = if (isUnit) rhsAtSlowDef else Assign(selectVar, rhsAtSlowDef)

        def needsInit = mkTest(lazyAccessor)
        val doInit = Block(List(storeRes), mkSetFlag(lazyAccessor))
        // the slow part of double-checked locking (TODO: is this the most efficient pattern? https://github.come/scala/scala-dev/issues/204)
        val slowPathRhs = Block(gen.mkSynchronized(thisRef)(If(needsInit, doInit, EmptyTree)) :: Nil, selectVar)

        // The lazy accessor delegates to the compute method if needed, otherwise just accesses the var (it was initialized previously)
        // `if ((bitmap&n & MASK) == 0) this.l$compute() else l$`
        val accessorRhs = If(needsInit, Apply(Select(thisRef, slowPathSym), Nil), selectVar)

        afterOwnPhase { // so that we can assign to vals
          Thicket(List((DefDef(slowPathSym, slowPathRhs)), DefDef(lazyAccessor, accessorRhs)) map typedPos(lazyAccessor.pos.focus))
        }
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
