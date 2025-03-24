/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
        assert(getter.isGetter, s"$getter must be a getter")
        assert(getter.hasFlag(PARAMACCESSOR), s"$getter must be an accessor")

        fieldAccess(getter)
      }

      protected def setterBody(setter: Symbol, getter: Symbol): Tree = {
        assert(getter.hasFlag(PARAMACCESSOR), s"missing implementation for non-paramaccessor $setter in $clazz")
        // scala-dev#408: fields for locals captured in a trait are non-final. The lambdalift phase adds the
        // ConstructorNeedsFence attachment to the primary constructor of the class to ensure safe publication.
        setter.accessed.setFlag(MUTABLE)
        Assign(fieldAccess(setter), Ident(setter.firstParam))
      }

      private def fieldAccess(accessor: Symbol) =
        Select(This(clazz), accessor.accessed)

    }
  }

  case class BitmapInfo(symbol: Symbol, mask: Literal) {
    def select(on: This): Tree = Select(on, symbol)
    def applyToMask(on: This, op: Name): Tree  = Apply(member(select(on), op), List(mask))
    def member(bitmapRef: Tree, name: Name): Tree  = Select(bitmapRef, getMember(storageClass, name))
    def convert(bitmapRef: Tree): Tree = Apply(member(bitmapRef, newTermName("to" + storageClass.name)), Nil)

    def isLong: Boolean = storageClass == LongClass
    def isBoolean: Boolean = storageClass == BooleanClass

    lazy val storageClass: ClassSymbol = symbol.info.typeSymbol.asClass
  }


  // TODO: better way to communicate from info transform to tree transform?
  private[this] val _bitmapInfo  = perRunCaches.newMap[Symbol, BitmapInfo]()
  private[this] val _slowPathFor = perRunCaches.newMap[Symbol, Symbol]()

  def checkedAccessorSymbolSynth(clz: Symbol): CheckedAccessorSymbolSynth =
    new CheckedAccessorSymbolSynth(clz)

  // base trait, with enough functionality for generating bitmap symbols for lazy vals and -Xcheckinit fields
  class CheckedAccessorSymbolSynth(val clazz: Symbol) {
    /**
      * Note: fields of classes inheriting DelayedInit are not checked.
      * This is because they are neither initialized in the constructor
      * nor do they have a setter (not if they are vals anyway). The usual
      * logic for setting bitmaps does therefore not work for such fields.
      * That's why they are excluded.
      *
      */
    private[this] val doCheckInit = settings.checkInit.value && !(clazz isSubClass DelayedInitClass)

    private[AccessorSynthesis] def bitmapFor(field: Symbol): BitmapInfo = _bitmapInfo(field)
    protected def bitmapOf(field: Symbol): Option[BitmapInfo] = _bitmapInfo.get(field)


    /** Fill the map from fields to bitmap infos.
      * This is called for all fields in each transformed class (by the fields info transformer),
      * after the fields inherited from traits have been added.
      *
      * bitmaps for checkinit fields are not inherited
      */
    def computeBitmapInfos(fields: List[Symbol]): List[Symbol] = {
      def bitmapCategory(field: Symbol): Name = {
        import nme._

        if (field.isLazy)
          if (field hasAnnotation TransientAttr) BITMAP_TRANSIENT else BITMAP_NORMAL
        else if (doCheckInit && !(field hasFlag DEFAULTINIT | PRESUPER | PARAMACCESSOR))
          if (field hasAnnotation TransientAttr) BITMAP_CHECKINIT_TRANSIENT else BITMAP_CHECKINIT
        else NO_NAME
      }

      def allocateBitmaps(fieldsWithBitmaps: List[Symbol], category: Name) = {
        val nbFields = fieldsWithBitmaps.length // we know it's > 0
        val (bitmapClass, bitmapCapacity) =
          if (nbFields == 1)       (BooleanClass, 1)
          else if (nbFields <= 8)  (ByteClass, 8)
          else if (nbFields <= 32) (IntClass, 32)
          else (LongClass, 64)

        // 0-based index of highest bit, divided by bits per bitmap
        // note that this is only ever > 0 when bitmapClass == LongClass
        val maxBitmapNumber = (nbFields - 1) / bitmapCapacity

        // transient fields get their own category
        val isTransientCategory = nme.isTransientBitmap(category)

        val bitmapSyms =
          (0 to maxBitmapNumber).toArray map { bitmapNumber =>
            val bitmapSym = (
              clazz.newVariable(nme.newBitmapName(category, bitmapNumber).toTermName, clazz.pos.focus)
                setInfo bitmapClass.tpe
                setFlag PrivateLocal | NEEDS_TREES
              )

            bitmapSym addAnnotation VolatileAttr

            if (isTransientCategory) bitmapSym addAnnotation TransientAttr

            bitmapSym
          }

        fieldsWithBitmaps.zipWithIndex foreach { case (f, idx) =>
          val bitmapIdx = idx / bitmapCapacity
          val offsetInBitmap = idx % bitmapCapacity
          val mask =
            if (bitmapClass == LongClass) Constant(1L << offsetInBitmap)
            else Constant(1 << offsetInBitmap)

          _bitmapInfo(f) = BitmapInfo(bitmapSyms(bitmapIdx), Literal(mask))
        }

        bitmapSyms
      }

      fields.groupBy(bitmapCategory).flatMap {
        case (category, fields) if category != nme.NO_NAME && fields.nonEmpty => allocateBitmaps(fields, category): Iterable[Symbol]
        case _ => Nil
      }.toList
    }

    def slowPathFor(lzyVal: Symbol): Symbol = _slowPathFor(lzyVal)

    def newSlowPathSymbol(lzyVal: Symbol): Symbol = {
      val pos = if (lzyVal.pos != NoPosition) lzyVal.pos else clazz.pos.focus // TODO: is the else branch ever taken?
      val sym = clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), pos, PRIVATE) setInfo MethodType(Nil, lzyVal.tpe.resultType)
      _slowPathFor(lzyVal) = sym
      sym
    }

  }



  // synthesize trees based on info gathered during info transform
  // (which are known to have been run because the tree transform runs afterOwnPhase)
  // since we can't easily share all info via symbols and flags, we have two maps above
  // (they are persisted even between phases because the -Xcheckinit logic runs during constructors)
  // TODO: can we use attachments instead of _bitmapInfo and _slowPathFor?
  trait CheckedAccessorTreeSynthesis extends AccessorTreeSynthesis {
    // note: we deal in getters here, not field symbols
    class SynthCheckedAccessorsTreesInClass(clazz: Symbol) extends CheckedAccessorSymbolSynth(clazz) {
      def isUnitGetter(sym: Symbol) = sym.tpe.resultType.typeSymbol == UnitClass
      def thisRef = gen.mkAttributedThis(clazz)


      /** Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
        * precise comparison operator depending on the value of 'equalToZero'.
        */
      def mkTest(bm: BitmapInfo, equalToZero: Boolean = true): Tree =
        if (bm.isBoolean)
          if (equalToZero) Apply(NOT(bm.select(thisRef)), Nil) else bm.select(thisRef)
        else
          Apply(bm.member(bm.applyToMask(thisRef, nme.AND), if (equalToZero) nme.EQ else nme.NE), List(ZERO))

      /** Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      def mkSetFlag(bitmap: BitmapInfo): Tree =
        Assign(bitmap.select(thisRef),
          if (bitmap.isBoolean) TRUE
          else {
            val ored = bitmap.applyToMask(thisRef, nme.OR)
            // NOTE: Unless the bitmap is a Long, we must convert explicitly to avoid widening
            // For example, bitwise OR (`|`) on two bytes yields and Int
            if (bitmap.isLong) ored else bitmap.convert(ored)
          })
    }

    class SynthLazyAccessorsIn(clazz: Symbol) extends SynthCheckedAccessorsTreesInClass(clazz) {
      /**
        * The compute method (slow path) looks like:
        *
        * {{{
        * def l\$compute() = {
        *   synchronized(this) {
        *     if ((bitmap\$n & MASK) == 0) {
        *      init // l\$ = <rhs>
        *      bitmap\$n = bimap\$n | MASK
        *     }
        *   }
        *   ...
        *   this.f1 = null
        *   ...
        *   this.fn = null
        *   l\$
        * }
        * }}}
        *
        * `bitmap\$n` is a byte, int or long value acting as a bitmap of initialized values.
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
        val rhsAtSlowDef = transformedRhs.changeOwner(lazyAccessor, slowPathSym)

        val isUnit    = isUnitGetter(lazyAccessor)
        val selectVar = if (isUnit) UNIT         else Select(thisRef, lazyVar)
        val storeRes  = if (isUnit) rhsAtSlowDef else Assign(selectVar, fields.castHack(rhsAtSlowDef, lazyVar.info))

        val bitmap = bitmapFor(lazyVar)
        def needsInit = mkTest(bitmap)
        val doInit = Block(List(storeRes), mkSetFlag(bitmap))
        // the slow part of double-checked locking (TODO: is this the most efficient pattern? https://github.come/scala/scala-dev/issues/204)
        val slowPathRhs = Block(gen.mkSynchronized(thisRef)(If(needsInit, doInit, EmptyTree)) :: Nil, selectVar)

        // The lazy accessor delegates to the compute method if needed, otherwise just accesses the var (it was initialized previously)
        // `if ((bitmap&n & MASK) == 0) this.l$compute() else l$`
        val accessorRhs = fields.castHack(If(needsInit, Apply(Select(thisRef, slowPathSym), Nil), selectVar), lazyVar.info)

        afterOwnPhase { // so that we can assign to vals
          Thicket(List((DefDef(slowPathSym, slowPathRhs)), DefDef(lazyAccessor, accessorRhs)) map typedPos(lazyAccessor.pos.focus))
        }
      }
    }

    class SynthInitCheckedAccessorsIn(clazz: Symbol) extends SynthCheckedAccessorsTreesInClass(clazz) {

      // Add statements to the body of a constructor to set the 'init' bit for each field initialized in the constructor
      private object addInitBitsTransformer extends AstTransformer {
        override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
          val checkedStats = stats flatMap {
            // Mark field as initialized after an assignment
            case stat@Assign(lhs@Select(This(_), _), _) =>
              stat :: bitmapOf(lhs.symbol).toList.map(bitmap => typedPos(stat.pos.focus)(mkSetFlag(bitmap)))

            // remove initialization for default values
            // TODO is this case ever hit? constructors does not generate Assigns with EmptyTree for the rhs AFAICT
            // !!! Ident(self) is never referenced, is it supposed to be confirming
            // that self is anything in particular?
            case Apply(lhs@Select(Ident(_), _), EmptyTree.asList) if lhs.symbol.isSetter => Nil
            case stat => List(stat)
          }

          super.transformStats(checkedStats, exprOwner)
        }
      }

      private[this] val isTrait = clazz.isTrait
      // We only act on concrete methods, and traits only need to have their constructor rewritten
      def needsWrapping(dd: DefDef) =
        dd.rhs != EmptyTree && (!isTrait || dd.symbol.isConstructor)

      /** Make getters check the initialized bit, and the class constructor & setters are changed to set the initialized bits. */
      def wrapRhsWithInitChecks(sym: Symbol)(rhs: Tree): Tree =
        if (sym.isConstructor) addInitBitsTransformer transform rhs
        else if ((sym hasFlag ACCESSOR) && !(sym hasFlag (LAZY | PARAMACCESSOR))) {
          val field = clazz.info.decl(sym.localName)
          if (field == NoSymbol) rhs
          else bitmapOf(field) match {
            case Some(bitmap) =>
              if (sym.isGetter) mkCheckedAccessorRhs(if (isUnitGetter(sym)) UNIT else rhs, rhs.pos, bitmap) // TODO: why not always use rhs?
              else Block(List(rhs, typedPos(rhs.pos.focus)(mkSetFlag(bitmap))), UNIT)
            case _            => rhs
          }
        }
        else rhs

      private def mkCheckedAccessorRhs(retVal: Tree, pos: Position, bitmap: BitmapInfo): Tree = {
        val msg = s"Uninitialized field: ${clazz.sourceFile.name}: ${pos.line}"
        val result =
          IF(mkTest(bitmap, equalToZero = false)).
            THEN(retVal).
            ELSE(Throw(NewFromConstructor(UninitializedFieldConstructor, LIT(msg))))

        typedPos(pos)(BLOCK(result, retVal))
      }
    }
  }
}
