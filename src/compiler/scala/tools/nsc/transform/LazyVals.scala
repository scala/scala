package scala.tools.nsc
package transform

import scala.collection.{ mutable, immutable }

abstract class LazyVals extends Transform with TypingTransformers with ast.TreeDSL {
  // inherits abstract value `global` and class `Phase` from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import CODE._

  val phaseName: String = "lazyvals"
  private val FLAGS_PER_BYTE: Int = 8 // Byte
  private def bitmapKind = ByteClass

  def newTransformer(unit: CompilationUnit): Transformer =
    new LazyValues(unit)

  private def lazyUnit(sym: Symbol) = sym.tpe.resultType.typeSymbol == UnitClass

  object LocalLazyValFinder extends Traverser {
    var result: Boolean  = _

    def find(t: Tree) = {result = false; traverse(t); result}
    def find(ts: List[Tree]) = {result = false; traverseTrees(ts); result}

    override def traverse(t: Tree) {
      if (!result)
        t match {
          case v@ValDef(_, _, _, _) if v.symbol.isLazy =>
            result = true

          case d@DefDef(_, _, _, _, _, _) if d.symbol.isLazy && lazyUnit(d.symbol) =>
            d.symbol.resetFlag(symtab.Flags.LAZY)
            result = true

          case ClassDef(_, _, _, _) | DefDef(_, _, _, _, _, _) | ModuleDef(_, _, _) =>

          case LabelDef(name, _, _) if nme.isLoopHeaderLabel(name) =>

          case _ =>
            super.traverse(t)
        }
    }
  }

  /**
   * Transform local lazy accessors to check for the initialized bit.
   */
  class LazyValues(unit: CompilationUnit) extends TypingTransformer(unit) {
    /** map from method symbols to the number of lazy values it defines. */
    private val lazyVals = perRunCaches.newMap[Symbol, Int]() withDefaultValue 0

    import symtab.Flags._

    /** Perform the following transformations:
     *  - for a lazy accessor inside a method, make it check the initialization bitmap
     *  - for all methods, add enough int vars to allow one flag per lazy local value
     *  - blocks in template bodies behave almost like methods. A single bitmaps section is
     *      added in the first block, for all lazy values defined in such blocks.
     *  - remove ACCESSOR flags: accessors in traits are not statically implemented,
     *    but moved to the host class. local lazy values should be statically implemented.
     */
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      curTree = tree

      tree match {

        case Block(_, _) =>
          val block1 = super.transform(tree)
          val Block(stats, expr) = block1
          val stats1 = stats.flatMap(_ match {
            case Block(List(d1@DefDef(_, n1, _, _, _, _)), d2@DefDef(_, n2, _, _, _, _)) if (nme.newLazyValSlowComputeName(n2) == n1) =>
              List(d1, d2)
            case stat =>
              List(stat)
          })
          treeCopy.Block(block1, stats1, expr)

        case DefDef(_, _, _, _, _, rhs) => atOwner(tree.symbol) {
          val (res, slowPathDef) = if (!sym.owner.isClass && sym.isLazy) {
            val enclosingClassOrDummyOrMethod = {
              val enclMethod = sym.enclMethod

              if (enclMethod != NoSymbol ) {
                val enclClass = sym.enclClass
                if (enclClass != NoSymbol && enclMethod == enclClass.enclMethod)
                  enclClass
                else
                  enclMethod
              } else
                sym.owner
            }
            debuglog(s"determined enclosing class/dummy/method for lazy val as $enclosingClassOrDummyOrMethod given symbol $sym")
            val idx = lazyVals(enclosingClassOrDummyOrMethod)
            lazyVals(enclosingClassOrDummyOrMethod) = idx + 1
            val (rhs1, sDef) = mkLazyDef(enclosingClassOrDummyOrMethod, transform(rhs), idx, sym)
            sym.resetFlag((if (lazyUnit(sym)) 0 else LAZY) | ACCESSOR)
            (rhs1, sDef)
          } else
            (transform(rhs), EmptyTree)

          val ddef1 = deriveDefDef(tree)(_ => if (LocalLazyValFinder.find(res)) typed(addBitmapDefs(sym, res)) else res)
          if (slowPathDef != EmptyTree) Block(slowPathDef, ddef1) else ddef1
        }

        case Template(_, _, body) => atOwner(currentOwner) {
          val body1 = super.transformTrees(body)
          var added = false
          val stats =
            for (stat <- body1) yield stat match {
              case Block(_, _) | Apply(_, _) | If(_, _, _) | Try(_, _, _) if !added =>
                // Avoid adding bitmaps when they are fully overshadowed by those
                // that are added inside loops
                if (LocalLazyValFinder.find(stat)) {
                  added = true
                  typed(addBitmapDefs(sym, stat))
                } else stat
              case ValDef(_, _, _, _) =>
                typed(deriveValDef(stat)(addBitmapDefs(stat.symbol, _)))
              case _ =>
                stat
            }
          val innerClassBitmaps = if (!added && currentOwner.isClass && bitmaps.contains(currentOwner)) {
              // add bitmap to inner class if necessary
                val toAdd0 = bitmaps(currentOwner).map(s => typed(ValDef(s, ZERO)))
                toAdd0.foreach(t => {
                    if (currentOwner.info.decl(t.symbol.name) == NoSymbol) {
                      t.symbol.setFlag(PROTECTED)
                      currentOwner.info.decls.enter(t.symbol)
                    }
                })
                toAdd0
            } else List()
          deriveTemplate(tree)(_ => innerClassBitmaps ++ stats)
        }

        case ValDef(_, _, _, _) if !sym.owner.isModule && !sym.owner.isClass =>
          deriveValDef(tree) { rhs0 =>
            val rhs = transform(rhs0)
            if (LocalLazyValFinder.find(rhs)) typed(addBitmapDefs(sym, rhs)) else rhs
          }

        case l@LabelDef(name0, params0, ifp0@If(_, _, _)) if name0.startsWith(nme.WHILE_PREFIX) =>
          val ifp1 = super.transform(ifp0)
          val If(cond0, thenp0, elsep0) = ifp1

          if (LocalLazyValFinder.find(thenp0))
            deriveLabelDef(l)(_ => treeCopy.If(ifp1, cond0, typed(addBitmapDefs(sym.owner, thenp0)), elsep0))
          else
            l

        case l@LabelDef(name0, params0, block@Block(stats0, expr))
          if name0.startsWith(nme.WHILE_PREFIX) || name0.startsWith(nme.DO_WHILE_PREFIX) =>
          val stats1 = super.transformTrees(stats0)
          if (LocalLazyValFinder.find(stats1))
            deriveLabelDef(l)(_ => treeCopy.Block(block, typed(addBitmapDefs(sym.owner, stats1.head))::stats1.tail, expr))
          else
            l

        case _ => super.transform(tree)
      }
    }

    /** Add the bitmap definitions to the rhs of a method definition.
     *  If the rhs has been tail-call transformed, insert the bitmap
     *  definitions inside the top-level label definition, so that each
     *  iteration has the lazy values uninitialized. Otherwise add them
     *  at the very beginning of the method.
     */
    private def addBitmapDefs(methSym: Symbol, rhs: Tree): Tree = {
      def prependStats(stats: List[Tree], tree: Tree): Block = tree match {
        case Block(stats1, res) => Block(stats ::: stats1, res)
        case _ => Block(stats, tree)
      }

      val bmps = bitmaps(methSym) map (ValDef(_, ZERO))

      def isMatch(params: List[Ident]) = (params.tail corresponds methSym.tpe.params)(_.tpe == _.tpe)

      if (bmps.isEmpty) rhs else rhs match {
        case Block(assign, l @ LabelDef(name, params, _))
          if (name string_== "_" + methSym.name) && isMatch(params) =>
            Block(assign, deriveLabelDef(l)(rhs => typed(prependStats(bmps, rhs))))

        case _ => prependStats(bmps, rhs)
      }
    }

    def mkSlowPathDef(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                      stats: List[Tree], retVal: Tree): Tree = {
      // Q: is there a reason to first set owner to `clazz` (by using clazz.newMethod), and then
      // changing it to lzyVal.owner very soon after? Could we just do lzyVal.owner.newMethod?
      val defSym = clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), lzyVal.pos, STABLE | PRIVATE)
      defSym setInfo MethodType(List(), lzyVal.tpe.resultType)
      defSym.owner = lzyVal.owner
      debuglog(s"crete slow compute path $defSym with owner ${defSym.owner} for lazy val $lzyVal")
      if (bitmaps.contains(lzyVal))
        bitmaps(lzyVal).map(_.owner = defSym)
      val rhs: Tree = gen.mkSynchronizedCheck(clazz, cond, syncBody, stats).changeOwner(currentOwner -> defSym)

      DefDef(defSym, addBitmapDefs(lzyVal, BLOCK(rhs, retVal)))
    }


    def mkFastPathBody(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                       stats: List[Tree], retVal: Tree): (Tree, Tree) = {
      val slowPathDef: Tree = mkSlowPathDef(clazz, lzyVal, cond, syncBody, stats, retVal)
      (If(cond, Apply(Ident(slowPathDef.symbol), Nil), retVal), slowPathDef)
    }

    /** return a 'lazified' version of rhs. Rhs should conform to the
     *  following schema:
     *  {
     *    l$ = <rhs>
     *    l$
     *  } or
     *  <rhs> when the lazy value has type Unit (for which there is no field
     *  to cache its value.
     *
     *  Similarly as for normal lazy val members (see Mixin), the result will be a tree of the form
     *  { if ((bitmap&n & MASK) == 0) this.l$compute()
     *    else l$
     *
     *    def l$compute() = { synchronized(enclosing_class_or_dummy) {
     *      if ((bitmap$n & MASK) == 0) {
     *       l$ = <rhs>
     *       bitmap$n = bimap$n | MASK
     *      }}
     *      l$
     *    }
     *  }
     *  where bitmap$n is a byte value acting as a bitmap of initialized values. It is
     *  the 'n' is (offset / 8), the MASK is (1 << (offset % 8)). If the value has type
     *  unit, no field is used to cache the value, so the l$compute will now look as following:
     *  {
     *    def l$compute() = { synchronized(enclosing_class_or_dummy) {
     *      if ((bitmap$n & MASK) == 0) {
     *       <rhs>;
     *       bitmap$n = bimap$n | MASK
     *      }}
     *    ()
     *    }
     *  }
     */
    private def mkLazyDef(methOrClass: Symbol, tree: Tree, offset: Int, lazyVal: Symbol): (Tree, Tree) = {
      val bitmapSym           = getBitmapFor(methOrClass, offset)
      val mask                = LIT(1 << (offset % FLAGS_PER_BYTE))
      val bitmapRef = if (methOrClass.isClass) Select(This(methOrClass), bitmapSym) else Ident(bitmapSym)

      def mkBlock(stmt: Tree) = BLOCK(stmt, mkSetFlag(bitmapSym, mask, bitmapRef), UNIT)

      debuglog(s"create complete lazy def in $methOrClass for $lazyVal")
      val (block, res) = tree match {
        case Block(List(assignment), res) if !lazyUnit(lazyVal) =>
          (mkBlock(assignment),  res)
        case rhs                          =>
          (mkBlock(rhs),         UNIT)
      }

      val cond = (bitmapRef GEN_& (mask, bitmapKind)) GEN_== (ZERO, bitmapKind)
      val lazyDefs = mkFastPathBody(methOrClass.enclClass, lazyVal, cond, List(block), Nil, res)
      (atPos(tree.pos)(localTyper.typed {lazyDefs._1 }), atPos(tree.pos)(localTyper.typed {lazyDefs._2 }))
    }

    private def mkSetFlag(bmp: Symbol, mask: Tree, bmpRef: Tree): Tree =
      bmpRef === (bmpRef GEN_| (mask, bitmapKind))

    val bitmaps = mutable.Map[Symbol, List[Symbol]]() withDefaultValue Nil

    /** Return the symbol corresponding of the right bitmap int inside meth,
     *  given offset.
     */
    private def getBitmapFor(meth: Symbol, offset: Int): Symbol = {
      val n = offset / FLAGS_PER_BYTE
      val bmps = bitmaps(meth)
      if (bmps.length > n)
        bmps(n)
      else {
        val sym = meth.newVariable(nme.newBitmapName(nme.BITMAP_NORMAL, n), meth.pos).setInfo(ByteTpe)
        enteringTyper {
          sym addAnnotation VolatileAttr
        }

        bitmaps(meth) = (sym :: bmps).reverse
        sym
      }
    }
  }
}
