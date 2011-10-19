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
  val FLAGS_PER_WORD: Int

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
    private val lazyVals = new mutable.HashMap[Symbol, Int] {
      override def default(meth: Symbol) = 0
    }

    import symtab.Flags._
    import lazyVals._

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
        case DefDef(mods, name, tparams, vparams, tpt, rhs) => atOwner(tree.symbol) {
          val res = if (!sym.owner.isClass && sym.isLazy) {
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
            val idx = lazyVals(enclosingClassOrDummyOrMethod)
            lazyVals(enclosingClassOrDummyOrMethod) = idx + 1
            val rhs1 = mkLazyDef(enclosingClassOrDummyOrMethod, super.transform(rhs), idx, sym)
            sym.resetFlag((if (lazyUnit(sym)) 0 else LAZY) | ACCESSOR)
            rhs1
          } else
            super.transform(rhs)

          treeCopy.DefDef(tree, mods, name, tparams, vparams, tpt,
                  if (LocalLazyValFinder.find(res)) typed(addBitmapDefs(sym, res)) else res)
        }

        case Template(parents, self, body) => atOwner(currentOwner) {
          val body1 = super.transformTrees(body)
          var added = false
          val stats =
            for (stat <- body1) yield stat match {
              case Block(_, _) | Apply(_, _) | If(_, _, _) if !added =>
                // Avoid adding bitmaps when they are fully overshadowed by those
                // that are added inside loops
                if (LocalLazyValFinder.find(stat)) {
                  added = true
                  typed(addBitmapDefs(sym, stat))
                } else stat
              case ValDef(mods, name, tpt, rhs) =>
                typed(treeCopy.ValDef(stat, mods, name, tpt, addBitmapDefs(stat.symbol, rhs)))
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
          treeCopy.Template(tree, parents, self, innerClassBitmaps ++ stats)
        }

        case ValDef(mods, name, tpt, rhs0) if (!sym.owner.isModule && !sym.owner.isClass) =>
          val rhs = super.transform(rhs0)
          treeCopy.ValDef(tree, mods, name, tpt,
                  if (LocalLazyValFinder.find(rhs)) typed(addBitmapDefs(sym, rhs)) else rhs)

        case l@LabelDef(name0, params0, ifp0@If(_, _, _)) if name0.startsWith(nme.WHILE_PREFIX) =>
          val ifp1 = super.transform(ifp0)
          val If(cond0, thenp0, elsep0) = ifp1
          if (LocalLazyValFinder.find(thenp0))
            treeCopy.LabelDef(l, name0, params0,
                    treeCopy.If(ifp1, cond0, typed(addBitmapDefs(sym.owner, thenp0)), elsep0))
          else
            l

        case l@LabelDef(name0, params0, block@Block(stats0, _))
          if name0.startsWith(nme.WHILE_PREFIX) || name0.startsWith(nme.DO_WHILE_PREFIX) =>
          val stats1 = super.transformTrees(stats0)
          if (LocalLazyValFinder.find(stats1))
            treeCopy.LabelDef(l, name0, params0,
                    treeCopy.Block(block, typed(addBitmapDefs(sym.owner, stats1.head))::stats1.tail, block.expr))
          else
            l

        case _ => super.transform(tree)
      }
    }

    /** Add the bitmap definitions to the rhs of a method definition.
     *  If the rhs has been tail-call transformed, insert the bitmap
     *  definitions inside the top-level label definition, so that each
     *  iteration has the lazy values un-initialized. Otherwise add them
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
        case Block(assign, l @ LabelDef(name, params, rhs1))
          if name.toString == ("_" + methSym.name) && isMatch(params) =>
            Block(assign, treeCopy.LabelDef(l, name, params, typed(prependStats(bmps, rhs1))))

        case _ => prependStats(bmps, rhs)
      }
    }

    /** return a 'lazified' version of rhs. Rhs should conform to the
     *  following schema:
     *  {
     *    l$ = <rhs>
     *    l$
     *  } or
     *  <rhs> when the lazy value has type Unit (for which there is no field
     *  to cache it's value.
     *
     *  The result will be a tree of the form
     *  {
     *    if ((bitmap$n & MASK) == 0) {
     *       l$ = <rhs>
     *       bitmap$n = bimap$n | MASK
     *    }
     *    l$
     *  }
     *  where bitmap$n is an int value acting as a bitmap of initialized values. It is
     *  the 'n' is (offset / 32), the MASK is (1 << (offset % 32)). If the value has type
     *  unit, no field is used to cache the value, so the resulting code is:
     *  {
     *    if ((bitmap$n & MASK) == 0) {
     *       <rhs>;
     *       bitmap$n = bimap$n | MASK
     *    }
     *    ()
     *  }
     */
    private def mkLazyDef(methOrClass: Symbol, tree: Tree, offset: Int, lazyVal: Symbol): Tree = {
      val bitmapSym           = getBitmapFor(methOrClass, offset)
      val mask                = LIT(1 << (offset % FLAGS_PER_WORD))
      val bitmapRef = if (methOrClass.isClass) Select(This(methOrClass), bitmapSym) else Ident(bitmapSym)

      def mkBlock(stmt: Tree) = BLOCK(stmt, mkSetFlag(bitmapSym, mask, bitmapRef), UNIT)


      val (block, res) = tree match {
        case Block(List(assignment), res) if !lazyUnit(lazyVal) =>
          (mkBlock(assignment),  res)
        case rhs                          =>
          (mkBlock(rhs),         UNIT)
      }

      val cond = (bitmapRef INT_& mask) INT_== ZERO

      atPos(tree.pos)(localTyper.typed {
        def body = gen.mkDoubleCheckedLocking(methOrClass.enclClass, cond, List(block), Nil)
        BLOCK(body, res)
      })
    }

    private def mkSetFlag(bmp: Symbol, mask: Tree, bmpRef: Tree): Tree =
      bmpRef === (bmpRef INT_| mask)

    val bitmaps = new mutable.HashMap[Symbol, List[Symbol]] {
      override def default(meth: Symbol) = Nil
    }

    /** Return the symbol corresponding of the right bitmap int inside meth,
     *  given offset.
     */
    private def getBitmapFor(meth: Symbol, offset: Int): Symbol = {
      val n = offset / FLAGS_PER_WORD
      val bmps = bitmaps(meth)
      if (bmps.length > n)
        bmps(n)
      else {
        val sym = meth.newVariable(meth.pos, nme.newBitmapName(nme.BITMAP_NORMAL, n)).setInfo(IntClass.tpe)
        atPhase(currentRun.typerPhase) {
          sym addAnnotation VolatileAttr
        }

        bitmaps(meth) = (sym :: bmps).reverse
        sym
      }
    }
  }
}
