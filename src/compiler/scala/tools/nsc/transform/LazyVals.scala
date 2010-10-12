package scala.tools.nsc
package transform

import scala.collection.{ mutable, immutable }

abstract class LazyVals extends Transform with TypingTransformers with ast.TreeDSL {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import CODE._

  val phaseName: String = "lazyvals"
  val FLAGS_PER_WORD: Int

  def newTransformer(unit: CompilationUnit): Transformer =
    new LazyValues(unit)

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
          val res = if (!sym.owner.isClass && sym.hasFlag(LAZY)) {
            val enclosingDummyOrMethod =
              if (sym.enclMethod == NoSymbol) sym.owner else sym.enclMethod
            val idx = lazyVals(enclosingDummyOrMethod)
            val rhs1 = mkLazyDef(enclosingDummyOrMethod, super.transform(rhs), idx)
            lazyVals(sym.owner) = idx + 1
            sym.resetFlag(LAZY | ACCESSOR)
            rhs1
          } else
            super.transform(rhs)
          treeCopy.DefDef(tree, mods, name, tparams, vparams, tpt,
                          typed(addBitmapDefs(sym, res)))
        }

        case Template(parents, self, body) => atOwner(currentOwner) {
          val body1 = super.transformTrees(body)
          var added = false
          val stats =
            for (stat <- body1) yield stat match {
              case Block(_, _) if !added =>
                added = true
                typed(addBitmapDefs(sym, stat))
              case ValDef(mods, name, tpt, rhs) =>
                typed(treeCopy.ValDef(stat, mods, name, tpt, addBitmapDefs(stat.symbol, rhs)))
              case _ =>
                stat
            }
          treeCopy.Template(tree, parents, self, stats)
        }

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
    private def mkLazyDef(meth: Symbol, tree: Tree, offset: Int): Tree = {
      val bitmapSym           = getBitmapFor(meth, offset)
      val mask                = LIT(1 << (offset % FLAGS_PER_WORD))
      def mkBlock(stmt: Tree) = BLOCK(stmt, mkSetFlag(bitmapSym, mask), UNIT)

      val (block, res) = tree match {
        case Block(List(assignment), res) => (mkBlock(assignment),  res)
        case rhs                          => (mkBlock(rhs),         UNIT)
      }
      assert(res != UNIT || meth.tpe.finalResultType.typeSymbol == UnitClass)

      val cond = (Ident(bitmapSym) INT_& mask) INT_== ZERO

      atPos(tree.pos)(localTyper.typed {
        def body = gen.mkDoubleCheckedLocking(meth.enclClass, cond, List(block), Nil)
        BLOCK(body, res)
      })
    }

    private def mkSetFlag(bmp: Symbol, mask: Tree): Tree =
      Ident(bmp) === (Ident(bmp) INT_| mask)

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
        val sym = meth.newVariable(meth.pos, nme.bitmapName(n)).setInfo(IntClass.tpe)
        atPhase(currentRun.typerPhase) {
          sym addAnnotation AnnotationInfo(VolatileAttr.tpe, Nil, Nil)
        }

        bitmaps(meth) = (sym :: bmps).reverse
        sym
      }
    }
  }
}
