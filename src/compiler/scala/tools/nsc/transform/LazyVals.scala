package scala.tools.nsc.transform;

import scala.tools.nsc._
import scala.collection.mutable.HashMap

abstract class LazyVals extends Transform {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees

  val phaseName: String = "lazyvals"

  def newTransformer(unit: CompilationUnit): Transformer =
    new LazyValues(unit)

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit =
        newTransformer(unit).transformUnit(unit);
  }

  /**
   * Transform local lazy accessors to check for the initialized bit.
   */
  class LazyValues(unit: CompilationUnit) extends Transformer {

    import definitions.{Int_And, Int_Or, Int_==}
    /** map from method symbols to the number of lazy values it defines. */
    private val lazyVals = new HashMap[Symbol, Int] {
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
      tree match {
        case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
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
          copy.DefDef(tree, mods, name, tparams, vparams, tpt,
                      typed(addBitmapDefs(sym, res)))

        case Template(parents, self, body) =>
          val body1 = super.transformTrees(body)
          var added = false
          val stats =
            for (stat <- body1) yield stat match {
              case Block(_, _) if !added =>
                added = true
                typed(addBitmapDefs(sym, stat))
              case ValDef(mods, name, tpt, rhs) =>
                typed(copy.ValDef(stat, mods, name, tpt, addBitmapDefs(stat.symbol, rhs)))
              case _ =>
                stat
            }
          copy.Template(tree, parents, self, stats)

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

      val bmps = bitmaps(methSym) map { b => ValDef(b, Literal(Constant(0))) }
      if (bmps.isEmpty) rhs else rhs match {
        case Block(assign, l @ LabelDef(name, params, rhs1))
          if (name.toString.equals("_" + methSym.name)
              && List.forall2(params.tail, methSym.tpe.paramTypes) { (ident, tpe) => ident.tpe == tpe }) =>
            val sym = l.symbol
            Block(assign, copy.LabelDef(l, name, params, typed(prependStats(bmps, rhs1))))

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
     *  unit, no field is used to chache the value, so the resulting code is:
     *  {
     *    if ((bitmap$n & MASK) == 0) {
     *       <rhs>;
     *       bitmap$n = bimap$n | MASK
     *    }
     *    ()
     *  }
     */
    private def mkLazyDef(meth: Symbol, tree: Tree, offset: Int): Tree = {
      val bitmapSym = getBitmapFor(meth, offset)
      val mask = Literal(Constant(1 << (offset % FLAGS_PER_WORD)))

      val (block, res) = tree match {
        case Block(List(assignment), res) =>
          (Block(List(assignment, mkSetFlag(bitmapSym, mask)), Literal(Constant(()))), res)
        case rhs =>
          assert(meth.tpe.finalResultType.typeSymbol == definitions.UnitClass)
          (Block(List(rhs, mkSetFlag(bitmapSym, mask)), Literal(Constant(()))), Literal(()))
      }

      val result = atPos(tree.pos) {
        If(Apply(
            Select(
              Apply(Select(Ident(bitmapSym), Int_And),
                    List(mask)),
              Int_==),
            List(Literal(Constant(0)))), block, EmptyTree)
      }
      typed(Block(List(result), res))
    }

    private def mkSetFlag(bmp: Symbol, mask: Tree): Tree =
      Assign(Ident(bmp),
        Apply(Select(Ident(bmp), Int_Or), List(mask)))


    final val FLAGS_PER_WORD = 32
    val bitmaps = new HashMap[Symbol, List[Symbol]] {
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
        bitmaps(meth) = (sym :: bmps).reverse
        sym
      }
    }
  }
}
