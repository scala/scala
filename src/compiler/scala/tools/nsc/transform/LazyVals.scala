package scala.tools.nsc.transform;

import scala.tools.nsc._
import scala.collection.mutable.HashMap

abstract class LazyVals extends Transform {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions

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
     */
    override def transform(tree: Tree): Tree = {
       val sym = tree.symbol
      tree match {
        case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          val res = if (!sym.owner.isClass && sym.hasFlag(LAZY)) {
            val idx = lazyVals(sym.enclMethod)
            val rhs1 = mkLazyDef(sym.enclMethod, super.transform(rhs), idx)
            lazyVals(sym.owner) = idx + 1
            sym.resetFlag(LAZY)
            rhs1
          } else
            super.transform(rhs)
          val bmps = bitmaps(sym) map { b => ValDef(b, Literal(Constant(0))) }
          copy.DefDef(tree, mods, name, tparams, vparams, tpt, typed(if (bmps.isEmpty) res else Block(bmps, res)))

        case _ => super.transform(tree)
      }
    }

    /** return a 'lazified' version of rhs. Rhs should conform to the
     *  following schema:
     *  {
     *    l$ = <rhs>
     *    l$
     *  }
     *  The result will be a tree of the form
     *  {
     *    if ((bitmap$n & MASK) == 0) {
     *       l$ = <rhs>
     *       bitmap$n = bimap$n | MASK
     *    }
     *    l$
     *  }
     *  where bitmap$n is an int value acting as a bitmap of initialized values. It is
     *  the 'n' is (offset / 32), the MASK is (1 << (offset % 32)).
     */
    private def mkLazyDef(meth: Symbol, tree: Tree, offset: Int): Tree = {
      val bitmapSym = getBitmapFor(meth, offset)
      val Block(List(assignment), res) = tree
      val mask = Literal(Constant(1 << (offset % FLAGS_PER_WORD)))
      val result =
        If(Apply(
            Select(
              Apply(Select(Ident(bitmapSym), Int_And),
                    List(mask)),
              Int_==),
            List(Literal(Constant(0)))), Block(List(assignment, mkSetFlag(bitmapSym, mask)), Literal(Constant(()))), EmptyTree)
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
