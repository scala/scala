/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

/** Translation of pattern matching
 *
 *  @author Burak Emir
 */
abstract class TransMatcher extends transform.Transform
with PatternNodes
with CodeFactory
with PatternMatchers
//with NewMatchers
with SequenceMatchers
with AlgebraicMatchers
with MatcherLabels
with BerrySethis
with DetWordAutoms
with NondetWordAutoms
with Autom2
with WordAutoms
with LeftTracers
with RightTracers {

  import global._
  import definitions._
  import posAssigner.atPos
  import typer.typed
  import symtab.Flags

  val phaseName = "transmatcher"

  protected def newTransformer(unit: global.CompilationUnit): global.Transformer = {
    new TransMatch
  }

  /** container. classes AlgebraicMatcher and SequenceMatcher get input and
   *  store their results in here. resembles the 'Memento' design pattern,
   *  could also be named 'Liaison'
   */
  abstract class PartialMatcher {

    /** owner of the code we create (input)
     */
    val owner: Symbol

    /** the selector value (input)
     */
    val selector: Tree

    /** tree representing the matcher (output)
     */
    var tree: Tree  = _

    def pos: int = selector.pos

    //assert( owner != null ) : "owner is null";
    //assert owner != Symbol.NONE ;
    //this.owner      = owner;

    //assert root != null;
    //assert root.type != null;
    //this.selector   = root;

    //assert this.resultType != Type.NoType;
    //this.resultType = resultType;

    //this.pos        = root.pos; // for convenience only
  }

  var cunit: CompilationUnit = _

  def fresh = cunit.fresh

  var currentOwner: Symbol = _

  var resultType: Type = _

  def containsBinding(pat: Tree): Boolean = {
    var generatedVars = false

    def handleVariableSymbol(sym: Symbol): Unit  =
      if (sym.name.toString().indexOf("$") == -1) {
        generatedVars = true // .add(sym)
      }

    def isVariableName(name: Name): Boolean =
      (treeInfo.isVariableName(name)) && (name != nme.USCOREkw)

    def isVariableSymbol(sym: Symbol): Boolean =
      (sym != null) && (!sym.isPrimaryConstructor)

    def traverse(tree: Tree): Unit = {
      tree match {
        case x @ Ident(name) =>
          if (x.symbol != definitions.PatternWildcard)
            error("shouldn't happen?!")

        case Bind(name, subtree) =>
          var sym: Symbol = null
          if (isVariableName(name)
              && isVariableSymbol( {sym = tree.symbol; tree.symbol} ))
            handleVariableSymbol(sym)
          traverse(subtree)

        // congruence
        case Apply(fun, args) => args foreach traverse
        case Sequence(trees)  => trees foreach traverse
        case Star(arg)        => traverse(arg)
        case Typed(expr, tpe) => traverse(expr) // needed??

        case _ : Select |
             _ : Alternative |
             _ : Select |
             _ : Literal =>  ; // no variables

        case _ =>
          cunit.error(tree.pos, "unknown pattern node:" + tree + " = " + tree.getClass())
      }
    }
    traverse(pat)
    generatedVars
  }

  /** returns true if apply is a "sequence apply". analyzer inserts Sequence nodes if something is a
   *
   *  - last update: discussion with Martin 2005-02-18
   *
   *  - if true, tree.fn must be ignored. The analyzer ensures that the selector will be a subtype
   *    of fn; it thus assigns the expected type from the context (which is surely a subtype,
   *    but may have different flags etc.
   *
   *  - so should be
   *     (( tree.args.length == 1 ) && tree.args(0).isInstanceOf[Sequence])
   *     but fails
   */
  protected def isSeqApply( tree: Apply  ): Boolean =  {
   // Console.print("isSeqApply? "+tree.toString());
   // val res =
	tree match {
	  case Apply(_, List(ArrayValue(_,_))) => (tree.tpe.symbol.flags & Flags.CASE) == 0
	  case _ => false;
	}
	//Console.println(res);
	//res;
  }

    /** a casedef with sequence subpatterns like
     *
     *  case ..x @ ().. => body
     *
     * should be replaced straight away with
     *
     *  case    .. () .. => val x = Nil; body
     */

  def isRegularPattern(pat: Tree): Boolean = pat match {
    case Alternative(trees)    =>
      trees exists { isRegularPattern }
    case Star(t)               => true
    case Ident(_)              => false
    case Bind(n, pat1)         => isRegularPattern(pat1)
    case Sequence(trees)       => true // cause there are ArrayValues now
    case ArrayValue(tt, trees) =>
      trees exists { isRegularPattern }
    case Apply(fn, List(Sequence(List()))) =>
      false
    case Apply(fn, trees)      =>
      trees exists { isRegularPattern }
    case Literal(_)            => false
    case Select(_, _)          => false
    case Typed(_, _)           => false
  }

  protected def isDefault(p: Tree): Boolean = p match {
    case Bind(_, q)           => isDefault(q)
    case Ident(nme.WILDCARD)  => true
    case _                    => false
  }

  protected def isDefaultStar(p: Tree): Boolean = p match {
    case Bind(_, q)                 => isDefaultStar(q)
    case Star(Ident(nme.WILDCARD))  => true
    case _                          => false
  }

  // @todo: this should be isNotRegular :-/ premature opt src of all evil
  // check special case Seq(p1,...,pk,_*) where pi not regular
  protected def isRightIgnoring(p: ArrayValue): Boolean = p match {
    case ArrayValue(s, trees) =>
      val it = trees.elements
      var c: Tree = null
      while (it.hasNext && {c = it.next; !isRegularPattern(c)}) {}
      (!it.hasNext) && isDefaultStar(c)
  }

  class TransMatch extends Transformer {

    override def transformUnit(unit: CompilationUnit) = {
      cunit = unit
      super.transformUnit(unit)
      cunit = null
    }

    /** a casedef with sequence subpatterns like
     *
     *  case ..x @ ().. => body
     *
     * should be replaced straight away with
     *
     *  case    .. () .. => val x = Nil; body
     */
    def isRegular(pats: List[CaseDef]): Pair[List[CaseDef],Boolean] = {
      var existsReg = false
      var isReg = false
      var nilVars: List[Symbol] = null

      def isRegular1(pat: Tree): Tree = pat match {
        case Alternative(trees) =>
          copy.Alternative(pat, trees map { isRegular1 })

        case Star(t) =>
          isReg = true; copy.Star(pat, isRegular1(t))

        case Ident(_) =>
          pat

        case Bind(id, empt @ Sequence(List())) =>
          nilVars = pat.symbol /*id.symbol()*/ :: nilVars
          empt

        case Bind(n, pat1) =>
          copy.Bind(pat, n, isRegular1(pat1))

        case Sequence(trees) =>
          //isReg = isReg || ( trees.length == 0 );
          isReg = true // cause there are ArrayValues now
          copy.Sequence(pat, trees map { isRegular1 })

        //case ArrayValue( tt, List(b @ Bind(id, Star(wc @ Ident(nme.WILDCARD))))) =>

        // a pattern of the form List(foo@_*)
        case app @ Apply(fn, List(pat2@ ArrayValue( tt, List(b @ Bind(id, Star(wc @ Ident(nme.WILDCARD))))))) if isSeqApply(app) =>
          //Console.println("OPTIMIZING")
          //Console.println(pat)
          //Console.println(pat.tpe)
          //Console.println(tt.tpe)
          //Console.println("b.tpe "+b.tpe+" widened"+b.tpe.widen)
          //Console.println("b.symbol.tpe "+b.symbol.tpe+" widened"+b.symbol.tpe.widen)
          //Console.println("pat2.tpe "+pat2.tpe+" widened"+pat2.tpe.widen)
          val tpe1:Type = pat2.tpe.widen.baseType( definitions.SeqClass ).typeArgs(0)

          val tpe = appliedType(definitions.SeqClass.typeConstructor, List(tpe1))
          b.symbol.setInfo(tpe)
          b.setType(tpe)
          val res = copy.Bind(b, id, wc)
          //Console.println("====>")
          //Console.println(res)
          res

        // a pattern of the form MyCaseConstructor(foo@_*)
        case app @ Apply(fn, List(pat2@ ArrayValue( tt, List(b @ Bind(id, Star(wc @ Ident(nme.WILDCARD)))))))  =>
          //Console.println("OPTIMIZING")
          //Console.println(pat)
          //Console.println(pat.tpe)
          //Console.println(tt.tpe)
          //Console.println("b.tpe "+b.tpe+" widened"+b.tpe.widen)
          //Console.println("b.symbol.tpe "+b.symbol.tpe+" widened"+b.symbol.tpe.widen)
          //Console.println("pat2.tpe "+pat2.tpe+" widened"+pat2.tpe.widen)
          val tpe1:Type = pat2.tpe.widen.baseType( definitions.SeqClass ).typeArgs(0)

          val tpe = appliedType(definitions.SeqClass.typeConstructor, List(tpe1))
          b.symbol.setInfo(tpe)
          b.setType(tpe)
          val res =  copy.Apply(pat, fn, List(copy.Bind(b, id, wc)))
          //Console.println("====>")
          //Console.println(res)
          res

        case av @ ArrayValue(s, trees) =>
          if (isRightIgnoring(av)) pat
          else copy.ArrayValue(pat, s, (trees map { isRegular1 }))

        case Apply(fn, List(Sequence(List()))) =>
          pat

        case Apply(fn, trees) =>
          //Console.println(" IN isRegular, apply node "+pat.toString());
          //Console.println(" trees are:"+(trees map {x => x.getClass().toString()}));
          copy.Apply(pat, fn, (trees map { isRegular1 }))

        case Literal(_) =>
          pat

        case Select(_, _) =>
          pat

        case Typed(_, _) =>
          pat

        //case _ =>
        //  Console.println(pat);
        //  Console.println(pat.getClass());
        //  scala.Predef.error"( what is this ? ")
      }

      var res: List[CaseDef] = Nil
      val it = pats.elements; while (it.hasNext) {
        nilVars = Nil
        val cdef = it.next
        val newt = isRegular1(cdef.pat)
        existsReg = existsReg || isReg

        val nbody = if (nilVars.isEmpty) cdef.body else
          atPos(cdef.body.pos)(
            Block(nilVars map {
              x => ValDef(x, Ident(definitions.NilModule))
            }, cdef.body)
          )

        res = copy.CaseDef(cdef, newt, cdef.guard, nbody) :: res
      }
      Pair(res.reverse, existsReg)
    }

    /** handles all translation of pattern matching
     */
    def handle(sel: Tree, ocases: List[CaseDef]): Tree = {
      // TEMPORARY
      //new NewMatcher().toIR(sel, ocases)
      //
      // 1. is there a regular pattern?

      val Pair(cases, containsReg) = isRegular(ocases)

      // @todo: remove unused variables

      if (containsReg) {
        // 2. replace nilVariables
        //@todo: bring over AlgebraicMatcher
                /*
        val matcher = new PartialMatcher {
          val global: TransMatcher.this.global.type = TransMatcher.this.global;
          val owner = currentOwner;
          val selector = sel ;
        }
        new AlgebraicMatcher() {
          val tm: TransMatcher.this.type = TransMatcher.this;
        }.construct( matcher, cases );
        matcher.tree
                */

        System.out.println("" + sel + " match " + ocases)
        cunit.error(sel.pos, "regular expressions not yet implemented")
        //sel
        EmptyTree
      } else {
        val pm = new PatternMatcher()
        pm.initialize(sel, currentOwner)
        pm.construct(cases)
        //if (global.log()) {
        //  global.log("internal pattern matching structure");
        //  pm.print();
        //}
        pm.toTree()
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      /* // test code to generate forward jumps
      case Match(selector, CaseDef(Ident(nme.WILDCARD), EmptyTree, exp)::Nil) => // inserting test-code

        val target1 = currentOwner.newLabel(tree.pos, "target1")
        .setInfo(new MethodType(List(definitions.IntClass.tpe), definitions.IntClass.tpe));

        val a = currentOwner.newValue(tree.pos, "a").setInfo(definitions.IntClass.tpe);
        val x = currentOwner.newValueParameter(tree.pos, "x").setInfo(definitions.IntClass.tpe);
        val y = currentOwner.newValueParameter(tree.pos, "y").setInfo(definitions.IntClass.tpe);
        val target2 = currentOwner.newLabel(tree.pos, "target2")
        .setInfo(new MethodType(List(definitions.IntClass.tpe, definitions.IntClass.tpe), definitions.IntClass.tpe));

        val z = currentOwner.newValueParameter(tree.pos, "z").setInfo(definitions.IntClass.tpe);
        typed { atPos(tree.pos) { Block(
          List(
            Apply(Ident(target2), List(Literal(Constant(1)),Literal(Constant(2)))),
            LabelDef(target1, List(x), exp)),
            LabelDef(target2, List(y,z), Apply(Select(Ident(y), nme.PLUS), List(Ident(z))))
        )}};
*/

      case Match(selector, cases) =>
        val nselector = transform(selector).setType(selector.tpe)
        val ncases = cases map { transform }
/*
      Console.println("TransMatch translating cases: ")
      for(val t <- cases) {
        Console.println(t.pat.toString())
        Console.println("BODY "+t.body.toString())
      }
*/
        // @todo: remove from partial matcher
        TransMatcher.this.currentOwner = currentOwner
        TransMatcher.this.resultType = tree.tpe
        //Console.println("TransMatcher currentOwner ="+currentOwner+")")
        //Console.println("TransMatcher selector.tpe ="+selector.tpe+")")
        //Console.println("TransMatcher resultType ="+resultType+")")
        val t_untyped = handle(nselector, ncases.asInstanceOf[List[CaseDef]])
        //Console.println("t_untyped "+t_untyped.toString())
        val t         = atPos(tree.pos) { typer.atOwner(tree,currentOwner).typed(t_untyped, resultType) }
        //val t         = atPos(tree.pos) { typed(t_untyped, resultType) }
        //val t         = atPos(tree.pos) { typed(t_untyped) }
        //Console.println("t typed "+t.toString())
        t
      case _ =>
        super.transform(tree)
    }
  }
}
