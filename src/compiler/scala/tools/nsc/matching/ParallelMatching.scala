package scala.tools.nsc.matching

import compat.StringBuilder
import scala.tools.nsc.util.Position

/** utility methods (not just for ParallelMatching) */
trait ParallelMatching requires (transform.ExplicitOuter with PatternMatchers with CodeFactory) {

  import global._

  final def DEBUG(x:String) = {if (settings.debug.value) Console.println(x)}
  // ----------------------------------   data

  sealed trait RuleApplication
  case class ErrorRule    extends RuleApplication
  case class VariableRule(subst:List[Pair[Symbol,Symbol]], guard: Tree, body: Tree) extends RuleApplication

  def MixtureRule(scrutinee:Symbol, column:List[Tree], rest:Rep): MixtureRule = {
    if((scrutinee.tpe =:= definitions.IntClass.tpe) && (column forall {case Literal(c) => true case _ => false})) {
      throw CantOptimize
      //@todo: add SwitchRule, a special case of MixtureRule
    } else {
      new MixtureRule(scrutinee, column, rest)
    }
  }
  /*
  class SwitchRule(val scrutinee:Symbol, val column:List[Tree], val rest:Rep) extends MixtureRule(scrutinee,column,rest) {
    def getTransition() = {
      //here, we generate a switch tree, bodies of the switch is the corresponding [[row in rest]]
	  //if a default case has to be handled, body is [[that row + the default row]]
    }
  }
  */
  class MixtureRule(val scrutinee:Symbol, val column:List[Tree], val rest:Rep) extends RuleApplication {

    var parent: Rep = null
    def setParent(rep:Rep) = { parent = rep; this }
    var casted: Symbol = null
    var moreSpecific:   List[Tree]        = Nil
    var subsumed:  List[(Int,List[Tree])] = Nil // row index and subpatterns
    var remaining: List[(Int,Tree)]       = Nil // row index and pattern

    val isExhaustive = !scrutinee.tpe.symbol.hasFlag(symtab.Flags.SEALED) || {
      //DEBUG("check exha for column "+column)
      val tpes = column.map (.tpe.symbol)
      scrutinee.tpe.symbol.children.forall { sym => tpes.contains(sym) }
    }

    //DEBUG("Mixture, is exhaustive? "+isExhaustive)
    //if(!isExhaustive)
    //  cunit.warning(column.head.pos, "match is not exhaustive, I think (first approx)")
    private val patternType     = column.head match {
      case p@(_:Ident | _:Select) => singleType(p.symbol.tpe.prefix, p.symbol)  //ConstantType(p.tpe.singleton)
      //case p@Apply(_,_) if !p.tpe.symbol.hasFlag(symtab.Flags.CASE) => ConstantType(new NamedConstant(p))
      case _ => column.head.tpe
    }
    private val isCaseScrutinee = patternType.symbol.hasFlag(symtab.Flags.CASE)
    private val dummies = if(!isCaseScrutinee) Nil else patternType.symbol.caseFieldAccessors.map { x => EmptyTree }
    private def subpatterns(pat:Tree): List[Tree] = pat match {
      case Bind(_,p)                                                          => subpatterns(p)
      case app @ Apply(fn, pats) if app.tpe.symbol.hasFlag(symtab.Flags.CASE) => pats
      case _: UnApply                                                         => throw CantHandleUnapply
      case pat                                                                => /*DEBUG("dummy patterns for "+pat+" of class "+pat.getClass);*/dummies
    }
      // more specific patterns, subpatterns, remaining patterns
    private var sr = column.zipWithIndex.foldLeft (moreSpecific,subsumed,remaining) {
      (p,patAndIndex) =>
        val (ms,ss,rs) = p
        val (pat,j)    = patAndIndex
      //Console.println("pat.tpe = "+pat.tpe)
      //Console.println("patternType = "+patternType)
      pat match {
        case Literal(Constant(null)) if !(patternType =:= pat.tpe) => //special case for constant null pattern
          (ms,ss,(j,pat)::rs)
        case _ if (pat.symbol ne null) && (patternType =:= singleType(pat.symbol.tpe.prefix, pat.symbol))=>
          (EmptyTree::ms, (j,dummies)::ss, rs);                                                 // matching an object
        case _ if (pat.tpe <:< patternType) =>
          ({if(pat.tpe =:= patternType) EmptyTree else pat}::ms, (j,subpatterns(pat))::ss, rs); // subsumed (same or more specific) pattern;
        case _ if (patternType <:< pat.tpe) || isDefaultPattern(pat) =>
          (EmptyTree::ms, (j,dummies)::ss, (j,pat)::rs);                                        // subsuming (matched *and* remaining pattern)
        case _ =>
          (ms,ss,(j,pat)::rs)
      }
    }
    this.moreSpecific = sr._1.reverse
    this.subsumed     = sr._2.reverse
    this.remaining    = sr._3.reverse
    sr = null
    override def toString = {
      "MixtureRule("+scrutinee+":"+scrutinee.tpe+") {\n  moreSpecific:"+moreSpecific+"\n  subsumed:"+subsumed+"\n  remaining"+remaining+"\n}"
    }

    def getTransition(implicit theOwner: Symbol): (Symbol, Rep, Option[Rep]) = {
      DEBUG("*** getTransition! of "+this.toString)
      // the following works for type tests... what fudge is necessary for value comparisons?
      // type test
      casted = if(scrutinee.tpe =:= patternType) scrutinee else newVar(scrutinee.pos, patternType)
      if(scrutinee.hasFlag(symtab.Flags.CAPTURED))
        casted.setFlag(symtab.Flags.CAPTURED)
      // succeeding => transition to translate(subsumed) (taking into account more specific)
      val nmatrix = {
        var ntemps   = casted.caseFieldAccessors map {
          meth =>
            val ctemp = newVar(scrutinee.pos, casted.tpe.memberType(meth).resultType)
            if(scrutinee.hasFlag(symtab.Flags.CAPTURED))
              ctemp.setFlag(symtab.Flags.CAPTURED)
            ctemp
        } // (***)
        var subtests = subsumed

        if(moreSpecific.exists { x => x != EmptyTree }) {
          ntemps   = casted::ntemps                                                                                 // (***)
          subtests = moreSpecific.zip(subsumed) map { case (mspat, (j,pats)) => (j,mspat::pats) }
        }
        ntemps = ntemps ::: rest.temp
        val ntriples = subtests map {
          case (j,pats) =>
            val (vs:List[Symbol],_) = strip(column(j));
            val (opats,osubst:List[(Symbol,Symbol)],og,ob) = rest.row(j);
            val subst1:List[(Symbol,Symbol)] = vs map { v => (v,casted) }

            DEBUG("getTransition, vs = "+vs)
            DEBUG("getTransition, subst1 = "+subst1)

          // def doSubst(vs:List[Symbol], exp:Tree) = { new TreeSymSubstituter(vs,vs map {x=> casted}).traverse(exp); exp }

          // don't substitute eagerly here, problems with bodies that can be reached with several routes
          // problems would disappear if we didn't aim for sharing code and &^% duplicate would correctly duplicate definitions (alloc new symbols)

            (pats ::: opats, osubst ::: subst1, og, ob) // don't duplicate body/guard, get "defined twice" error cause hashing breaks
        }
        Rep(ntemps, ntriples) setParent this
      }
      DEBUG("nmatrix for type "+patternType)
      DEBUG(nmatrix.toString)

      // and then more or this... Console.println(nmatrix.applyRule)
      // CONTINUE HERE: epsilon transitions, which ensure that transitions are tested in the right order.

      // fails      => transition to translate(remaining)

      val nmatrixFail: Option[Rep] = {
        val ntemps   = scrutinee::rest.temp
        val ntriples = remaining map {
          case (j, pat) => val r = rest.row(j);  (pat :: r._1, r._2, r._3, r._4)
        }
        if(ntriples.isEmpty) None else Some(Rep(ntemps, ntriples) setParent this)
      }
      if(!nmatrixFail.isEmpty) {
        DEBUG("nmatrix for failing type test "+patternType)
        DEBUG(nmatrixFail.get.toString)
      } else {
        DEBUG("pattern type "+patternType+" cannot fail for "+scrutinee)
      }
      (casted, nmatrix, nmatrixFail)
    } // end getTransitions
  }

  def repToTree(rep:Rep, typed:Tree => Tree, handleOuter: Tree => Tree)(implicit theOwner: Symbol, failTree: Tree, bodies: collection.mutable.Map[Tree,(Tree,Tree, Symbol)]): Tree = {
    rep.applyRule match {
      case VariableRule(subst, EmptyTree, b) => bodies.get(b) match {
        case Some(EmptyTree, b, theLabel) =>
          // (**) approach 2
          val body  = Apply(Ident(theLabel), subst.map { p => Ident(p._2) })
          return body

        // (*) approach 1
          //DEBUG("--- variable \n JUMP ! subst = "+subst)
          val vdefs = subst map {
            case (v,t) => ValDef(v, {v.setFlag(symtab.Flags.TRANS_FLAG);
                                     if(v.tpe <:< t.tpe) typed{gen.mkAsInstanceOf(Ident(t),v.tpe)} /*refinement*/ else typed{Ident(t)}})
          }
        // @todo! transfer variables/subst
          Apply(Ident(theLabel), List())
        case None    =>
          //DEBUG("--- variable \n new  ! subst = "+subst)
        val theLabel = theOwner.newLabel(b.pos, "body"+b.hashCode).setInfo(new MethodType(subst map { case (v,_) => v.tpe}, b.tpe))
        // make new value parameter for each vsym in subst
        // (**) approach 2
        //val pdefsyms = subst map { case (v,tmp) => theLabel.newValueParameter(v.pos, v.name+"_!") . setInfo (v.tpe). setFlag(symtab.Flags.MUTABLE) }
        DEBUG("subst of "+b.hashCode+"! in case VariableRule (new) "+subst)
        val vdefs    = subst map { case (v,t) => ValDef(v, {v.setFlag(symtab.Flags.TRANS_FLAG);
                                                            if(v.tpe =:= t.tpe) typed{Ident(t)} else typed{gen.mkAsInstanceOf(Ident(t),v.tpe)}}) }
        // this weird thing should only be done for shared states.
        val dom = subst.map(._1)
        //val tss = new TreeSymSubstituter(dom,pdefsyms)
        var nbody: Tree = b
        //tss.traverse(nbody)
        val vrefs = vdefs.map { p:ValDef => Ident(p.symbol) }
        nbody  = Block(vdefs:::List(Apply(Ident(theLabel), vrefs)), LabelDef(theLabel, subst.map(._1), nbody))
        /*
        val body  = LabelDef(theLabel, List(), b.duplicate)
        // (*) approach 1
        val (dom,cod) = List.unzip(subst map {
          case (v,t) => (v,{if(v.tpe =:= t.tpe) typed{Ident(t)} else typed{gen.mkAsInstanceOf(Ident(t),v.tpe)}})})
        val tss = new TreeSubstituter(dom,cod)
        val nbody = tss.transform(body)
        //@todo
        // instead of substitution, use valdefs
        // val vdefs = subst map {
        //   case (v,t) => ValDef(v, {if(v.tpe <:< t.tpe) typed{gen.mkAsInstanceOf(Ident(t),v.tpe)} /*refinement*/ else typed{Ident(t)}})
        //}

      */
        bodies(b) = (EmptyTree, nbody, theLabel)
        nbody
      }
      case VariableRule(subst,g,b) =>
        throw CantHandleGuard
      case mm:MixtureRule =>
        val (casted,srep,frep) = mm.getTransition
        //DEBUG("--- mixture \n succ \n"+srep.toString+"\n fail\n"+frep.toString)
        //val cond = typed{gen.mkIsInstanceOf(Ident(mm.scrutinee), casted.tpe)}
        var cond = typed { condition(casted.tpe, mm.scrutinee) }
        if(needsOuterTest(casted.tpe, mm.scrutinee.tpe)) // @todo merrge into def condition
          cond = addOuterCondition(cond, casted.tpe, typed{Ident(mm.scrutinee)}, handleOuter)
        val succ = repToTree(srep, typed, handleOuter)
        val fail = if(frep.isEmpty) {
          /*
          if (true || casted.tpe.symbol.hasFlag(symtab.Flags.SEALED)) {
            cunit.warning(casted.pos, "match not exhaustive! ");
            var rep1 = rep
            var mr,last : MixtureRule   = null
            while((rep1 ne null) && {mr = rep1.mixtureParent; mr ne null}) {
              Console.println("casted "+casted+" !?:"+mr.scrutinee+" ? "+mr.column.head.tpe)
              Console.println(mr.isExhaustive)
              rep1 = mr.parent
              last = mr
            }
          }
          Console.println("BASTA")
          */
          failTree
        } else repToTree(frep.get, typed, handleOuter)

        // dig out case field accessors that were buried in (***)
        val cfa  = casted.caseFieldAccessors
        //DEBUG(casted.toString)
        //DEBUG(cfa.toString)
        val caseTemps = (if(!srep.temp.isEmpty && srep.temp.head == casted) srep.temp.tail else srep.temp).zip(cfa)

        //DEBUG(caseTemps.toString)
        var vdefs     = caseTemps map {
          case (tmp,meth) =>
            val typedAccess = typed { Apply(Select(typed{Ident(casted)}, meth),List()) }
            typed { ValDef(tmp, typedAccess) }
        }

        vdefs = typed { ValDef(casted, gen.mkAsInstanceOf(typed{Ident(mm.scrutinee)}, casted.tpe))} :: vdefs
      def makeIf(cond:Tree, thenp:Tree, elsep:Tree) = cond match {
        case Literal(Constant(true)) => thenp
        case Literal(Constant(false)) => elsep
        case _ => If(cond, thenp, elsep)
      }
        typed { makeIf(cond, /*squeezed*/Block(vdefs,succ), fail) }
    }
  }

    def makeRep(temp:List[Symbol], row1:List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)]/*, exCh:List[Boolean]*/): Rep = {
      var i = -1
      val row = row1 flatMap {
        xx =>
          def isAlternative(p: Tree): Boolean = p match {
            case Bind(_,p)       => isAlternative(p)
            case Alternative(ps) => true
            case _               => false
          }
          def getAlternativeBranches(p:Tree): List[Tree] = {
            def get_BIND(pctx:Tree => Tree, p:Tree):List[Tree] = p match {
              case b @ Bind(n,p)       => get_BIND({ x:Tree => pctx(copy.Bind(b, n, x) setType x.tpe) }, p)
              case Alternative(ps) => ps map pctx
            }
            get_BIND({x=>x}, p)
          }
          val (pats,subst,g,b) = xx
          i = pats findIndexOf isAlternative
          if(i == -1)
            List((pats,subst,g,b))
          else {
            val prefix:List[Tree] = pats.take(i)
            val alts  = getAlternativeBranches(pats(i))
            val suffix:List[Tree] = pats.drop(i+1)
            alts map { p => (prefix ::: p :: suffix, subst, g, b) }
          }
      }
      if(i == -1)
        Rep(temp,row/*,ex*/)
      else
        makeRep(temp,row/*,ex*/)
    }

  case class Rep(val temp:List[Symbol], val row:List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)]/*, val exhaustivenessChecked:List[Boolean]*/) {
    assert(row.forall { case (pats,subst,g,b) => temp.length == pats.length })

    var sealedCols = List[Int]()
    var sealedComb = List[Set[Symbol]]()
    //Console.println(" the matrix "+this.toString)
    temp.zipWithIndex.foreach {
      case (sym,i) =>
        //Console.println("sym! "+sym+" mutable? "+sym.hasFlag(symtab.Flags.MUTABLE)+" captured? "+sym.hasFlag(symtab.Flags.CAPTURED))
        if (sym.hasFlag(symtab.Flags.MUTABLE) &&  // indicates that have not yet checked exhaustivity
            !sym.hasFlag(symtab.Flags.CAPTURED) &&  // indicates presence of catch-all at higher level
            sym.tpe.symbol.hasFlag(symtab.Flags.SEALED)) {

              sym.resetFlag(symtab.Flags.MUTABLE)
              if(row.exists { case (pats,_,_,_) => isDefaultPattern(pats(i)) })
                sym.setFlag(symtab.Flags.CAPTURED) // mark presence of catch-all
              else {
                sealedCols = i::sealedCols
                // this should enumerate all cases... however, also the superclass is taken if it is not abstract
                def checkExCoverage(tpesym:Symbol): SymSet =
                  if(!tpesym.hasFlag(symtab.Flags.SEALED)) emptySymbolSet else
                    tpesym.children.flatMap { x =>
                      val z = checkExCoverage(x)
                                             if(x.hasFlag(symtab.Flags.ABSTRACT)) z else z + x
                                           }

                val cases = checkExCoverage(sym.tpe.symbol)
                sealedComb = cases::sealedComb
              }
            }
    }
    //  computes cartesian product, keeps indices available
    def combine(colcom: List[(Int,Set[Symbol])]): List[List[(Int,Symbol)]] = colcom match {
      case Nil => Nil
      case (i,syms)::Nil  => syms.toList.map { sym => List((i,sym)) }
      case (i,syms)::cs   => for(val s <- syms.toList; val rest <- combine(cs)) yield (i,s) :: rest
    }

    if(!sealedCols.isEmpty) {
      DEBUG("cols"+sealedCols)
      DEBUG("comb")
      for(val com <- sealedComb)
        DEBUG(com.toString)

      val allcomb = combine(sealedCols zip sealedComb)
      //Console.println("all comb!" + allcomb)
      /** returns true if pattern vector pats covers a type symbols "combination"
       *  @param pats pattern vector
       *  @param comb pairs of (column index, type symbol)
       */
      def covers(pats: List[Tree], comb:List[(Int,Symbol)]) = {
        comb forall { case (i,sym) => val p = pats(i); p.tpe.symbol == sym || sym.tpe <:< p.tpe }
      }
      val coversAll = allcomb forall { combination => row exists { r => covers(r._1, combination)}}
      //Console.println("all combinations covered? "+coversAll)
      if(!coversAll) {
        val sb = new compat.StringBuilder()
        sb.append("match is not exhaustive!\n")
        for(val open <- allcomb;
            !(row exists { r => covers(r._1, open)})) {
              sb.append("missing combination ")
              val NPAD = 15
              def pad(s:String) = { Iterator.range(1,NPAD - s.length).foreach { x => sb.append(" ") }; sb.append(s) }
              List.range(0, temp.length) foreach {
                 i => open.find { case (j,sym) => j==i } match {
                  case None => pad("*")
                  case Some((_,sym)) => pad(sym.name.toString)
                }
              }
              sb.append('\n')
            }
        cunit.warning(temp.head.pos, sb.toString)
      }
    }
    // if this was the *fail* branch, the Rep preceding this Rep
    var mixtureParent: MixtureRule = null

    def setParent(mr:MixtureRule): this.type = { mixtureParent = mr; this }
    //assert(temp.length == exhaustivenessChecked.length)
    /*
    def prepend(temp1:List[Symbol], row2:List[List[Tree]]) = {
      assert(row.length == row2.length)
      Rep(temp1 ::: temp, row.zip(row2) map { case ((pats,g,b),pats2) => (pats2:::pats,g,b) })
    }
    */
    def applyRule: RuleApplication = row match {
      case Nil            => ErrorRule
      case (pats,subst,g,b)::xs =>
        if(pats forall isDefaultPattern) {
          val subst1 = pats.zip(temp) flatMap { case (p,tmp) => val (vs,_) = strip(p); vs.zipAll(Nil,null,tmp)}
          DEBUG("applyRule! subst1="+subst1)
          VariableRule (subst:::subst1, g, b)
        } else {
          val i = pats findIndexOf {x => !isDefaultPattern(x)}

          val column = row map { case (pats,subst,g,b) => pats(i) }
          //val ex = exhaustivenessChecked(i)
          val restTemp =                                     temp.take(i) ::: temp.drop(i+1)
          val restRows = row map { case (pats,subst,g,b) => (pats.take(i) ::: pats.drop(i+1),subst,g,b) }
          //val restEx   =                    exhaustivenessChecked.take(i) ::: exhaustivenessChecked.drop(i+1)
          MixtureRule(temp(i), column, makeRep(restTemp,restRows/*,restEx*/)) setParent this
        }
    }
    // a fancy toString method for debugging
    override def toString = {
      val sb   = new StringBuilder
      val NPAD = 15
      def pad(s:String) = { Iterator.range(1,NPAD - s.length).foreach { x => sb.append(" ") }; sb.append(s) }
      for(val tmp <- temp) pad(tmp.name.toString)
      sb.append('\n')
      for(val (r,i) <- row.zipWithIndex) {
        for(val c <- r._1 ::: List(r._2, r._3)) {
          pad(c.toString)
        }
        sb.append('\n')
      }
      sb.toString
    }
  }

  def initRep(selector:Tree, cases:List[Tree], checkExhaustive: Boolean)(implicit theOwner: Symbol) = {
    val root = newVar(selector.pos, selector.tpe)
    // communicate whether exhaustiveness-checking is enabled via some flag
    if(!checkExhaustive)
      root.setFlag(symtab.Flags.CAPTURED)
    val row  = cases map { case CaseDef(pat,g,b) => (List(pat),List(),g,b) }
    makeRep(List(root), row)
  }
  /** this tree node is used several times in the parallel algo and will never be needed for matching, so it is reused */
   val Ident_WILDCARD = Ident(nme.WILDCARD) setType definitions.AnyClass.tpe

  val NoSymbol_Ident_WILDCARD = (NoSymbol, EmptyTree);

  // ----------------------------------   helper functions that extract information from patterns, symbols, types

  /** returns if pattern can be considered a no-op test ??for expected type?? */
  def isDefaultPattern(pattern:Tree): Boolean = pattern match {
    case Bind(_, p)            => isDefaultPattern(p)
    case EmptyTree             => true // dummy
    case Ident(nme.WILDCARD)   => true
    case _                     => false
// -- what about the following? still have to test "ne null" :/
//  case Typed(nme.WILDCARD,_) => pattern.tpe <:< scrutinee.tpe
  }

  /** returns all variables that are binding the given pattern */
  def strip(x:Tree): (List[Symbol], Tree) = x match {
    case b @ Bind(_,pat) => val (vs,p) = strip(pat); (b.symbol :: vs,p)
    case z               => (Nil,z)
  }

  // ----------------------------------   functions used in internal data structure of the algorithm (matrix)

  /** an entry ((x,m),y) hints at a case field access "val y = x.m" (@see tempValdef). we can recover the parent (@see getParent)
   */
//  type TempMap = collection.mutable.Map[(Symbol,Symbol),Symbol]

  /** returns the temp which is the "parent" of the given temp */
/*
  def getParent(sym:Symbol)(implicit memo: TempMap): Option[((Symbol, Symbol), Symbol)] = memo.elements.find { x => x._2 == sym }

  def getChild(pos: PositionType, scrutineeSymbol:Symbol, theCaseFieldAccessor:Symbol)(implicit memo: TempMap, theOwner: Symbol) = {
    val p = (scrutineeSymbol,theCaseFieldAccessor)
    memo.get (p) match {
      case Some(v) => v
      case None    =>  val v = newVar(pos, scrutineeSymbol.tpe.memberType(theCaseFieldAccessor).resultType);  memo.update(p, v); v
    }
  }

  def tempValDef1(tmpsym:Symbol)(implicit memo: TempMap): Option[Tree] =
    getParent(tmpsym) match {
      case Some((par,meth),_) => Some(ValDef(tmpsym, Select(Ident(par),meth)))
      case None               => None //assert(scrut == root.casted); Block(List(ValDef(scrut, selector)), theTest)
    }
*/
  /** same as tempValDef, but with a default (typically root=selector)
  def tempValDef(tmpsym:Symbol, default: Tree)(implicit memo: TempMap): Tree =  // overloading+implicit+tuple == bug
    getParent(tmpsym) match {
      case Some((par,meth),_) => ValDef(tmpsym, Select(Ident(par),meth))
      case None               => ValDef(tmpsym, default)
    }
*/

  /** an entry ((x,T),y) hints at a case field access "val y = x.asInstanceOf[T]" (@see caseValdef).
  type CastMap = collection.mutable.Map[(Symbol,Type),Symbol]

  def getCasted(oldScrutineeSymbol: Symbol, patternType: Type)(implicit castMap: CastMap, theOwner: Symbol) = {
    val p = (oldScrutineeSymbol, patternType)
    castMap.get (p) match {
      case Some(v) => v
      case None    => val v = newVar(oldScrutineeSymbol.pos, patternType); castMap.update(p,v); v
    }
  }

  def castValDef(oldScrutinee: Symbol, tpe:Type)(implicit castMap: CastMap): Tree  = {
    val newScrutinee = castMap.get((oldScrutinee, tpe)).get
    ValDef(newScrutinee, gen.mkAsInstanceOf(Ident(oldScrutinee), tpe))
  }
*/
  /** creates a new entry for casting, and replaces the scrutinee in give patterns
  def doCast(scrutineeSymbol:Symbol, pattern:Tree)(implicit castMap: CastMap, theOwner: Symbol) = {
    val patternType = pattern.tpe
    val newScrutinee = getCasted(scrutineeSymbol, patternType)
    (newScrutinee, pattern)
  }
*/
  // access to children for matched constructor patterns

  /** for a case pattern, returns subpatterns, otherwise a sequence of dummy wildcard patterns
   *  @param x tuple2 where _1 is the temp containing the scrutinee, _2 is the pattern
  def childPats(x:(Symbol,Tree), len:Int)(implicit memo: TempMap, theOwner: Symbol) = {
    def childPatsOfPattern(p:Tree): List[(Symbol,Tree)] = p match {
      case Bind(_, p)  => childPatsOfPattern(p)

      // for case patterns, get children... condition necessary since e.g. pattern 'nme.CONSTRUCTOR' is also an Apply(fn,())
      case app @ Apply(fn, patargs) if app.tpe.symbol.hasFlag(symtab.Flags.CASE) =>
        val patternSymbol   = app.tpe.symbol
        val scrutineeSymbol = x._1
        assert(  patternSymbol.caseFieldAccessors.length == len)
        assert(scrutineeSymbol.tpe.symbol.tpe =:= patternSymbol.tpe)
        patternSymbol.caseFieldAccessors.map( { z => getChild(app.pos, scrutineeSymbol, z) }).zip(patargs)
      case _                   =>
        List.range(0,len) map { x => NoSymbol_Ident_WILDCARD } //(NoSymbol, Ident_WILDCARD)
    }
    childPatsOfPattern(x._2)
  }
*/

  // ----------------------------------   helper functions that generate symbols, trees for type tests, pattern tests

  def newVar(pos: Position, name: Name, tpe: Type)(implicit theOwner: Symbol): Symbol = {
    if(tpe eq null) assert(tpe ne null, "newVar("+name+", null)")
    val sym = theOwner.newVariable(pos, name) // careful: pos has special meaning
    sym.setFlag(symtab.Flags.TRANS_FLAG)
    sym.setInfo(tpe)
    sym
  }

  def newVar(pos: Position, tpe: Type)(implicit theOwner: Symbol): Symbol =
    newVar(pos, cunit.fresh.newName("temp"), tpe).setFlag(symtab.Flags.SYNTHETIC)

  /** returns the condition in "if(cond) k1 else k2"
   */
  def condition(tpe: Type, scrut: Symbol): Tree = {
    val res = condition1(tpe, scrut)
    DEBUG("condition, tpe = "+tpe+", scrut.tpe = "+scrut.tpe+", res = "+res)
    res
  }
  def condition1(tpe: Type, scrut: Symbol): Tree = {
    assert (scrut ne NoSymbol)
    condition(tpe, Ident(scrut) . setType (scrut.tpe) . setSymbol (scrut))
  }

  def condition(tpe: Type, scrutineeTree: Tree): Tree = {
    assert(  tpe ne NoType  )
    assert(scrutineeTree.tpe ne NoType)
    if(tpe.isInstanceOf[SingletonType] && !tpe.isInstanceOf[ConstantType]) {
      if(scrutineeTree.tpe <:< definitions.AnyRefClass.tpe)
        Eq(gen.mkAttributedRef(tpe.symbol), scrutineeTree)             // object
      else
        Equals(gen.mkAttributedRef(tpe.symbol), scrutineeTree)             // object
    } else if(tpe.isInstanceOf[ConstantType]) {
      val value = tpe.asInstanceOf[ConstantType].value
      //if(false && value.isInstanceOf[NamedConstant])
      //  Equals(Ident(scrut), value.asInstanceOf[NamedConstant].tree)             // constant
      //assert(scrut.tpe <:< definitions.AnyRefClass.tpe, "stupid, should be caught by type checker "+value)
      //else
      if(value == Constant(null) && scrutineeTree.tpe <:< definitions.AnyRefClass.tpe)
        Eq(scrutineeTree, Literal(value))             // constant
      else
        Equals(scrutineeTree, Literal(value))             // constant
    } else if(scrutineeTree.tpe <:< tpe && tpe <:< definitions.AnyRefClass.tpe)
      NotNull(scrutineeTree)
    else if(tpe.prefix.symbol.isTerm && tpe.symbol.linkedModuleOfClass != NoSymbol) { // object
      //Console.println("iT"+tpe.prefix.symbol.isTerm)
      //Console.println("lmoc"+tpe.symbol.linkedModuleOfClass)
      Eq(gen.mkAttributedRef(tpe.prefix, tpe.symbol.linkedModuleOfClass), scrutineeTree)
    } else
      //Console.println(tpe.prefix.symbol.isTerm)
      //Console.println(tpe.symbol)
      //Console.println(tpe.symbol.linkedModuleOfClass)
      gen.mkIsInstanceOf(scrutineeTree, tpe)
  }

  def needsOuterTest(tpe2test:Type, scrutinee:Type) = tpe2test.normalize match {
    case TypeRef(prefix,_,_) =>
      prefix.symbol.isTerm &&
    !prefix.symbol.isPackage &&
      outerAlwaysEqual(tpe2test,scrutinee) == Some(false)
    case _ => false
  }

  /** returns a result if both are TypeRefs, returns Some(true) if left and right are statically known to have
   *  the same outer, i.e. if their prefixes are the same
   */
  def outerAlwaysEqual(left: Type, right: Type): Option[Boolean] = (left.normalize,right.normalize) match {
    case (TypeRef(lprefix, _,_), TypeRef(rprefix,_,_)) =>
      if(!(lprefix =:= rprefix)) {
        DEBUG("DEBUG(outerAlwaysEqual) Some(f) for"+(left,right))
      }
      Some(lprefix =:= rprefix)
    case _                                             => None
  }

  /** adds a test comparing the dynamic outer to the static outer */
  def addOuterCondition(cond:Tree, tpe2test: Type, scrutinee: Tree, handleOuter: Tree=>Tree) = {
    val TypeRef(prefix,_,_) = tpe2test
    var theRef = gen.mkAttributedRef(prefix.prefix, prefix.symbol)

    // needs explicitouter treatment
    theRef = handleOuter(theRef)

    val outerAcc = outerAccessor(tpe2test.symbol)
    if(outerAcc == NoSymbol) {
      if(settings.debug.value) cunit.warning(scrutinee.pos, "no outer acc for "+tpe2test.symbol)
      cond
    } else
      And(cond,
          Eq(Apply(Select(
            gen.mkAsInstanceOf(scrutinee, tpe2test, true), outerAcc),List()), theRef))

  }

}
