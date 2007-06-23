/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import scala.tools.nsc.util.Position

/** Utility methods (not just for ParallelMatching).
 *
 *  @author Burak Emir
 */
trait ParallelMatching  {
  self: transform.ExplicitOuter with PatternMatchers with CodeFactory =>

  import global._

  // ----------------------------------   data

  sealed trait RuleApplication
  case class ErrorRule extends RuleApplication
  case class VariableRule(subst:List[Pair[Symbol,Symbol]], guard: Tree, body: Tree) extends RuleApplication

  def MixtureRule(scrutinee:Symbol, column:List[Tree], rest:Rep): RuleApplication = {
    def isSimpleIntSwitch: Boolean = {
      (isSameType(scrutinee.tpe.widen, definitions.IntClass.tpe)) && {
        var xs = column
        while(!xs.isEmpty) { // forall
          val h = xs.head
          if(h.isInstanceOf[Literal] || isDefaultPattern(h)) { xs = xs.tail } else return false
        }
        return true
      }}
    // an unapply for which we don't need a type test
    def isUnapplyHead: Boolean = {
      def isUnapply(x:Tree): Boolean = x match {
        case Bind(_,p) => isUnapply(p)
        case UnApply(Apply(fn,_),arg) => fn.tpe match {
          case MethodType(List(argtpe),_) =>
            //Console.println("scrutinee.tpe"+scrutinee.tpe)
            //Console.println("argtpe"+argtpe)
            val r = scrutinee.tpe <:< argtpe
            //Console.println("<: ???"+r)
          r
          case _                          =>
            //Console.println("wrong tpe")
            false
        }
        case x =>
          //Console.println("is something else"+x+" "+x.getClass)
          false
      }
      isUnapply(column.head)
    }
    //def containsUnapply = column exists { _.isInstanceOf[UnApply] }
    if(settings_debug) {
      Console.println("/// MixtureRule("+scrutinee.name+":"+scrutinee.tpe+","+column+", rep = ")
      Console.println(rest)
      Console.println(")///")
    }
    if(isSimpleIntSwitch) {
      if(settings_debug) { Console.println("MixLiteral") }
      return new MixLiterals(scrutinee, column, rest)
    }
    //Console.println("isUnapplyHead")
    if(isUnapplyHead) {
      if(settings_debug) { Console.println("MixUnapply") }
      return new MixUnapply(scrutinee, column, rest)
    }

    if(settings_debug) { Console.println("MixTypes") }
    return new MixTypes(scrutinee, column, rest) // todo: handle type tests in unapply
  }

  /**
   *  mixture rule for literals
   *
   *  this rule gets translated to a switch. all defaults/same literals are collected
  **/
  class MixLiterals(val scrutinee:Symbol, val column:List[Tree], val rest:Rep) extends RuleApplication {

    var defaults: List[Int]    = Nil
    var defaultV: List[Symbol] = Nil

    // sorted e.g. case _ => 1,5,7
    private def insertDefault(tag: Int,vs:List[Symbol]) {
      def ins(xs:List[Int]):List[Int] = xs match {
        case y::ys if y > tag => y::ins(ys)
        case ys               => tag :: ys
      }
      defaultV = defaultV:::vs
      defaults = ins(defaults)
    }

    class TagIndexPair(val tag:Int, val index: Int, val next: TagIndexPair) {}
    // e.g. (1,1) (1,3) (42,2) for column {case ..1.. => ;; case ..42..=> ;; case ..1.. => }

    private def insertTagIndexPair(tag: Int, index: Int) {
      def ins(current: TagIndexPair): TagIndexPair = {
        if (current eq null)
          new TagIndexPair(tag, index, null)
        else if (tag > current.tag) // maintain relative order
          new TagIndexPair(current.tag, current.index, ins(current.next))
        else
          new TagIndexPair(tag, index, current)
      }
      tagIndexPairs = ins(tagIndexPairs)
    }

    var tagIndexPairs: TagIndexPair = null

    private def sanity(pos:Position,pvars:List[Symbol]) {
      if(!pvars.isEmpty) cunit.error(pos, "nonsensical variable binding")
    }

    {
      var xs = column
      var i = 0;
      while(!xs.isEmpty) { // forall
        val (pvars,p) = strip(xs.head)
        p match {
          case Literal(Constant(c:Int))  => sanity(p.pos, pvars); insertTagIndexPair(c,i)
          case Literal(Constant(c:Char)) => sanity(p.pos, pvars); insertTagIndexPair(c.toInt,i)
          case p  if isDefaultPattern(p) => insertDefault(i,pvars)
        }
        i = i + 1
        xs = xs.tail
      }
    }
    def tagIndicesToReps = {

      var trs:List[(Int,Rep)] = Nil
      while(tagIndexPairs ne null) { // collect all with same tag
        val tag = tagIndexPairs.tag
        var tagRows = rest.row(tagIndexPairs.index)::Nil
        tagIndexPairs = tagIndexPairs.next
        while((tagIndexPairs ne null) && tagIndexPairs.tag == tag) {
          tagRows = rest.row(tagIndexPairs.index)::tagRows
          tagIndexPairs = tagIndexPairs.next
        }
        var tagRowsD:List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)] = Nil
        var ds = defaults; while(ds ne Nil) {
          tagRowsD = rest.row(ds.head) :: tagRowsD
          ds = ds.tail
        }
        trs = (tag, Rep(rest.temp, tagRows ::: tagRowsD)) :: trs
      }
      trs
    }
    def defaultsToRep = {
      var drows:List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)] = Nil
      while(defaults ne Nil) {
        drows = rest.row(defaults.head) :: drows
        defaults = defaults.tail
      }
      Rep(rest.temp, drows)
    }
    def getTransition(implicit theOwner: Symbol): (List[(Int,Rep)],List[Symbol],Option[Rep]) =
      (tagIndicesToReps, defaultV, {if(!defaults.isEmpty) Some(defaultsToRep) else None})
  }

  /**
   * mixture rule for unapply pattern
   */
  class MixUnapply(val scrutinee:Symbol, val column:List[Tree], val rest:Rep) extends RuleApplication {

    def newVarCapture(pos:Position,tpe:Type)(implicit theOwner:Symbol) = {
      val v = newVar(pos,tpe)
      if(scrutinee.hasFlag(symtab.Flags.CAPTURED))
        v.setFlag(symtab.Flags.CAPTURED)
      v
    }

    private def bindToScrutinee(x:Symbol) = typer.typed(ValDef(x,Ident(scrutinee)))

    val (_, unapp) = strip(column.head)
    /** returns the (un)apply and two continuations */
    var rootvdefs:List[Tree] = Nil // later, via bindToScrutinee

    def getTransition(implicit theOwner: Symbol): (Tree, List[Tree], Rep, Option[Rep]) = {
      unapp match {
        case ua @ UnApply(app @ Apply(fn, _), args) =>
          val ures = newVarCapture(ua.pos, app.tpe)
          val n    = args.length
          val uacall = typer.typed { ValDef(ures, Apply(fn, List(Ident(scrutinee)))) }
          //Console.println("uacall:"+uacall)

          val nrowsOther = column.tail.zip(rest.row.tail) flatMap { case (pat,(ps,subst,g,b)) => strip(pat)._2 match {
            case UnApply(Apply(fn1,_),args) if fn.symbol==fn1.symbol => Nil
            case p                                                   => List((p::ps, subst, g, b))
          }}
          val nrepFail = if(nrowsOther.isEmpty) None else Some(Rep(scrutinee::rest.temp, nrowsOther))
        //Console.println("active = "+column.head+" / nrepFail = "+nrepFail)
          n match {
            case 0  => //special case for unapply(), app.tpe is boolean
              val ntemps = scrutinee :: rest.temp
              val nrows  = column.zip(rest.row) map { case (pat,(ps,subst,g,b)) => strip(pat) match {
                case (pvars,UnApply(Apply(fn1,_),args)) if fn.symbol==fn1.symbol =>
                  rootvdefs = (pvars map bindToScrutinee) ::: rootvdefs
                  (EmptyTree::ps, subst, g, b)
                case (_, p)                                                      =>
                  (     p   ::ps,         subst, g, b)
              }}
              (uacall, rootvdefs, Rep(ntemps, nrows), nrepFail)

            case  1 => //special case for unapply(p), app.tpe is Option[T]
              val vtpe = app.tpe.typeArgs(0)
              val vsym = newVarCapture(ua.pos, vtpe)
              val ntemps = vsym :: scrutinee :: rest.temp
              val nrows = column.zip(rest.row) map { case (pat,(ps,subst,g,b)) => strip(pat) match {
                case (pvars,UnApply(Apply(fn1,_),args)) if fn.symbol==fn1.symbol =>
                  rootvdefs = (pvars map bindToScrutinee) ::: rootvdefs
                  (args(0)   :: EmptyTree :: ps, subst, g, b)
                case (_, p)                                                      =>
                  (EmptyTree :: p     ::ps, subst, g, b)
              }}
              (uacall, rootvdefs:::List( typer.typed { ValDef(vsym, Select(Ident(ures), nme.get) )}), Rep(ntemps, nrows), nrepFail)

            case _ => // app.tpe is Option[? <: ProductN[T1,...,Tn]]
              val uresGet = newVarCapture(ua.pos, app.tpe.typeArgs(0))
              var vdefs = ValDef(uresGet, Select(Ident(ures), nme.get))::Nil
              var ts = definitions.getProductArgs(uresGet.tpe).get
              var i = 1;
              //Console.println("typeargs"+ts)
              var vsyms:List[Symbol] = Nil
              var dummies:List[Tree] = Nil
              while(ts ne Nil) {
                val vtpe = ts.head
                val vchild = newVarCapture(ua.pos, vtpe)
                val accSym = definitions.productProj(uresGet, i)
                val rhs = typer.typed(Apply(Select(Ident(uresGet), accSym), List())) // nsc !
                vdefs = ValDef(vchild, rhs)::vdefs
                vsyms   =  vchild  :: vsyms
                dummies = EmptyTree::dummies
                ts = ts.tail
                i = i + 1
              }
              val ntemps  =  vsyms.reverse ::: scrutinee :: rest.temp
              dummies     = dummies.reverse
              val nrows = column.zip(rest.row) map { case (pat,(ps,subst,g,b)) => strip(pat) match {
                case (pvars, UnApply(Apply(fn1,_),args)) if fn.symbol==fn1.symbol =>
                  rootvdefs = (pvars map bindToScrutinee) ::: rootvdefs
                  (   args::: EmptyTree ::ps, subst, g, b)
                case (_, p)                                                       =>
                  (dummies:::         pat      ::ps, subst, g, b)
              }}
              (uacall, rootvdefs:::vdefs.reverse, Rep(ntemps, nrows), nrepFail)
          }}
    }
  }

  /**
   *   mixture rule for type tests
  **/
  class MixTypes(val scrutinee:Symbol, val column:List[Tree], val rest:Rep) extends RuleApplication {

    var casted: Symbol = null
    var moreSpecific:   List[Tree]        = Nil
    var subsumed:  List[(Int,List[Tree])] = Nil // row index and subpatterns
    var remaining: List[(Int,Tree)]       = Nil // row index and pattern

    val isExhaustive = !scrutinee.tpe.symbol.hasFlag(symtab.Flags.SEALED) || {
      //DEBUG("check exha for column "+column)
      val tpes = column.map {x => /*Console.println("--x:"+x+":"+x.tpe); */ x.tpe.symbol}
      scrutinee.tpe.symbol.children.forall { sym => tpes.contains(sym) }
    }

    private val patternType     = column.head match {
      case p @ (_:Ident | _:Select) =>
        singleType(p.symbol.tpe.prefix, p.symbol)  //ConstantType(p.tpe.singleton)
      //case p@Apply(_,_) if !p.tpe.symbol.hasFlag(symtab.Flags.CASE) => ConstantType(new NamedConstant(p))
      case _ => column.head.tpe
    }
    private val isCaseHead = patternType.symbol.hasFlag(symtab.Flags.CASE)
    private val dummies = if(!isCaseHead) Nil else patternType.symbol.caseFieldAccessors.map { x => EmptyTree }

    //Console.println("isCaseHead = "+isCaseHead)
    //Console.println("dummies = "+dummies)

    private def subpatterns(pat:Tree): List[Tree] = {
      //Console.print("subpatterns("+pat+")=")
      //val x =
      pat match {
        case Bind(_,p)                                                          =>
          subpatterns(p)
        case app @ Apply(fn, pats) if app.tpe.symbol.hasFlag(symtab.Flags.CASE) && (fn.symbol eq null)=>
          pats
        case Apply(fn,xs) => assert((xs.isEmpty) && (fn.symbol ne null)); dummies // named constant
        case _: UnApply                                                         =>
          dummies
        case pat                                                                =>
          dummies
      }
      //Console.println(x)
      //x
    }

    /** returns true if pattern tests an object */
    def objectPattern(pat:Tree): Boolean = try {
      (pat.symbol ne null) &&
      (pat.symbol != NoSymbol) &&
      pat.symbol.tpe.prefix.isStable &&
      patternType =:= singleType(pat.symbol.tpe.prefix, pat.symbol)
    } catch {
      case e =>
        Console.println("object pattern test throws "+e.getMessage())
        if(settings_debug)
          System.exit(-1);
        throw e
    }
    {
      var sr = (moreSpecific,subsumed,remaining)
      var j = 0; var pats = column; while(pats ne Nil) {
        val (ms,ss,rs) = sr // more specific, more general(subsuming current), remaining patterns
        val pat = pats.head

        //Console.println("pat = "+pat+":"+pat.tpe)
        //Console.println("current pat is wild? = "+isDefault(pat))
        //Console.println("current pat.symbol = "+pat.symbol+", pat.tpe "+pat.tpe)
        //Console.println("patternType = "+patternType)
        //Console.println("(current)pat.tpe <:< patternType = "+(pat.tpe <:< patternType))
        //Console.println("patternType <:< (current)pat.tpe = "+(patternType <:< pat.tpe))
        //Console.println("(current)pat.tpe =:= patternType = "+(pat.tpe <:< patternType))

        sr = pat match {

          case a:Alternative =>
            if(settings_debug) {
              Console.println("this may not happen, alternatives should be preprocessed away")
              System.exit(-1)
            }
            throw InternalError
          case Literal(Constant(null)) if !(patternType =:= pat.tpe) => //special case for constant null pattern
            //Console.println("[1")
            (ms,ss,(j,pat)::rs);
          case _ if objectPattern(pat) =>
            //Console.println("[2")
            (EmptyTree::ms, (j,dummies)::ss, rs);                                 // matching an object

          case Typed(p:UnApply,_) if (pat.tpe <:< patternType) =>
            //Console.println("unapply arg is same or *more* specific")
            (p::ms, (j, dummies)::ss, rs);

          case q @ Typed(_,_) if (pat.tpe <:< patternType) =>
            //Console.println("current pattern is same or *more* specific")
            ({if(pat.tpe =:= patternType) EmptyTree else q}::ms, (j, dummies)::ss, rs);

          case _ if (pat.tpe <:< patternType) && !isDefaultPattern(pat) =>
            //Console.println("current pattern is same or *more* specific")
            ({if(pat.tpe =:= patternType) EmptyTree else pat}::ms, (j,subpatterns(pat))::ss, rs);

          case _ if (patternType <:< pat.tpe) || isDefaultPattern(pat) =>
            //Console.println("current pattern is *more general*")
            (EmptyTree::ms, (j,dummies)::ss, (j,pat)::rs);                        // subsuming (matched *and* remaining pattern)

          case _ =>
            //Console.println("current pattern tests something else")
            (ms,ss,(j,pat)::rs)
        }
        j = j + 1
        pats = pats.tail
      }
      this.moreSpecific = sr._1.reverse
      this.subsumed     = sr._2.reverse
      this.remaining    = sr._3.reverse
      sr = null
    }

    override def toString = {
      "MixTypes("+scrutinee+":"+scrutinee.tpe+") {\n  moreSpecific:"+moreSpecific+"\n  subsumed:"+subsumed+"\n  remaining"+remaining+"\n}"
    }

    /** returns casted symbol, success matrix and optionally fail matrix for type test on the top of this column */
    def getTransition(implicit theOwner: Symbol): (Symbol, Rep, Option[Rep]) = {
      //DEBUG("*** getTransition! of "+this.toString)
      // the following works for type tests... what fudge is necessary for value comparisons?
      // type test
      casted = if(scrutinee.tpe =:= patternType) scrutinee else newVar(scrutinee.pos, patternType)
      if(scrutinee.hasFlag(symtab.Flags.CAPTURED))
        casted.setFlag(symtab.Flags.CAPTURED)
      // succeeding => transition to translate(subsumed) (taking into account more specific)
      val nmatrix = {
        //Console.println("casted:"+casted)
        //Console.println("casted.case:"+casted.tpe.symbol.hasFlag(symtab.Flags.CASE))
        var ntemps   = if(casted.tpe.symbol.hasFlag(symtab.Flags.CASE)) casted.caseFieldAccessors map {
          meth =>
            val ctemp = newVar(scrutinee.pos, casted.tpe.memberType(meth).resultType)
            if(scrutinee.hasFlag(symtab.Flags.CAPTURED))
              ctemp.setFlag(symtab.Flags.CAPTURED)
            ctemp
        } else Nil // (***)
        var subtests = subsumed

        //Console.println("subtests BEFORE "+subtests)
        if(moreSpecific.exists { x => x != EmptyTree }) {
          ntemps   = casted::ntemps                                                                            // (***)
          subtests = moreSpecific.zip(subsumed) map { case (mspat, (j,pats)) => (j,mspat::pats) }
          //Console.println("MOS "+subtests)
        }
        ntemps = ntemps ::: rest.temp
        val ntriples = subtests map {
          case (j,pats) =>
            val (vs,_) = strip(column(j))
            val (opats,osubst,og,ob) = rest.row(j)
            val subst1 = vs map { v => (v,casted) }
          //Console.println("j = "+j)
          //Console.println("pats:"+pats)
          //Console.println("opats:"+pats)
          //debug

          // don't substitute eagerly here, problems with bodies that can
          //   be reached with several routes... impossible to find one-fits-all ordering.

            (pats ::: opats, osubst ::: subst1, og, ob)

          // BTW duplicating body/guard, gets you "defined twice" error cause hashing breaks
        }
        if(settings_debug) {
          Console.println("ntemps   = "+ntemps.mkString("[["," , ","]]"))
          Console.println("ntriples = "+ntriples.mkString("[["," , ","]]"))
        }
        Rep(ntemps, ntriples) /*setParent this*/
      }
      // fails      => transition to translate(remaining)

      val nmatrixFail: Option[Rep] = {
        val ntemps   = scrutinee :: rest.temp
        //Console.println("nmatrixfail ntemps:"+ntemps)
        val ntriples = remaining map {
          case (j, pat) => val r = rest.row(j);  (pat :: r._1, r._2, r._3, r._4)
        }
        //Console.println("nmatrixfail triples:"+ntriples)
        if(ntriples.isEmpty) None else Some(Rep(ntemps, ntriples) /*setParent this*/)
      }
      if(!nmatrixFail.isEmpty) {
        //DEBUG("nmatrix for failing type test "+patternType)
        //DEBUG(nmatrixFail.get.toString)
      } else {
        //DEBUG("pattern type "+patternType+" cannot fail for "+scrutinee)
      }
      (casted, nmatrix, nmatrixFail)
    } // end getTransitions
  }

  /** converts given rep to a tree - performs recursive call to translation in the process to get sub reps
   */
  def repToTree(rep:Rep, typed:Tree => Tree, handleOuter: Tree => Tree)(implicit theOwner: Symbol, failTree: Tree, bodies: collection.mutable.Map[Tree,(Tree, Tree, Symbol)]): Tree = {
    rep.applyRule match {
      case VariableRule(subst, EmptyTree, b) => bodies.get(b) match {

        case Some(EmptyTree, nb, theLabel) =>           //Console.println("H E L L O"+subst+" "+b)
          if(b.isInstanceOf[Literal])
            return b
          // recover the order of the idents that is expected for the labeldef
          val args = nb match { case Block(_, LabelDef(_, origs, _)) =>
            origs.map { p => Ident(subst.find { q => q._1 == p.symbol }.get._2) } // wrong!
          }   // using this instead would not work: subst.map { p => Ident(p._2) }
              // the order can be garbled, when default patterns are used... #bug 1163

          val body  = Apply(Ident(theLabel), args)
          return body

        case None    =>
          if(b.isInstanceOf[Literal]) {
            bodies(b) = (EmptyTree, b, null) // unreachable code is detected via missing hash entries
            return b
          }
          // this seems weird, but is necessary for sharing bodies. unnecessary for bodies that are not shared
          var argtpes   = subst map { case (v,_) => v.tpe }
          val theLabel  = targetLabel(theOwner, b.pos, "body"+b.hashCode, argtpes, b.tpe)
          // make new value parameter for each vsym in subst
          val vdefs     = targetParams(subst)

          var nbody: Tree = b
          val vrefs = vdefs.map { p:ValDef => Ident(p.symbol) }
          nbody  = squeezedBlock(vdefs:::List(Apply(Ident(theLabel), vrefs)), LabelDef(theLabel, subst.map(_._1), nbody))
          bodies(b) = (EmptyTree, nbody, theLabel)
          nbody
      }
      case VariableRule(subst,g,b) =>
        throw CantHandleGuard

      case ml: MixLiterals =>
        val (branches, defaultV, default) = ml.getTransition // tag body pairs
        if(settings_debug) {
          Console.println("[[mix literal transition: branches \n"+(branches.mkString("","\n","")))
          Console.println("defaults:"+defaultV)
          Console.println(default)
          Console.println("]]")
        }
        val cases = branches map { case (tag, rep) => CaseDef(Literal(tag), EmptyTree, repToTree(rep,typed,handleOuter)) }
        var ndefault = if(default.isEmpty) failTree else repToTree(default.get, typed, handleOuter)
        if(!defaultV.isEmpty) {
          var to:List[Symbol]  = Nil
          var i = 0; while(i < defaultV.length) { i = i + 1; to = ml.scrutinee::to }
          val tss = new TreeSymSubstituter(defaultV, to)
          tss.traverse( ndefault )
        }
        if(cases.length == 1) {
          val CaseDef(lit,_,body) = cases.head
          makeIf(Equals(Ident(ml.scrutinee),lit), body, ndefault)
        } else {
          val defCase = CaseDef(mk_(definitions.IntClass.tpe), EmptyTree, ndefault)
          Match(Ident(ml.scrutinee), cases :::  defCase :: Nil)
        }

      case mm:MixTypes =>
        val (casted,srep,frep) = mm.getTransition
        //DEBUG("--- mixture \n succ \n"+srep.toString+"\n fail\n"+frep.toString)
        //val cond = typed{gen.mkIsInstanceOf(Ident(mm.scrutinee), casted.tpe)}
        //Console.println("casted.tpe="+casted.tpe)
        val condUntyped = condition(casted.tpe, mm.scrutinee)
        //Console.println("condUntyped:" + condUntyped)
        var cond = handleOuter(typed { condUntyped })
        if(needsOuterTest(casted.tpe, mm.scrutinee.tpe)) // @todo merrge into def condition
          cond = addOuterCondition(cond, casted.tpe, typed{Ident(mm.scrutinee)}, handleOuter)

        val succ = repToTree(srep, typed, handleOuter)

        val fail = if(frep.isEmpty) failTree else repToTree(frep.get, typed, handleOuter)

        // dig out case field accessors that were buried in (***)
        val cfa  = casted.caseFieldAccessors
        //DEBUG("case field accessors, casted = "+casted.toString)
        //DEBUG("case field accessors, the things themselves = "+cfa.toString)
        val caseTemps = (if(!srep.temp.isEmpty && srep.temp.head == casted) srep.temp.tail else srep.temp).zip(cfa)

        //DEBUG("case temps"+caseTemps.toString)
        var vdefs     = caseTemps map {
          case (tmp,meth) =>
            val typedAccess = typed { Apply(Select(typed{Ident(casted)}, meth),List()) }
            typed { ValDef(tmp, typedAccess) }
        }

        vdefs = typed { ValDef(casted, gen.mkAsInstanceOf(typed{Ident(mm.scrutinee)}, casted.tpe))} :: vdefs
        typed { makeIf(cond, squeezedBlock(vdefs,succ), fail) }

      case mu: MixUnapply =>
        val (uacall,vdefs,srep,frep) = mu.getTransition
        //Console.println("getTransition"+(uacall,vdefs,srep,frep))
        val succ = repToTree(srep, typed, handleOuter)
        val fail = if(frep.isEmpty) failTree else repToTree(frep.get, typed, handleOuter)
        val cond = if(uacall.symbol.tpe =:= definitions.BooleanClass.tpe)
          Ident(uacall.symbol)
                   else
          typed { Not(Select(Ident(uacall.symbol),nme.isEmpty)) }
        typed { squeezedBlock(List(uacall), makeIf(cond,squeezedBlock(vdefs,succ),fail)) }
    }
  }

object Rep {
  type RepType = Product2[List[Symbol], List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)]]
  def unapply(x:Rep):Option[RepType] =
    if(x.isInstanceOf[RepImpl]) Some(x.asInstanceOf[RepImpl]) else None

  private
  case class RepImpl(val temp:List[Symbol], val row:List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)]) extends Rep with RepType {
    assert(row.forall { case (pats,subst,g,b) => temp.length == pats.length });
    def _1 = temp
    def _2 = row
  }

  /** the injection here handles alternatives and unapply type tests */
  def apply(temp:List[Symbol], row1:List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)]): Rep = {
    var unchanged: Boolean = true
    val row = row1 flatMap {
      xx =>
        def isAlternative(p: Tree): Boolean = p match {
          case Bind(_,p)       => isAlternative(p)
          case Alternative(ps) => true
          case _               => false
        }
        def getAlternativeBranches(p:Tree): List[Tree] = {
          def get_BIND(pctx:Tree => Tree, p:Tree):List[Tree] = p match {
            case b @ Bind(n,p)   => get_BIND({ x:Tree => pctx(copy.Bind(b, n, x) setType x.tpe) }, p)
            case Alternative(ps) => ps map pctx
          }
          get_BIND({x=>x}, p)
        }
        val (opatso,subst,g,b) = xx
        var opats = opatso
        var pats:List[Tree] = Nil
        var indexOfAlternative = -1
        var j = 0; while(opats ne Nil) {
          opats.head match {
            case p if isAlternative(p) && indexOfAlternative == -1 =>
              indexOfAlternative = j
              unchanged = false
              pats = p :: pats

            case typat @ Typed(p:UnApply,tpt) =>
              pats = (if (tpt.tpe <:< temp(j).tpe) p else typat)::pats

            case o @ Ident(n) =>
                if(n != nme.WILDCARD) {
                  //Console.println("/'''''''''''' 1"+o.tpe)
                  //Console.println("/'''''''''''' 2"+o.symbol)
                  //Console.println("/'''''''''''' 3"+o.symbol.tpe)
                  //Console.println("/'''''''''''' 4"+o.symbol.tpe.prefix)
                  //Console.println("/'''''''''''' 5"+o.symbol.tpe.prefix.isStable)

                  val stpe =
                    if(o.tpe.symbol.isModule)
                      singleType(o.tpe.prefix, o.symbol)
                    else
                      singleType(NoPrefix, o.symbol)

                  val p    = Ident(nme.WILDCARD) setType stpe
                  val q    = Typed(p, TypeTree(stpe)) setType stpe
                  pats = q::pats
                } else
                  pats = o::pats
            case ua @ UnApply(Apply(fn,_),arg)             => fn.tpe match {
              case MethodType(List(argtpe),_) =>
                pats = (if (temp(j).tpe <:< argtpe) ua else Typed(ua,TypeTree(argtpe)).setType(argtpe))::pats
              }
            case p =>
              pats = p :: pats
          }
          opats = opats.tail
          j = j + 1
        }
        pats = pats.reverse
        //i = pats findIndexOf isAlternative
        if(indexOfAlternative == -1)
          List((pats,subst,g,b))
        else {
          val prefix = pats.take( indexOfAlternative )
          val alts   = getAlternativeBranches(pats( indexOfAlternative ))
          val suffix = pats.drop(indexOfAlternative + 1)
          alts map { p => (prefix ::: p :: suffix, subst, g, b) }
        }
    }
    if(unchanged) {
      val ri = RepImpl(temp,row).init
      //Console.println("ri = "+ri)
      ri
    } else
      Rep(temp,row) // recursive call
  }
}

  abstract class Rep {
    val temp:List[Symbol]
    val row:List[(List[Tree], List[(Symbol,Symbol)], Tree, Tree)]

    var sealedCols = List[Int]()
    var sealedComb = List[Set[Symbol]]()
    //Console.println(" the matrix "+this.toString)

    def init: this.type = {
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
        case (i,syms)::cs   => for (s <- syms.toList; rest <- combine(cs)) yield (i,s) :: rest
      }

      if(!sealedCols.isEmpty) {
        //Console.println("cols"+sealedCols)
        //DEBUG("comb")
        //for (com <- sealedComb) //DEBUG(com.toString)

        val allcomb = combine(sealedCols zip sealedComb)
        //Console.println("all comb!" + allcomb)
        /** returns true if pattern vector pats covers a type symbols "combination"
         *  @param pats pattern vector
         *  @param comb pairs of (column index, type symbol)
         */
        def covers(pats: List[Tree], comb:List[(Int,Symbol)]) = {
          comb forall { case (i,sym) => val p = pats(i);
                       val symtpe = if(sym.hasFlag(symtab.Flags.MODULE)) {
                         singleType(sym.tpe.prefix, sym.linkedModuleOfClass) // e.g. None, Nil
                       } else sym.tpe
                       p.tpe.symbol == sym || symtpe <:< p.tpe }
        }
        val coversAll = allcomb forall { combination => row exists { r => covers(r._1, combination)}}
        //Console.println("all combinations covered? "+coversAll)
        if(!coversAll) {
          val sb = new StringBuilder()
          sb.append("match is not exhaustive!\n")
          for (open <- allcomb if !(row exists { r => covers(r._1, open)})) {
            sb.append("missing combination ")
            val NPAD = 15
            def pad(s:String) = { 1.until(NPAD - s.length).foreach { x => sb.append(" ") }; sb.append(s) }
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
      return this
    } // end init

    // if this was the *fail* branch, the Rep preceding this Rep
    //var mixtureParent: MixTypes = null

    //def setParent(mr:MixTypes): this.type = { mixtureParent = mr; this }

    def applyRule: RuleApplication = row match {
      case Nil            => ErrorRule
      case (pats,subst,g,b)::xs =>
        if(pats forall isDefaultPattern) {
          val subst1 = pats.zip(temp) flatMap {
            case (p,tmp) =>
              val (vs,_) = strip(p);
              vs.zipAll(Nil,null,tmp) // == vs map { (v,tmp) }
          }
          VariableRule (subst:::subst1, g, b)
        } else {
          val i = pats findIndexOf {x => !isDefaultPattern(x)}

          val column = row map { case (pats,subst,g,b) => pats(i) }

          val restTemp =                                     temp.take(i) ::: temp.drop(i+1)
          val restRows = row map { case (pats,subst,g,b) => (pats.take(i) ::: pats.drop(i+1),subst,g,b) }

          val r = MixtureRule(temp(i), column, Rep(restTemp,restRows))
          //Console.println(r)
          r
        }
    }
    // a fancy toString method for debugging
    override def toString = {
      val sb   = new StringBuilder
      val NPAD = 15
      def pad(s:String) = { 1.until(NPAD - s.length).foreach { x => sb.append(" ") }; sb.append(s) }
      for (tmp <- temp) pad(tmp.name.toString)
      sb.append('\n')
      for ((r,i) <- row.zipWithIndex) {
        for (c <- r._1 ::: List(r._2, r._3, r._4)) {
          pad(c.toString)
        }
        sb.append('\n')
      }
      sb.toString
    }
  }

  /** creates initial clause matrix
   */
  def initRep(selector:Tree, cases:List[Tree], checkExhaustive: Boolean)(implicit theOwner: Symbol) = {
    val root = newVar(selector.pos, selector.tpe)
    // communicate whether exhaustiveness-checking is enabled via some flag
    if(!checkExhaustive)
      root.setFlag(symtab.Flags.CAPTURED)
    val row  = cases map { case CaseDef(pat,g,b) => (List(pat),List(),g,b) }
    Rep(List(root), row)
  }

  /** this tree node is used several times in the parallel algo and will never be needed for matching, so it is reused */

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

  /** returns all variables that are binding the given pattern
   *  @param   x a pattern
   *  @return  vs variables bound, p pattern proper
   */
  def strip(x:Tree): (List[Symbol], Tree) = x match {
    case b @ Bind(_,pat) => val (vs,p) = strip(pat); (b.symbol :: vs,p)
    case z               => (Nil,z)
  }

  // ----------------------------------   functions used in internal data structure of the algorithm (matrix)


  // ----------------------------------   helper functions that generate symbols, trees for type tests, pattern tests
  // (shared by both algorithms, cough)

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
    if(settings_debug)
      Console.println("condition, tpe = "+tpe+", scrut.tpe = "+scrut.tpe+", res = "+res)
    res
  }
  def condition1(tpe: Type, scrut: Symbol): Tree = {
    assert (scrut ne NoSymbol)
    condition(tpe, Ident(scrut) . setType (scrut.tpe) . setSymbol (scrut))
  }

  def condition(tpe: Type, scrutineeTree: Tree): Tree = {
    assert(tpe ne NoType)
    assert(scrutineeTree.tpe ne NoType)
    //Console.println("tpe = "+tpe+" prefix="+tpe.prefix)
    //Console.println("singletontype?"+tpe.isInstanceOf[SingletonType])
    //Console.println("constanttype? "+tpe.isInstanceOf[ConstantType])
    //Console.println("value         "+tpe.symbol.isValue)
    //Console.println("module        "+tpe.symbol.isModule)
    if (tpe.isInstanceOf[SingletonType] && !tpe.isInstanceOf[ConstantType]) {

      if(tpe.symbol.isModule) {// object
        if (scrutineeTree.tpe <:< definitions.AnyRefClass.tpe)
          Eq(gen.mkAttributedRef(tpe.symbol), scrutineeTree)             // object
        else
          Equals(gen.mkAttributedRef(tpe.symbol), scrutineeTree)             // object
      } else {
        //Console.print("111 ??")
        //Console.println("tpe stable         "+tpe.isStable)
        //Console.println("tpe prefix stable  "+tpe.prefix.isStable)
        //val x = Equals(Apply(gen.mkAttributedRef(tpe.symbol), List()), scrutineeTree)
        val x =
          if(tpe.prefix ne NoPrefix) gen.mkIsInstanceOf(scrutineeTree, tpe)
          else Equals(gen.mkAttributedRef(tpe.symbol), scrutineeTree)
        //Console.println(" = "+x)
        typer.typed { x }
      }
    } else if (tpe.isInstanceOf[ConstantType]) {
      val value = tpe.asInstanceOf[ConstantType].value
      //if(false && value.isInstanceOf[NamedConstant])
      //  Equals(Ident(scrut), value.asInstanceOf[NamedConstant].tree)             // constant
      //assert(scrut.tpe <:< definitions.AnyRefClass.tpe, "stupid, should be caught by type checker "+value)
      //else
      if (value == Constant(null) && scrutineeTree.tpe <:< definitions.AnyRefClass.tpe)
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
        //DEBUG("DEBUG(outerAlwaysEqual) Some(f) for"+(left,right))
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
    if (outerAcc == NoSymbol) {
      if (settings_debug) cunit.warning(scrutinee.pos, "no outer acc for "+tpe2test.symbol)
      cond
    } else
      And(cond,
          Eq(Apply(Select(
            gen.mkAsInstanceOf(scrutinee, tpe2test, true), outerAcc),List()), theRef))

  }

}
