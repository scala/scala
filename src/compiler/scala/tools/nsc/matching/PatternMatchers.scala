/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import scala.tools.nsc.util.{Position, NoPosition}

/** This trait ...
 *
 *  @author Burak Emir
 *  @version 1.0
 */
trait PatternMatchers { self: transform.ExplicitOuter with PatternNodes with ParallelMatching =>
  import global._
  import typer.typed
  import symtab.Flags

  abstract class CantHandle // extends Exception
  object CantHandleSeq     extends CantHandle
  object CantHandleUnapply extends CantHandle
  object CantHandleApply   extends CantHandle
  object InternalError     extends CantHandle
  object CantHandleIdent   extends CantHandle
  object CantHandleGuard   extends CantHandle
  object CantOptimize      extends CantHandle
  //object CantHandleLiteral extends Exception

  var nPatterns = 0
  var nParallel = 0

  class PatternMatcher() {

    protected var optimize = true

    /** if this is false, will not check exhaustivity */
    protected var doCheckExhaustive = true

    /** the owner of the pattern matching expression
     */
    var owner:Symbol = _
    implicit def theOwner:Symbol = owner

    /** the selector expression
     */
    protected var selector: Tree = _

    /** the root of the pattern node structure
     */
    protected var root: PatternNode = _

    import global.{ definitions => defs }

    var handleOuter: Tree=>Tree = _

    def initialize(selector: Tree, doCheckExhaustive: Boolean, owner: Symbol, handleOuter:Tree=>Tree): Unit = {
      this.owner = owner
      this.doCheckExhaustive = doCheckExhaustive
      this.selector = selector
      this.handleOuter = handleOuter
      if (settings_debug) {
        Console.println("****")
        Console.println("**** initalize, selector = "+selector+" selector.tpe = "+selector.tpe)
        Console.println("****    doCheckExhaustive == "+doCheckExhaustive)
      }
      this.root = pConstrPat(selector.pos, selector.tpe.widen);
      this.root.and = pHeader(selector.pos,
                              selector.tpe.widen,
                              Ident(root.casted).setType(root.tpe));
      //Konsole.println("resultType =  "+resultType);
      //this.optimize = this.optimize && (settings.target.value == "jvm");
    }

    // factories

    def pSequencePat(pos: Position, tpe: Type, len: int) = new SequencePat(newVar(NoPosition, tpe), len) set ((pos,tpe))

    def pRightIgnoringSequencePat(pos: Position, tpe: Type, castedRest: Symbol, minlen: int) =
      new RightIgnoringSequencePat(newVar(NoPosition, tpe), castedRest, minlen) set ((pos,tpe))

    def pDefaultPat(pos: Position, tpe: Type) = new DefaultPat() set ((pos,tpe))

    def pConstrPat(pos: Position, tpe: Type) = new ConstrPat(newVar(pos, tpe)) set ((pos,tpe))

    def pUnapplyPat(pos: Position, fn: Tree) = {
      var tpe = defs.unapplyUnwrap(fn.tpe)
      if(defs.isOptionOrSomeType(tpe)) { tpe = tpe.typeArgs.head }
      new UnapplyPat(newVar(pos, tpe), fn) set ((pos,tpe))
    }

    def pConstantPat(pos: Position, tpe: Type, value: Any) = new ConstantPat(value) set ((pos,tpe))

    def pVariablePat(pos: Position, tree: Tree) = new VariablePat(tree) set ((pos,tree.tpe))

    def pAltPat(pos: Position, header: Header) = new AltPat(header) set ((pos, header.tpe))

    def pHeader(pos: Position, tpe: Type, selector: Tree) = new Header(selector, null) set ((pos,tpe))

    def pBody(pos: Position) = {
      val node = new Body(new Array[Array[ValDef]](0), new Array[Tree](0),new Array[Tree](0))
      node.pos = pos
      node
    }

    def pBody(pos: Position, bound: Array[ValDef], guard: Tree, body: Tree) = {
      val node = new Body(Array(bound), Array(guard), Array(body))
      node.pos = pos
      node
    }

    /** pretty printer */
    def print(): Unit = Console.println(root.and.print("", new StringBuilder()).toString())

    // states ---
    // states have constructors which implicitly get added to some set of states in scope

    // --- states

    var dfatree: Tree = null

    /** enters a sequence of cases into the pattern matcher */
    def construct(cases: List[Tree]): Unit = {
      nPatterns = nPatterns + 1

      if (settings_debug) {
        Console.println(cases.mkString("construct{{{\n","\n","\n}}}"))
      }

      if(settings_useParallel) {
        constructParallel(cases) match {
          case null => // ignore
            return

          case a:AssertionError =>
            cunit.error(cases.head.pos, "assertion failed while compiling this case")
            a.printStackTrace()
            throw FatalError("died in parallel match algorithm" )

          case _:AbstractMethodError =>
            cunit.error(cases.head.pos, "please recompile matcher component (explicitouter,patternmattcher, parallelmatching,codefactory)")
            throw FatalError("died in parallel match algorithm" )

          case n:NullPointerException =>
            cunit.error(cases.head.pos, "internal error (null pointer exception)")
            n.printStackTrace()
            throw FatalError("died in parallel match algorithm" )

          case _:OutOfMemoryError =>
            cunit.error(cases.head.pos, "internal error (out of memory in parallel match algorithm)")
            throw FatalError("died in parallel match algorithm" )

          case e:MatchError =>
            cunit.error(cases.head.pos, "internal error (match error in parallel match algorithm)"+e.getMessage)
          e.printStackTrace()
            throw FatalError("died in parallel match algorithm" )

          case e =>
            if(!settings_useIncr)
              throw new RuntimeException(e.toString)
            if (settings_debug) {
              Console.println("****")
              Console.println("**** falling back, cause " + e.toString )
              Console.println("****")
              for (CaseDef(pat,guard,_) <- cases)
                Console.println(pat.toString)
            }
        }
      }

      constructIncremental(cases)
    }

    def constructParallel(cases: List[Tree]): Any = {
      var cases1 = cases; while(cases1 ne Nil) {
        val c = cases1.head.asInstanceOf[CaseDef]
        //if(c.guard != EmptyTree) return CantHandleGuard // TEST
        //hasUnapply.traverse(c.pat)
        val e = isImplemented(c.pat); if(e ne null) return e
        cases1 = cases1.tail
      }

      val irep = initRep(selector, cases, doCheckExhaustive)
      val root = irep.temp.head

      implicit val fail: Tree = ThrowMatchError(selector.pos, Ident(root))
      val vdef = typed{ValDef(root, selector)}

      //implicit val memo       = new collection.mutable.HashMap[(Symbol,Symbol),Symbol]
      //implicit val theCastMap = new collection.mutable.HashMap[(Symbol,Type),Symbol]

      try {
        val mch  = typed{repToTree(irep, handleOuter)}
        dfatree = typed{squeezedBlock(List(vdef), mch)}

        //DEBUG("**** finished\n"+dfatree.toString)
        var bx = 0; var cs = cases; while(cs ne Nil) {
          if(!Rep.isReached(bx)) {
            cunit.error(cs.head.asInstanceOf[CaseDef].body.pos, "unreachable code")
          }
          cs = cs.tail
          bx += 1
        }
        dfatree = Rep.cleanup(dfatree)
        resetTrav.traverse(dfatree)

        //constructParallel(cases) // ZZZ
        nParallel += 1
        return null
      } catch {
        //case e =>
          /*
          Console.println("!!!unit: "+cunit)
          Console.println("!!!selector.pos: "+selector.pos)
          cunit.warning(selector.pos, "going gaga here")
          Console.println("!!!problem: "+e.getMessage)
          */
          //Rep.cleanup()
	//	 return e // fallback

         //non-fallback:
        case e: CantHandle => Rep.cleanup(); return e
        case e => throw e
      }
    }

    /** constructs match-translation incrementally */
    private def constructIncremental(cases:List[Tree]) {
      doCheckExhaustive = false
      cases foreach enter
    }

    object resetTrav extends Traverser {
      override def traverse(x:Tree): unit = x match {
        case vd @ ValDef(_,_,_,_)=>
          vd.symbol.resetFlag(symtab.Flags.CAPTURED)
          if(vd.symbol.hasFlag(symtab.Flags.TRANS_FLAG)) {
            vd.symbol.resetFlag(symtab.Flags.TRANS_FLAG)
            vd.symbol.resetFlag(symtab.Flags.MUTABLE)
          }
        case _ =>
          super.traverse(x)
      }
    }

    def isImplemented(xs:List[Tree]): CantHandle =
      if(xs eq Nil) null else {
        val e = isImplemented(xs.head)
        if(e ne null) e else isImplemented(xs.tail)
      }

    def isImplemented(x:Tree): CantHandle = {
      //Console.println("isImplemented? "+x)
      x match {
        case app @ Apply(fn,xs) =>
          if(!isCaseClass(app.tpe)) { /*|| (fn.symbol ne null)*/
             //Console.println(app)
             //Console.println(app.tpe)
             //Console.println(app.symbol)
             //Console.println(fn.symbol)
             if(!xs.isEmpty) {
               assert(false)
               return CantHandleApply // System.exit(-1); // this should never happen
             }
             null
          } else {
            /*if(!app.tpe.symbol.hasFlag(symtab.Flags.CASE)) {
              Console.print("what is this?"+x)
              if(fn.symbol eq null) {
                Console.println("it's fn doesn't even have a symbol?!")
              } else {
                Console.println("it's fn symbol is "+fn.symbol)
              }
            }*/
            isImplemented(xs)
          }
        case p @ Ident(n)       => null // if(n!= nme.WILDCARD && p.symbol.) CantHandleIdent else null
        case UnApply(fn,xs)     => isImplemented(xs)
        case Bind(n, p)         => isImplemented(p)
        case Alternative(xs)    => isImplemented(xs)
        case p:Literal          => null
        case p:Select           => null
        case p:Typed            => null


        case ArrayValue(_,xs)   => CantHandleSeq

        //@todo
        //case ArrayValue(_,xs)   => isImplemented(xs) //CantHandleSeq // DEBUG
        //case Star(t)            => isImplemented(t) //can't happen/ excluded by Transmatcher:isregular
        //case Sequence(trees) =>can't happen/ only appear below ArrayValue
      }
    }

    /*
    object hasUnapply extends Traverser {
      override def traverse(x:Tree) = {
        //Console.println("pat:'"+x+"' / "+x.getClass)
        x match {
          case _:ArrayValue => throw CantHandleSeq
          case UnApply(fn,args)    => //throw CantHandleUnapply // EXPR
            traverseTrees(args)
          case Ident(n) if n!= nme.WILDCARD =>
            //DEBUG("I can't handle IDENT pattern:"+x)
            //DEBUG("x.tpe.symbols:"+x.tpe.symbol)
            throw CantHandleIdent
          case p:Select =>
            //case p:Select =>
            //  //DEBUG("I can't handle SELECT pattern:"+p)
            //  //DEBUG("p.tpe.symbols:"+p.tpe.symbol)
            //throw CantHandleUnapply
            case p@Apply(fn,args) if !p.tpe.symbol.hasFlag(symtab.Flags.CASE) =>
              //Console.println("I can't handle APPLY pattern:"+p)
              //Console.println("fn:"+fn)
              //Console.println("args:"+args)
              //Console.println("p.tpe.symbols:"+p.tpe.symbol)
              throw CantHandleApply

          //case p@Apply(_,_) if !p.tpe.symbol.hasFlag(symtab.Flags.CASE) => throw CantHandleUnapply //@todo
          case _ => super.traverse(x)
        }
      }
      }
*/

    /** enter a single case into the pattern matcher */
    protected def enter(caseDef: Tree): Unit = caseDef match {
      case CaseDef(pat, guard, body) =>
        val env = new CaseEnv
        val target = enter1(pat, -1, root, root.casted, env)
        // if (target.and != null)
        //   unit.error(pat.pos, "duplicate case");
      if (null == target.and)
        target.and = pBody(caseDef.pos, env.getBoundVars(), guard, body)
      else if (target.and.isInstanceOf[Body])
        updateBody(target.and.asInstanceOf[Body], env.getBoundVars(), guard, body)
      else target.and match {
        case _h: Header =>
          val h = _h.findLast;

          // target.and is a header
          //
          //print()
          //cunit.error(pat.pos, "duplicate case")
          h.or = pDefaultPat(caseDef.pos, target.tpe)
          h.or.and = pBody(caseDef.pos, env.getBoundVars(), guard, body)
          //print()
          //Console.println("tao = "+target.and.or)
          //Console.println("tao = "+target.and.or.or)
        case _ =>
          Predef.error("overlapping match at unit = " + cunit + "; cdef = " + caseDef)

      }
    }

    protected def updateBody(tree: Body, bound: Array[ValDef],
                             guard: Tree, body: Tree): Unit =
      if (tree.guard(tree.guard.length - 1) == EmptyTree) {
        cunit.error(body.pos, "B unreachable code")
      } else {
        val bd = new Array[Array[ValDef]](tree.bound.length + 1)
        val ng = new Array[Tree](tree.guard.length + 1)
        val nb = new Array[Tree](tree.body.length + 1)
        Array.copy(tree.bound, 0, bd, 0, tree.bound.length)
        Array.copy(tree.guard, 0, ng, 0, tree.guard.length)
        Array.copy(tree.body, 0, nb, 0, tree.body.length)
        bd(bd.length - 1) = bound
        ng(ng.length - 1) = guard
        nb(nb.length - 1) = body
        tree.bound = bd
        tree.guard = ng
        tree.body = nb
      }

  protected def patternNode(tree:Tree , header:Header , env: CaseEnv ): PatternNode  = {
    //Console.println("patternNode("+tree+","+header+")");
        //else scala.Predef.error("got null tree in patternNode");
    //Console.println("tree.tpe "+tree.tpe);
    //Console.println("tree.getClass() "+tree.getClass());
    val t = tree match {
      case Bind(name, Typed(Ident(nme.WILDCARD), tpe)) => // x@_:Type
        // @note: safest! typer does not always use tpe.tpe, e.g. in the case of refinement type tests
        val tpe2test = tree.symbol.tpe
        // @note: if (isSubType(header.tpe, tpe2test)) then this will be translated to isNull test
        val node = pConstrPat(tree.pos, tpe2test)
        env.newBoundVar(tree.symbol, tpe2test, typed(Ident(node.casted)));
        node

      case Bind(name, Ident(nme.WILDCARD)) => // x @ _
        val node = pDefaultPat(tree.pos, header.tpe)
        if ((env ne null) && (tree.symbol != defs.PatternWildcard))
          env.newBoundVar( tree.symbol, tree.tpe, header.selector);
        node

      case Bind(name, pat) =>                // x @ p
        val node = patternNode(pat, header, env)
        if ((env ne null) && (tree.symbol != defs.PatternWildcard)) {
          val theValue = node.symbol2bind match {
            case NoSymbol => header.selector
            case x        => Ident(x) setType x.tpe
          }
          env.newBoundVar(tree.symbol, tree.tpe, theValue)
        }
       node

      case t @ UnApply(fn, args)  =>
        pUnapplyPat(tree.pos, fn)

      case t @ Apply(fn, args) =>             // pattern with args
        //Console.println("Apply node: "+t);
        //Console.println("isSeqApply "+isSeqApply(t)); // todo:do seq applies still exist?
        if (isSeqApply(t)) {
          args(0) match {
            //  case Sequence(ts)=>
              case av @ ArrayValue(_, ts)=>
                if(isRightIgnoring(av)) {
                  //Console.println(av.toString()+" IS RIGHTIGNORING");
                  val castedRest = ts.last match {
                    case b:Bind => b.symbol
                    case _      => null
                  }
                  pRightIgnoringSequencePat(tree.pos, tree.tpe, castedRest, ts.length-1);
                } else
                  //Console.println(av.toString()+" IS  N O T  RIGHTIGNORING");
                  pSequencePat(tree.pos, tree.tpe, ts.length);
          }
        } else if ((fn.symbol ne null) &&
                   fn.symbol.isStable &&
                   !(fn.symbol.isModule &&
                     ((fn.symbol.flags & Flags.CASE) != 0))) {
                       pVariablePat(tree.pos, tree);
                     }
          else {
             /*
            Console.println("apply but not seqApply");
            Console.println("tree.tpe="+tree.tpe);
            Console.println("tree.symbol="+tree.symbol);
             */
             pConstrPat(tree.pos, tree.tpe);
          }
      case Typed(Ident( nme.WILDCARD ), tpe) => // x@_:Type
        val doTest = isSubType(header.tpe, tpe.tpe) // this is already an optimization
        if (doTest) pDefaultPat(tree.pos, tpe.tpe)
        else pConstrPat(tree.pos, tpe.tpe)

      case t @ Typed(ident, tpe) =>       // variable pattern
        // can't optimize using isSubType(header.tpe,tpe.tpe);
        //  leave that (null check!) for later
        val node =
          pConstrPat(tree.pos, tpe.tpe)
        if (null != env) node match {
            case ConstrPat(casted) =>
              env.newBoundVar(t.expr.symbol,
                              tpe.tpe,
                              Ident( casted ).setType(casted.tpe));
          }
        node

      case Ident(nme.WILDCARD) => pDefaultPat(tree.pos, header.tpe)

      case Ident(name) => // pattern without args or named constant
        assert(!tree.symbol.isPrimaryConstructor) // may not happen
        pVariablePat(tree.pos, tree); // named constant (capitalized variable Foo)

      case Select(_, name) => // named constant
        if (tree.symbol.isPrimaryConstructor)
          pConstrPat(tree.pos, tree.tpe)
        else
          pVariablePat(tree.pos, tree)

      case Literal(Constant(value)) =>
        pConstantPat(tree.pos, tree.tpe, value)

      case av @ ArrayValue(_, ts) =>
        if(isRightIgnoring(av)) {
          val castedRest = ts.last match {
            case b:Bind => b.symbol
            case _      => null
          }
          //Console.println("array value "+av+" is right ignoring!")
          pRightIgnoringSequencePat(tree.pos, tree.tpe, castedRest, ts.length-1);
        } else {
          //Console.println("array value "+av+" is not considered right ignoring")
          //lastSequencePat =
          pSequencePat(tree.pos, tree.tpe, ts.length);
          //lastSequencePat

        }
      case Alternative(ts) =>
        if (ts.length < 2)
          Predef.error("ill-formed Alternative")
        val subroot = pConstrPat(header.pos, header.tpe)
        subroot.and = { val h = pHeader(header.pos, header.tpe, header.selector.duplicate); h.isSubHeader = true; h }
        val subenv = new CaseEnv
        var i = 0; while (i < ts.length) {
          val target = enter1(ts(i), -1, subroot, subroot.casted, subenv)
          target.and = pBody(tree.pos)
          i += 1
        }
        pAltPat(tree.pos, subroot.and.asInstanceOf[Header])
/*
      case Star(Ident(nme.WILDCARD))  =>
        header.selector match {
          case Apply(Select(t, apply), arg @ List(litconst)) =>
            Console.println(t)
            Console.println(apply)
            Console.println(litconst)
            val tree = Apply(Select(Select(t, "toList"), "drop"), arg)
            throw new OptimizeSequencePattern(tree); // ? has to be caught and rethrown by each bind
        }
        // bind the rest /////////////////////////////////////////////////////
*/
      case _ =>
        Predef.error("unit = " + cunit + "; tree = " +
          (if (tree eq null) "null" else tree))
    }
    //Console.print(t);
    t
  }

  protected def enter(pat: Tree, index: Int, target: PatternNode,
                      casted: Symbol, env: CaseEnv): PatternNode =
    target match {
      case ConstrPat(newCasted) =>
        enter1(pat, index, target, newCasted, env)
      case SequencePat(newCasted, len) =>
        enter1(pat, index, target, newCasted, env)
      case _ =>
        enter1(pat, index, target, casted, env)
    }

  private def newHeader(pos: Position, casted: Symbol, index: Int): Header = {
    //Console.println("newHeader(pos,"+casted+" (has CASE flag? "+casted.tpe.symbol.hasFlag(Flags.CASE)+") of type "+casted.tpe+" with pos "+casted.pos+"(equals FIRSTPOS? "+(casted.pos == Position.FIRSTPOS)+"),"+index+")");
    val ident = typed(Ident(casted))
    if (casted.pos == NoPosition) { // load the result of casted(i)
      //Console.println("FIRSTPOS");
      val t = typed(
        Apply(Select(ident, ident.tpe.member(nme.apply)),
              List(Literal(Constant(index)))))
      val seqType = t.tpe
      pHeader( pos, seqType, t )
    } else {
      //Console.println("NOT FIRSTPOS");
      //Console.println("newHeader :: ");
      if (!isCaseClass(casted.tpe)) {

        //Console.println("NOT CASE");
        //Console.println("getProductArgs? "+defs.getProductArgs(casted.tpe));
        defs.getProductArgs(casted.tpe) match  {
          case Some(targs) =>
            val accSym = defs.productProj(casted.tpe.typeSymbol, index+1)
            val accTree = typed(Apply(Select(ident, accSym), List())) // nsc !
            return pHeader(pos, accTree.tpe, accTree)
          case None =>
        }

        /*
	Console.println("  newHeader :: casted="+casted);
	Console.println("  newHeader :: casted.pos="+casted.pos);
	Console.println("  newHeader :: casted.pos==Position.FIRSTPOS"+(casted.pos == Position.FIRSTPOS));
	Console.println("  newHeader :: casted.tpe="+casted.tpe);
      	Console.println("  newHeader :: casted.tpe.symbol="+casted.tpe.symbol);
print()
*/
	throw new Error("internal problem, trying casefield access for no case class") //DBG
      }

      //Console.println("CASE");

      val caseAccs = casted.tpe.typeSymbol.caseFieldAccessors;
      if (caseAccs.length <= index) Console.println("selecting " + index + " in case fields of " + casted.tpe.typeSymbol + "=" + caseAccs);//debug
      val ts = caseAccs(index);
      val accTree = typed(Apply(Select(ident, ts), List()))
      val accType = accTree.tpe
      accType match {
        // scala case accessor
        case MethodType(_, _) =>
          //Console.println("Hello?!");
          pHeader(pos, accType.resultType, Apply(accTree, List()))
        // jaco case accessor
        case _ =>
          //Console.println("Hola?!");
          pHeader(pos, accType, accTree)
      }
    }
  }

  /** main enter function
   *
   *  invariant: ( curHeader == (Header)target.and ) holds
   */
  protected def enter1(pat: Tree, index: Int, target: PatternNode, casted: Symbol, env: CaseEnv): PatternNode = {

                         // special case List()
    pat match {
      case UnApply(fn, List(ArrayValue(zs, List()))) if isSameType(defs.ListModule.tpe, fn.symbol.owner.tpe) =>
        return enter1(posAssigner.atPos(pat.pos){gen.mkAttributedRef(defs.NilModule)}, index, target, casted, env)
        /*
      case UnApply(fn, List(ArrayValue(zs, pats))) if isSameType(defs.ListModule.tpe, fn.symbol.owner.tpe) =>
        Console.println("special case List"+pats)
        Console.println("special case zs = "+zs)
        def makeConsPat(p:Tree) = {
          val constpe = typeRef(defs.ConsClass.tpe.prefix, defs.ConsClass, List(zs.tpe))
          val patNode = pConstrPat(pat.pos, )
          patNode.and = newHeader(pat.pos, )
        }
      null
      */
      case _ =>
    }

    //Console.println("enter(" + pat + ", " + index + ", " + target + ", " + casted + ")");
    var bodycond: PatternNode => Body = null // in case we run into a body (combination of typed pattern and constructor pattern, see bug#644)
    val patArgs = patternArgs(pat);      // get pattern arguments
    //System.err.println("patArgs = "+patArgs);
    var curHeader: Header = target.and match {
      case null => null
      case h: Header => h    // advance one step in intermediate representation
      case b: Body if (b.or ne null) => b.or.asInstanceOf[Header]
      case b: Body =>
        if (b.guard(b.guard.length - 1) == EmptyTree) {
          cunit.error(pat.pos, "unreachable code")
          null
        }
        else {
          bodycond = {h => b.or = h; b} // depends on the side-effect to curHeader (*)
          null
        }
      case _ => Predef.error("cannot happen");
    }
    if (curHeader eq null) {                  // check if we have to add a new header
      //Console.println(" -- adding new header")
      //assert index >= 0 : casted;
      if (index < 0) { Predef.error("error entering:" + casted); return null }

      target match {
        case u @ UnapplyPat(_,_) if u.returnsOne =>
          //Console.println("u.returnsOne!"+u+ " casted:"+casted+" u.casted"+u.casted)
          assert(index==0)
          curHeader = pHeader(pat.pos, casted.tpe, Ident(casted) setType casted.tpe)
          target.and = curHeader
          curHeader.or = patternNode(pat, curHeader, env)
          enter(patArgs, curHeader.or, casted, env)
        case _ =>
          //access the index'th child of a case class
          curHeader  = newHeader(pat.pos, casted, index)
          target.and = curHeader; // (*)

          //Console.println("curHeader : "+curHeader)
          if (bodycond ne null) target.and = bodycond(target.and) // restores body with the guards

          curHeader.or = patternNode(pat, curHeader, env)
          enter(patArgs, curHeader.or, casted, env)
      }
    } else {
      //Console.println("   enter: using old header for casted = "+casted) // DBG
      // find most recent header
      curHeader = curHeader.findLastSection
      // create node
      var patNode = patternNode(pat, curHeader, env)
      var next: PatternNode = curHeader
      // add branch to curHeader, but reuse tests if possible
      while (true) {
        if (next.isSameAs(patNode)) {           // test for patNode already present --> reuse
          //Console.println(" -- found the same!")
          // substitute... !!!
          patNode.casted match {
            case NoSymbol => ;
            case ocasted =>
              env.substitute(ocasted, typed(Ident(next.casted)));
          }
          return enter(patArgs, next, casted, env);
        } else if (next.isDefaultPat() ||           // default case reached, or
                   ((next.or eq null) &&            //  no more alternatives and
                    (patNode.isDefaultPat() || next.subsumes(patNode)))) {
                      // new node is default or subsumed
                      var header = pHeader(patNode.pos,
                                           curHeader.tpe,
                                           curHeader.selector);
                      {curHeader.next = header; header};
                      header.or = patNode;
                      return enter(patArgs,
                                   patNode,
                                   casted,
                                   env);
                    }
          else if (next.or eq null) {
            //Console.println(" -- add new branch!")
            return enter(patArgs, {next.or = patNode; patNode}, casted, env); // add new branch
          } else
            next = next.or;
      }
      error("must not happen")
      null
    }
  }

    /** calls enter for an array of patterns, see enter
     *
     *  @param pats    ...
     *  @param target1 ...
     *  @param casted1 ...
     *  @param env     ...
     *  @return        ...
     */
    protected def enter(pats: List[Tree], target1: PatternNode,
                        casted1: Symbol, env: CaseEnv): PatternNode = {
      var target = target1
      var casted = casted1
      target match {
        case UnapplyPat(newCasted, fn) =>
          casted = newCasted
        case ConstrPat(newCasted) =>
          casted = newCasted
        case SequencePat(newCasted, len) =>
          casted = newCasted
        case RightIgnoringSequencePat(newCasted, _, len) =>
          casted = newCasted
        case _ =>
      }
      var i = 0; while (i < pats.length) {
        target = enter1(pats(i), i, target, casted, env)
        i += 1
      }
      target
    }

    protected def nCaseComponents(tree: Tree): int = {
      tree match {
        case Apply(fn, _) =>
          val tpe = tree.tpe.typeSymbol.primaryConstructor.tpe
          //Console.println("~~~ " + tree.type() + ", " + tree.type().symbol.primaryConstructor());
          tpe match {
            // I'm not sure if this is a good idea, but obviously, currently all case classes
            // without constructor arguments have type NoType
            case NoType =>
              error("this cannot happen")
              0
            case MethodType(args, _) =>
              args.length
            case PolyType(tvars, MethodType(args, _)) =>
              args.length
            case PolyType(tvars, _) =>
              0
            case _ =>
              error("not yet implemented;" +
                    "pattern matching for " + tree + ": " + tpe);
            }
        }
      0
    }


    //////////// generator methods

    def toTree(): Tree = {
      if(dfatree != null) {
        return dfatree
      }
     val t = if (isSimpleIntSwitch())
        intSwitchToTree()
      // else if (false && optimize && isSimpleSwitch())
      //  switchToTree()
      else {
        //print()
        generalSwitchToTree()
      }
      //if(settings.Xpatternstatistics.value) {
      //  Console.print("deleted "+nremoved+" temps,")
      //  Console.print("substituted "+nsubstituted+" temps")
      //  Console.println("and optimized "+(nstatic)+" instance-tests to ne-null")
      //}
      return t
    }
    case class Break(res:Boolean) extends java.lang.Throwable
    case class Break2() extends java.lang.Throwable

    /*
    // TODO disentangle this
    protected def isSimpleSwitch(): Boolean  = {
      print();
      var patNode = root.and;
      while (patNode ne null) {
        var node = patNode;
        while (({node = node.or; node}) ne null) {
          node match {
                case VariablePat(tree) =>
                  //Konsole.println(((tree.symbol.flags & Flags.CASE) != 0));
                case ConstrPat(_) =>
                  //Konsole.println(node.tpe.toString() + " / " + ((node.tpe.symbol.flags & Flags.CASE) != 0));
                    var inner = node.and;
                    def funct(inner: PatternNode): Boolean = {
                      //outer: while (true)
                      inner match {
                        case _h:Header =>
                          if (_h.next ne null)
                            throw Break(false);
                          funct(inner.or)

                        case DefaultPat() =>
                          funct(inner.and)

                        case b:Body =>
                          if ((b.guard.length > 1) ||
                              (b.guard(0) != EmptyTree))
                            throw Break(false);

                          throw Break2() // break outer
                        case _ =>
                          throw Break(false)
                      }
                    }
                    var res = false
                    var set = false
                    try {
                      funct(inner)
                    } catch {
                      case ex: Break =>
                        res = ex.res
                        set = true
                      case ex: Break2 =>
                    }
                    if(set) return res;
            case _ =>
              return false;
          }
        }
        patNode = patNode.nextH()
      }
      true
    }
*/
  protected def isSimpleIntSwitch(): Boolean =
    (isSameType(selector.tpe.widen, defs.IntClass.tpe)) && {
      root.and.asInstanceOf[Header].forEachSection {
        case patNode:Header =>
          patNode.forEachBranch {
            case n @ ConstantPat(_) if(n.and.isSingleUnguardedBody) => ;
            case n @ DefaultPat()   if(n.and.isSingleUnguardedBody) => ;
            case z              => return false;
          }}
      true
    }

  class TagBodyPair(tag1: Int, body1: Tree, next1: TagBodyPair ) {
    var tag: int = tag1
    var body: Tree = body1
    var next: TagBodyPair = next1
  }

  protected def defaultBody(patNode1: PatternNode, otherwise: Tree ): Tree = {
    patNode1.asInstanceOf[Header].forEachSection {
      case h:Header => h.forEachBranch {
        case n @ DefaultPat() => return n.and.bodyToTree();
        case _ =>
      }
    }
    otherwise
  }

  /** This method translates pattern matching expressions that match
   *  on integers on the top level.
   */
  private def intSwitchToTree(): Tree = {
    def insert1(tag: Int, body: Tree, current: TagBodyPair): TagBodyPair = {
      if (current eq null)
        new TagBodyPair(tag, body, null)
      else if (tag > current.tag)
        new TagBodyPair(current.tag, current.body, insert1(tag, body, current.next))
      else
        new TagBodyPair(tag, body, current)
    }

    //print();
    var ncases = 0
    root.and.asInstanceOf[Header].forEachBranch {
      case DefaultPat() => ;
      case _ => ncases = ncases + 1
    }

    val matchError = ThrowMatchError(selector.pos, Ident(root.casted))
    // without a case, we return a match error if there is no default case
    if (ncases == 0)
      return defaultBody(root.and, matchError);
    // for one case we use a normal if-then-else instruction
    else if (ncases == 1) {
      root.and.or match {
        case ConstantPat(value) =>
          return squeezedBlock(
            List(typedValDef(root.casted, selector)),
            If(Equals(Ident(root.casted), Literal(value)),
               (root.and.or.and).bodyToTree(),
               defaultBody(root.and, matchError))
          )
        case _ =>
          return generalSwitchToTree();
      }
    }
    //
    // if we have more than 2 cases than use a switch statement
    val _h:Header = root.and.asInstanceOf[Header]

    val next = _h.next
    var mappings: TagBodyPair = null
    var defaultBody1: Tree = matchError
    var patNode = root.and
    while (patNode ne null) {
      var node = patNode.or
      // we have checked beforehand that certain pattern nodes do not appear
      while (node ne null) {
        node match {
          case DefaultPat() =>
            //experimental: allow default body
            //if (defaultBody1 != null)
            //  scala.Predef.error("not your day today");
            defaultBody1 = node.and.bodyToTree();
            node = node.or;

          case ConstantPat( value: Int )=>
            mappings = insert1(
              value,
              node.and.bodyToTree(),
              mappings);
          node = node.or;

          case ConstantPat( value: Char )=>
            mappings = insert1(value.toInt,node.and.bodyToTree(),mappings);
          node = node.or;

/*
          case ConstantPat( value: Byte )=>
            mappings = insert1(value.toInt,node.and.bodyToTree(),mappings);
          node = node.or;

          case ConstantPat( value: Short )=>
            mappings = insert1(value.toInt,node.and.bodyToTree(),mappings);
          node = node.or;
*/ // these correct?
          case _ =>
            Predef.error("cannot happen")
        }
      }
      patNode = patNode.nextH()
    }

    var nCases: List[CaseDef] = Nil
    while (mappings ne null) {
      nCases = CaseDef(Literal(mappings.tag),
                       mappings.body) :: nCases;
      mappings = mappings.next;
    }

    // ERASURE-PROBLEM this was how it was before, perfectly equivalent code
    // nCases = CaseDef(Ident(nme.WILDCARD),Block(List(ValDef(root.casted, selector.duplicate)),defaultBody1)) :: nCases;
    // Match(selector, nCases)

    // changed to
    nCases = CaseDef(Ident(nme.WILDCARD),defaultBody1) :: nCases;
    squeezedBlock(List(typedValDef(root.casted, selector)),Match(Ident(root.casted), nCases))
  }



    var exit: Symbol = null
    /** simple optimization: if the last pattern is `case _' (no guards), we won't generate the ThrowMatchError
     */
  private def generalSwitchToTree(): Tree = {
    this.exit = owner.newLabel(root.pos, "exit").setInfo(new MethodType(List(resultType), resultType));
    val result = exit.newValueParameter(root.pos, "result").setInfo( resultType );
    squeezedBlock(
      List(
        typedValDef(root.casted, selector),
        typed { toTree(root.and) },
        ThrowMatchError(selector.pos,  Ident(root.casted))) ,
      LabelDef(exit, List(result), Ident(result)))
  }


  /*protected*/ def toTree(node1: PatternNode): Tree = {

    var node = node1;

    var res: Tree = Literal(Constant(false)); //.setInfo(defs.BooleanClass);
    var lastSelector: Tree = null
    var carryCovered: SymSet = emptySymbolSet
    while (node ne null)
    node match {

      case _h:Header =>
        //Console.println(_h.print("!!!! visit ", new StringBuilder()).toString())

        val selector = _h.selector;
        if(selector != lastSelector) {
          carryCovered = emptySymbolSet;
        }
        //Console.println(" catches all?"+_h.catchesAll)
      //Console.println("sel:"+selector+" last"+lastSelector+" - "+(selector == lastSelector))
        val next = _h.next;
        //res = And(mkNegate(res), toTree(node.or, selector));
        val (doOptimize, coveredCases, remainingCases) = _h.optimize1()
        if (doOptimize)
          res = Or(res, toOptTree(node.or, selector));
        else
          res = Or(res, toTree(node.or, selector));

      //Console.println("!carry covered "+ carryCovered)

        lastSelector = selector
        node = next;

      case _b:Body =>
        var bound = _b.bound
        val guard = _b.guard
        val body  = _b.body
        if ((bound.length == 0) &&
            (guard.length == 0) &&
            (body.length == 0)) {
              return Literal(Constant(true));
            }
        var i = guard.length - 1; while(i >= 0) {
          val ts:Seq[Tree] = bound(i).asInstanceOf[Array[Tree]];
          val temp = owner.newValue(body(i).pos, cunit.fresh.newName("r$"))
          .setFlag(Flags.SYNTHETIC) .setInfo(body(i).tpe);  // used to be resultType

          var res0: Tree =
            squeezedBlock(
              List(
                typedValDef(temp, body(i)),
                Apply(Ident(exit), List(Ident(temp).setType(temp.tpe)) )
              ),
              Literal(Constant(true))
            ); // forward jump

          if (guard(i) != EmptyTree)
            res0 = And(guard(i), res0);
          res = Or(squeezedBlock(ts.toList, res0), res)
          i = i - 1
        }
      if (_b.or ne null)
        res = Or(res, toTree(_b.or))
      return res
        case _ =>
          scala.Predef.error("error in toTree");
    }
    return res
  }

    class TagNodePair(tag1: int, node1: PatternNode, next1: TagNodePair) {
      var tag: int = tag1
      var node: PatternNode = node1
      var next: TagNodePair = next1

      //def length(): Int =
      //  if (null == next) 1 else (next.length() + 1)
    }

    final private def inheritsFromSealed(tpe:Type): Boolean = {
      val it = tpe.baseClasses.elements
      while(it.hasNext) {
        if(it.next.isSealed) return true
      }
      return false
    }

  //@todo: can this be simplified? (in particular: drop "dup" method)
    protected def toOptTree(node1: PatternNode, selector: Tree): Tree = {
      def insert2(tag: Int, node: PatternNode, current: TagNodePair): TagNodePair = {
        if (current eq null)
          return new TagNodePair(tag, node, null);
        else if (tag > current.tag)
          return new TagNodePair(current.tag, current.node, insert2(tag, node, current.next));
        else if (tag == current.tag) {
          val old = current.node;
          ({current.node = node; node}).or = old;
          return current;
        } else
          return new TagNodePair(tag, node, current);
      }

      def insertNode(tag: int, node: PatternNode, current:TagNodePair): TagNodePair = {
        val newnode = node.dup
        newnode.or = null
        insert2(tag, newnode, current)
      }
      var node = node1;
      //System.err.println("pm.toOptTree called"+node);
      var cases: TagNodePair  = null
      var defaultCase: PatternNode  = null
      while (node ne null)
      node match {
        case ConstrPat(casted) =>
          cases = insertNode(getCaseTag(node.tpe) , node, cases)
          node = node.or

        case DefaultPat() =>
          defaultCase = node
          node = node.or

        case VariablePat(tree) if node.tpe.typeSymbol hasFlag Flags.CASE =>
          cases = insertNode(getCaseTag(node.tpe), node, cases)
          node = node.or

        case _ =>
          scala.Predef.error("errare humanum est")
      }

      var nCases: List[CaseDef] = Nil
      while (cases ne null) {
        if(inheritsFromSealed(cases.node.tpe)) {
          val t = toTree_refined(cases.node, selector, true)
          //Console.println("optimize this"+t+" from this "+cases.node)
          nCases = CaseDef(Literal(Constant(cases.tag)),
                           t) :: nCases;
        } else
        nCases = CaseDef(Literal(Constant(cases.tag)),
                         toTree(cases.node, selector)) :: nCases;
        cases = cases.next
      }

      val defBody =
        if (defaultCase eq null) Literal(Constant(false))
        else toTree(defaultCase.and)

      nCases = CaseDef(Ident(nme.WILDCARD), defBody) :: nCases;
      return Match(Apply(Select(selector.duplicate, defs.ScalaObjectClass_tag),
                         List()),
                   nCases)
    }

    /** why not use plain `if's? the reason is that a failing *guard* must still remain
     *  on the testing path (a kind of backtracking) in order to test subsequent patterns
     *  consider for instance bug#440
     *
     *  @param cond  ...
     *  @param thenp ...
     *  @param elsep ...
     *  @return      ...
     */
    def myIf(cond: Tree, thenp: Tree, elsep: Tree) =
      Or(And(cond, thenp), elsep)

    protected def toTree(node: PatternNode, selector:Tree): Tree = {
      val t = toTree_refined(node, selector, false)
      try {
        //Console.println("type-checking "+t)
        typed { t } // //DEBUG
      } catch {
        case e =>

          Console.println("failed with "+e.getMessage()+" on: "+t)
          //System.exit(-1)
          //null
        t
      }
    }

    protected def toTree_refined(node: PatternNode, selector:Tree, ignoreSelectorType: Boolean): Tree = {
      //Console.println("pm.toTree("+node+","+selector+") selector.tpe = "+selector.tpe+")")
      if (selector.tpe eq null)
        scala.Predef.error("cannot go on due to internal error (type attribute set to null)")
      if (node eq null)
        return Literal(Constant(false));
      else
        node match {
          case DefaultPat() =>
            return toTree(node.and);

          case UnapplyPat(casted, Apply(fn1, appargs)) if casted.tpe.typeSymbol == defs.BooleanClass => // special case
            var useSelector = selector
            val checkType = fn1.tpe match {
              case MethodType(List(argtpe,_*),_) =>
                if(isSubType(selector.tpe, argtpe))
                  Literal(Constant(true))
                else {
                  useSelector = gen.mkAsInstanceOf(selector, argtpe)
                  gen.mkIsInstanceOf(selector.duplicate, argtpe)
                 }
            }
            Or(And(checkType,
             And(Apply(handleOuter(fn1),useSelector::appargs.tail), // test
                 toTree(node.and))
             ),
             toTree(node.or, selector.duplicate))

          case UnapplyPat(casted, fn @ Apply(fn1, appargs)) =>
             var useSelector = selector
             val checkType = fn1.tpe match {
               case MethodType(List(argtpe,_*),_) =>
                 if(isSubType(selector.tpe, argtpe))
                   Literal(Constant(true))
                 else {
                   useSelector = gen.mkAsInstanceOf(selector, argtpe)
                   gen.mkIsInstanceOf(selector.duplicate, argtpe)
                 }
             }
             val fntpe = fn.tpe
             val v = newVar(fn.pos, fntpe)
             var __opt_get__    = typed(Select(Ident(v),nme.get))
             var __opt_nonemp__ = emptynessCheck(v)

             Or(And(checkType,
                       squeezedBlock(
                         List(typedValDef(v,Apply(handleOuter(fn1), useSelector::appargs.tail))),
                         And(__opt_nonemp__,
                           squeezedBlock(List(typedValDef(casted, __opt_get__)),toTree(node.and))))
                ),
              toTree(node.or, selector.duplicate))

          case ConstrPat(casted) =>

            val ntpe = node.tpe
            var cond: Tree = null

          // new, but doesn't work
          // cond = if(ignoreSelectorType) Literal(Constant(true))
          //       else condition(ntpe, selector.duplicate)

          // if type 2 test is same as static type, then just null test
          if(isSubType(selector.tpe, ntpe) && isSubType(ntpe, defs.AnyRefClass.tpe)) {
            cond = NotNull(selector.duplicate)
            nstatic = nstatic + 1
          } else if(ignoreSelectorType) {
            cond = Literal(Constant(true))
          } else {
            cond = typed { gen.mkIsInstanceOf(selector.duplicate, ntpe) }
          }

          // compare outer instance for patterns like foo1.Bar foo2.Bar if not statically known to match
          casted.tpe match {
            case TypeRef(prefix,_,_) if needsOuterTest(casted.tpe, selector.tpe) =>
                //@attention, deep typer bug: if we omit "typed" here, we crash when typing the tree that contains this fragment
                cond = typed{ addOuterCondition(cond, casted.tpe, selector.duplicate, handleOuter) }

            case _ => //ignore ;
          }

          val cast_untyped = gen.mkAsInstanceOf(selector.duplicate, ntpe, true)
          val vdef_untyped = typedValDef(casted,
                                    if(isSubType(selector.tpe,ntpe))
                                      selector.duplicate
                                    else
                                      cast_untyped)
          try {

            val vdef = typed { vdef_untyped }
            val succ = squeezedBlock(List(vdef),toTree(node.and))
            val fail = toTree(node.or, selector.duplicate)

            return myIf(cond, succ, fail)
          } catch {
            case e:TypeError =>
              Console.println("while typing vdef_untyped = "+vdef_untyped);
              Console.println("casted.tpe = "+casted.tpe);
              Console.println("(a) selector.tpe       = "+selector.tpe);
              Console.println("(b) pattern type(ntpe) = "+ntpe);
              Console.println("isSubType(a, b) = "+isSubType(selector.tpe,ntpe))
              Console.println("cast.type = "+(typed {cast_untyped}).tpe)
              throw e
          }
          case SequencePat(casted, len) =>
            val ntpe = node.tpe

            val tpetest =
              if(!isSubType(selector.tpe, ntpe))
                gen.mkIsInstanceOf(selector.duplicate, ntpe);
              else
                NotNull(selector.duplicate)

            val treeAsSeq =
              if(!isSubType(selector.tpe, ntpe))
                typed(gen.mkAsInstanceOf(selector.duplicate, ntpe, true))
              else
                selector.duplicate

            val succ: Tree = squeezedBlock(
              List(typedValDef(casted, treeAsSeq)),
              toTree(node.and))

            val fail = toTree(node.or, selector.duplicate)

            return Or( And( And( tpetest, seqHasLength(treeAsSeq.duplicate, ntpe, len)), succ ),
                      fail);

          case RightIgnoringSequencePat(casted, castedRest, minlen) =>

            val ntpe = node.tpe

            val tpetest =
              if(!isSubType(selector.tpe,ntpe))
                gen.mkIsInstanceOf(selector.duplicate, ntpe);
              else
                Literal(Constant(true))

            val treeAsSeq =
              if (!isSubType(selector.tpe,ntpe))
                typed(gen.mkAsInstanceOf(selector.duplicate, ntpe, true))
              else
                selector.duplicate

            val cond =
              if (minlen == 0) tpetest
              else And(tpetest, seqLongerThan(treeAsSeq, ntpe, minlen))

            var bindings =
              if(castedRest ne null)
                List(typedValDef(castedRest, seqDrop(treeAsSeq.duplicate, minlen)))
              else
                List()

            bindings = typedValDef(casted, treeAsSeq.duplicate) :: bindings

            val succ = squeezedBlock(bindings, toTree(node.and))
            val fail = toTree(node.or, selector.duplicate)

            return Or(And(cond, succ), fail);

          case ConstantPat(value) =>
            val succ = toTree(node.and)
            val fail = toTree(node.or, selector.duplicate)
            return myIf(Equals(selector.duplicate,
                             typed(Literal(Constant(value))).setType(node.tpe)),
                        succ,
                        fail);

          case VariablePat(tree) =>
            // objects are compared by eq, not ==
            val cmp = if (tree.tpe.termSymbol.isModule && selector.tpe <:< defs.AnyRefClass.tpe)
                        Eq(selector.duplicate, tree)
                      else
                        Equals(selector.duplicate, tree)
            return myIf(cmp,
                      toTree(node.and),
                      toTree(node.or, selector.duplicate));

          case AltPat(header) =>
            return myIf(toTree(header),
                      toTree(node.and),
                      toTree(node.or, selector.duplicate));
          case _ =>
            scala.Predef.error("cannot handle pattern:"+node)
        }
    }
  }
}


