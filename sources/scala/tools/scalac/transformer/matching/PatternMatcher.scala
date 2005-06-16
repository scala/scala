/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */


import scala.tools.util.Position;
import scalac._;
import scalac.ast._;
import scalac.atree.AConstant;
import scalac.util._;
import scalac.symtab._;
//import scalac.transformer.matching.CodeFactory;
//import scalac.transformer.matching.PatternNodeCreator;
//import scalac.transformer.matching.PatternNode;
//import scalac.transformer.matching.CaseEnv;
//import scalac.transformer.matching.PatternTool;
//import PatternNode._;
import Tree._;

package scala.tools.scalac.transformer.matching {

class PatternMatcher(unit: CompilationUnit) extends PatternTool(unit) {

  protected var optimize = true;
  protected var delegateSequenceMatching = false;
  protected var doBinding = true;

  /** the owner of the pattern matching expression
   */
  protected var owner:Symbol = _ ;

  /** the selector expression
   */
  protected var selector: Tree = _;

  /** the root of the pattern node structure
   */
  protected var root: PatternNode = _;

  /** the symbol of the result variable
   */
  protected var resultVar: Symbol = _;

  /** methods to generate scala code
   */
  protected var cf: CodeFactory = _;

  /** methods to create pattern nodes
   */
  protected var mk: PatternNodeCreator = _;

  /** init method, also needed in subclass AlgebraicMatcher
   */
  def initialize(selector: Tree, owner: Symbol,
                 resultType: Type, doBinding: Boolean): Unit = {
    this.mk = new PatternNodeCreator(unit, owner);
    this.cf = new CodeFactory(unit, selector.pos);
    this.root = mk.ConstrPat(selector.pos, selector.getType().widen());
    this.root.and = mk.Header(selector.pos,
                              selector.getType().widen(),
                              gen.Ident(selector.pos, root.symbol()));
    this.resultVar = owner.newVariable(selector.pos,
                                       Modifiers.MUTABLE,
                                       Names.result);
    this.resultVar.setType(resultType);
    this.owner = owner;
    this.selector = selector;
    this.optimize = this.optimize & (unit.global.target == Global.TARGET_JVM);
    this.doBinding = doBinding;
  }

  /** pretty printer
   */
  def print(): Unit = {
    new PatternNodeDebugPrinter().print(root.and, "");
  }

  /** enters a sequence of cases into the pattern matcher
   */
  def construct(cases: Array[Tree]): Unit = {
    //Console.println("pattern matcher.construct "+cases);
    var i = 0; while(i < cases.length) {
      enter(cases(i));
      i = i + 1
    }
  }

  /** enter a single case into the pattern matcher
   */
  protected def enter(caseDef: Tree): Unit = {
    caseDef match {
      case CaseDef(pat, guard, body) =>
        val env = new CaseEnv(owner, unit);
        // PatternNode matched = match(pat, root);
        val target = enter1(pat, -1, root, root.symbol(), env);
        // if (target.and != null)
        //     unit.error(pat.pos, "duplicate case");
            if (null == target.and)
              target.and = mk.Body(caseDef.pos, env.getBoundVars(), guard, body);
            else if (target.and.isInstanceOf[Body])
              updateBody(target.and.asInstanceOf[Body], env.getBoundVars(), guard, body);
            else {
              //Console.println("target is "+target.getClass());
              //Console.println("target.and is "+target.and.getClass());
              //Console.println("target.or is "+target.or.getClass());

              unit.error(pat.pos, "duplicate case");
            }
    }
  }

  /** this method is only called if 2 patterns are redundant. In such
   *  a case, the right body has to be chosen by means of the guards!
   *    case 3 if false => "x";
   *    case 3 if true => "y";
   */
  protected def updateBody(tree: Body, bound: Array[ValDef], guard: Tree , body: Tree): Unit = {
    if (tree.guard(tree.guard.length - 1) == Tree.Empty) {
      //unit.error(body.pos, "unreachable code");
    } else {
      val bd = new Array[Array[ValDef]](tree.bound.length + 1);
      val ng = new Array[Tree](tree.guard.length + 1);
      val nb = new Array[Tree](tree.body.length + 1);
      System.arraycopy(tree.bound, 0, bd, 0, tree.bound.length);
      System.arraycopy(tree.guard, 0, ng, 0, tree.guard.length);
      System.arraycopy(tree.body, 0, nb, 0, tree.body.length);
      bd(bd.length - 1) = bound;
      ng(ng.length - 1) = guard;
      nb(nb.length - 1) = body;
      tree.bound = bd ;
      tree.guard = ng ;
      tree.body = nb ;
    }
  }

  protected def patternArgs(tree: Tree):Array[Tree] = {
    tree match {
      case Bind(_, pat)   => patternArgs(pat);
      case Apply(_, args) => if ( isSeqApply(tree.asInstanceOf[Apply])
                                   && !delegateSequenceMatching)
                              args(0) match {
                                case Sequence(ts) => ts;
                                case _            => args;
                              }
                             else
                               args

      case Sequence(ts) => if (!delegateSequenceMatching)
                            ts;
                           else
                             Tree.EMPTY_ARRAY;

      case _            => Tree.EMPTY_ARRAY;
      }
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
  protected def isSeqApply( tree: Tree.Apply  ): Boolean =
    (( tree.args.length == 1 ) && tree.args(0).isInstanceOf[Sequence])
    && (tree.getType().symbol().flags & Modifiers.CASE) == 0;

  protected def patternNode(tree:Tree , header:Header , env: CaseEnv ): PatternNode  = {
    //Console.println("patternNode("+tree+","+header+")");
    //Console.println("tree.getType()"+tree.getType());
    tree match {
      case Bind(name, Typed(Ident(Names.PATTERN_WILDCARD), tpe)) => // x@_:Type
      if (header.getTpe().isSubType(tpe.getType())) {
        val node = mk.DefaultPat(tree.pos, tpe.getType());
        env.newBoundVar( tree.symbol(), tree.getType(), header.selector );
        node;
      }
      else {
        val node = mk.ConstrPat(tree.pos, tpe.getType());
        env.newBoundVar( tree.symbol(), tree.getType(), gen.Ident(tree.pos, node.casted));
        node;
      }
      case Bind(name, Ident(Names.PATTERN_WILDCARD)) => // x @ _
        val node = mk.DefaultPat(tree.pos, header.getTpe());
        if ((env != null) && (tree.symbol() != defs.PATTERN_WILDCARD))
          env.newBoundVar( tree.symbol(), tree.getType(), header.selector);
        node;
      case Bind(name, pat) =>
        //if ( tree.getType() == null ) throw new ApplicationError("Bind tpe is null"); // DEBUG
        val node = patternNode(pat, header, env);
        if ((env != null) && (tree.symbol() != defs.PATTERN_WILDCARD)) {
          val casted = node.symbol();
          val theValue =  if (casted == Symbol.NONE) header.selector else gen.Ident(tree.pos, casted);
          env.newBoundVar(tree.symbol(), tree.getType(), theValue);
        }
       node;
      case t @ Apply(fn, args) =>             // pattern with args
        if (isSeqApply(t)) {
          if (!delegateSequenceMatching) {
            args(0) match {
              case Sequence(ts)=>
                mk.SequencePat(tree.pos, tree.getType(), ts.length);
            }
          } else {
            val res = mk.ConstrPat(tree.pos, tree.getType());
            res.and = mk.Header(tree.pos, header.getTpe(), header.selector);
            res.and.and = mk.SeqContainerPat(tree.pos, tree.getType(), args(0));
            res;
          }
        } else if ((fn.symbol() != null) &&
                   fn.symbol().isStable() &&
                   !(fn.symbol().isModule() &&
                     ((fn.symbol().flags & Modifiers.CASE) != 0))) {
                       mk.VariablePat(tree.pos, tree);
                     }
           else {
             /*
            Console.println("apply but not seqApply");
            Console.println("tree.getType()="+tree.getType());
            Console.println("tree.symbol()="+tree.symbol());
             */
             mk.ConstrPat(tree.pos, tree.getType());
           }
        case t @ Typed(ident, tpe) =>       // variable pattern
          //Console.println("typed! header:" + header.getTpe() +" "+tpe.getType());
          val doTest = header.getTpe().isSubType(tpe.getType());
          val node = {
            if(doTest)
              mk.DefaultPat(tree.pos, tpe.getType())
            else
              mk.ConstrPat(tree.pos, tpe.getType());
          }
          if ((null != env) && (ident.symbol() != defs.PATTERN_WILDCARD))
            node match {
              case ConstrPat(casted) =>
                env.newBoundVar(t.expr.symbol(),
                                tpe.getType(),
                                gen.Ident(tree.pos, casted));
              case _ =>
                env.newBoundVar(t.expr.symbol(),
                                tpe.getType(),
                                {if(doTest) header.selector else gen.Ident(tree.pos, node.asInstanceOf[ConstrPat].casted)});
            }
          node;

        case Ident(name) =>               // pattern without args or variable
            if (tree.symbol() == defs.PATTERN_WILDCARD)
              mk.DefaultPat(tree.pos, header.getTpe());
            else if (tree.symbol().isPrimaryConstructor()) {
              throw new ApplicationError("this may not happen"); // Burak
            } else if (name.isVariable()) {//  Burak
              throw new ApplicationError("this may not happen"); // Burak
            } else
              mk.VariablePat(tree.pos, tree);

        case Select(_, name) =>                                  // variable
            if (tree.symbol().isPrimaryConstructor())
                mk.ConstrPat(tree.pos, tree.getType());
            else
                mk.VariablePat(tree.pos, tree);

        case Literal(value) =>
            mk.ConstantPat(tree.pos, tree.getType(), value);

        case Sequence(ts) =>
            if ( !delegateSequenceMatching ) {
              //throw new ApplicationError("cannot happen:"+tree);
              //Console.println("PatternMatcher:patternNode:Sequence "+tree.getType());
                mk.SequencePat(tree.pos, tree.getType(), ts.length);
            } else {
                mk.SeqContainerPat(tree.pos, tree.getType(), tree);
            }
        case Alternative(branches) =>
          throw new ApplicationError("kicked out Alternatives");
      /*
          if(branches.length < 2)
            throw new ApplicationError("ill-formed Alternative");
          val subroot = mk.ConstrPat(header.pos, header.getTpe());
          subroot.and = mk.Header(header.pos, header.getTpe(), header.selector.duplicate());
            val subenv = new CaseEnv(owner, unit);
          var i = 0; while(i < branches.length) {
            val target = enter1(branches(i), -1, subroot, subroot.symbol(), subenv);
            target.and = mk.Body(tree.pos);
            i = i + 1
          }
          mk.AltPat(tree.pos, subroot.and.asInstanceOf[Header]);
      */
        case _ =>
          throw new ApplicationError("unit = " + unit + "; tree = "+tree);
    }
  }

  protected def enter(pat: Tree, index: Int, target: PatternNode, casted: Symbol, env: CaseEnv ): PatternNode = {
    target match {
      case ConstrPat(newCasted) =>
        enter1(pat, index, target, newCasted, env);
      case SequencePat(newCasted, len) =>
        enter1(pat, index, target, newCasted, env);
      case _ =>
        enter1(pat, index, target, casted, env);
    }
  }

  private def newHeader(pos: Int, casted: Symbol, index: Int): Header = {
    //Console.println("newHeader(pos,"+casted+","+index+")");
    //Console.println("  casted.getType()"+casted.getType());
    val ident = gen.Ident(pos, casted);
    if (casted.pos == Position.FIRSTPOS) {
      //Console.println("FIRSTPOS");
      val t = gen.mkApply_V(
        gen.Select( ident, defs.FUNCTION_APPLY( 1 )),
        Predef.Array[Tree]( gen.mkIntLit( pos, index ) ));
      val seqType = t.getType();
      mk.Header( pos, seqType, t );
    } else {
      //Console.println("NOT FIRSTPOS");
      val ts = casted.getType().symbol().asInstanceOf[ClassSymbol]
        .caseFieldAccessor(index);
      //Console.println("ts="+ts);
      val accType = casted.getType().memberType(ts);
      val accTree = gen.Select( ident, ts);
      accType match {
        // scala case accessor
        case Type.MethodType(_, _) =>
          mk.Header(pos, accType.resultType(), gen.mkApply__(accTree));
        // jaco case accessor
        case _ =>
          mk.Header(pos, accType, accTree);
      }
    }
  }

  /** main enter function
   *
   *  invariant: ( curHeader == (Header)target.and ) holds
   */
  protected def enter1(pat: Tree, index: Int, target: PatternNode, casted: Symbol,  env: CaseEnv): PatternNode = {
    //System.err.println("enter(" + pat + ", " + index + ", " + target + ", " + casted + ")");
    val patArgs   = patternArgs(pat);                 // get pattern arguments
    var curHeader = target.and.asInstanceOf[Header];  // advance one step in IR
    if (curHeader == null) {                 // check if we must add new header
      //assert index >= 0 : casted;
      if (index < 0) throw new ApplicationError("error entering:" + casted);
      target.and = {curHeader = newHeader(pat.pos, casted, index); curHeader};
      curHeader.or = patternNode(pat, curHeader, env);
      enter(patArgs, curHeader.or, casted, env);

    } else {
      // find most recent header
      while (curHeader.next != null)
      curHeader = curHeader.next;
      // create node
      var patNode = patternNode(pat, curHeader, env);
      var next: PatternNode = curHeader;
      // add branch to curHeader, but reuse tests if possible
      while (true) {
        if (next.isSameAs(patNode)) {     // patNode already present ?--> reuse
          // substitute... !!!
          patNode match {
            case ConstrPat(ocasted) =>
              env.substitute(ocasted, gen.Ident(patNode.pos,
                                                next.asInstanceOf[ConstrPat].casted));
            case _ =>
          }
          return enter(patArgs, next, casted, env);
        } else if (next.isDefaultPat() ||           // default case reached, or
                   ((next.or == null) &&            //  no more alternatives and
                    (patNode.isDefaultPat() || next.subsumes(patNode)))) {
                      // new node is default or subsumed
                      var header = mk.Header(patNode.pos,
                                             curHeader.getTpe(),
                                             curHeader.selector);
                      {curHeader.next = header; header};
                      header.or = patNode;
                      return enter(patArgs,
                                   patNode,
                                   casted,
                                   env);
                    }
          else if (next.or == null) {
            return enter(patArgs, {next.or = patNode; patNode}, casted, env); // add new branch
          } else
            next = next.or;
      }
      throw new ApplicationError("must not happen");
      null // never reached
    }
  }

  /** calls enter for an array of patterns, see enter
   */
  protected def enter(pats:Array[Tree], target1: PatternNode , casted1: Symbol  , env: CaseEnv): PatternNode = {
    var target = target1;
    var casted = casted1;
    target match {
      case ConstrPat(newCasted) =>
        casted = newCasted;
      case SequencePat(newCasted, len) =>
        casted = newCasted;
      case _ =>
    }
    var i = 0; while(i < pats.length) {
      target = enter1(pats(i), i, target, casted, env);
      i = i + 1
    }
    target;
  }

  protected def nCaseComponents(tree: Tree): int = {
    tree match {
      case Apply(fn, _) =>
        val tpe = tree.getType().symbol().primaryConstructor().getType();
      //Console.println("~~~ " + tree.type() + ", " + tree.type().symbol().primaryConstructor());
      tpe match {
        // I'm not sure if this is a good idea, but obviously, currently all case classes
        // without constructor arguments have type NoType
        case Type.NoType =>
          throw new ApplicationError("this cannot happen");
        0
        case Type.MethodType(args, _) =>
          args.length;
        case Type.PolyType(tvars, Type.MethodType(args, _)) =>
          args.length;
        case Type.PolyType(tvars, _) =>
          0;
        case _ =>
          throw new ApplicationError("not yet implemented;" +
                                     "pattern matching for " + tree + ": " + tpe);
      }
    }
    return 0;
  }


  //////////// generator methods

  def toTree(): Tree = {
    if (optimize && isSimpleIntSwitch())
      intSwitchToTree();
    else if (false /* optimize && isSimpleSwitch() */) // disabled
      switchToTree();
    else {
      //print();
      generalSwitchToTree();
    }
  }


  protected def isSimpleIntSwitch(): Boolean = {
    if (selector.getType().widen().isSameAs(defs.int_TYPE())) {
      var patNode = root.and;
      while (patNode != null) {
        var node = patNode;
        while (({node = node.or; node}) != null) {
          node match {
            case ConstantPat(_) => ;
            case _ =>
              return false;
          }
          node.and match {
            case _b:Body =>
              if ((_b.guard.length > 1) ||
                  (_b.guard(0) != Tree.Empty) ||
                  (_b.bound(0).length > 0))
                return false;
            case _ =>
              return false;
          }
        }
        patNode = patNode.nextH();
      }
      return true;
    } else
      return false;
  }


  protected def numCases(patNode1: PatternNode): Int = {
    var patNode = patNode1;
    var n = 0;
    while (({patNode = patNode.or; patNode}) != null)
    patNode match {
      case DefaultPat() => ;
      case _ =>
        n = n + 1;
    }
    n;
  }

  protected def _defaultBody(patNode1: PatternNode, otherwise: Tree ): Tree = {
    var patNode = patNode1;
    while (patNode != null) {
      var node = patNode;
      while (({node = node.or; node}) != null)
      node match {
        case DefaultPat() =>
          return node.and.bodyToTree();
        case _ =>
      }
      patNode = patNode.nextH();
    }
    otherwise;
  }

  /** This method translates pattern matching expressions that match
   *  on integers on the top level.
   */
  def intSwitchToTree(): Tree = {

    case class TagBodyPair(tag: Int, body: Tree, next: TagBodyPair) {
      def length(): Int = {
        if (null == next) 1 else (next.length() + 1);
      }
    }
    def insert(tag: Int, body: Tree, current: TagBodyPair): TagBodyPair = {
      if (current == null)
        return new TagBodyPair(tag, body, null);
      else if (tag > current.tag)
        return new TagBodyPair(current.tag, current.body, insert(tag, body, current.next));
      else
        return new TagBodyPair(tag, body, current);
    }
    //print();
    val ncases = numCases(root.and);
    val matchError = cf.ThrowMatchError(selector.pos, resultVar.getType());
    // without a case, we return a match error if there is no default case
    if (ncases == 0)
      return _defaultBody(root.and, matchError);
    // for one case we use a normal if-then-else instruction
    else if (ncases == 1) {
      root.and.or match {
        case ConstantPat(value) =>
          return gen.If(cf.Equals(selector,
                                  gen.Literal(root.and.or.pos, value)),
                        (root.and.or.and).bodyToTree(),
                        _defaultBody(root.and, matchError));
        case _ =>
          return generalSwitchToTree();
      }
    }
    //
    // if we have more than 2 cases than use a switch statement

    val _h:Header = root.and.asInstanceOf[Header];
    val next = _h.next;
    var mappings: TagBodyPair = null;
    var defaultBody = matchError;
    var patNode = root.and;

    // convert or-branches to sorted list of tag-body pairs

    while (patNode != null) {
      var node = patNode.or;
      while (node != null) {
        node match {
          case DefaultPat() =>
            if (defaultBody != null)
              throw new ApplicationError(); // shouldn't happen
          defaultBody = node.and.bodyToTree();
          node = node.or;

          case ConstantPat(AConstant.INT(value))=>
            mappings = insert(value, node.and.bodyToTree(), mappings);
            node = node.or;

          case _ =>
            throw new ApplicationError(node.toString());
        }
      }
      patNode = patNode.nextH();
    }
    /* mappings != null because numCases(...) > 2
     * so there were at least 2 or-branches
     */
    var n = mappings.length();
    val tags = new Array[Int](n);
    val bodies = new Array[Tree](n);
    n = 0;
    while (mappings != null) {
      tags(n) = mappings.tag;
      bodies(n) = mappings.body;
      n = n + 1;
      mappings = mappings.next;
    }
    return gen.Switch(selector,tags, bodies, defaultBody, resultVar.getType());
  }

    def generalSwitchToTree(): Tree = {
      val ts = Predef.Array[Tree] (
        gen.ValDef(root.symbol(), selector),
        gen.ValDef(resultVar, gen.mkDefaultValue(selector.pos, resultVar.getType())));
      val res = gen.If(
        selector.pos,
        toTree(root.and),
        gen.Ident(selector.pos, resultVar),
        cf.ThrowMatchError(selector.pos, resultVar.getType(), gen.Ident(selector.pos, root.symbol())));
      return gen.mkBlock(selector.pos, ts, res);
    }

  def toTree(node1: PatternNode): Tree = {
    def optimize1(selType:Type, alternatives1: PatternNode ): boolean = {
      var alts = alternatives1;
      if (!optimize || !selType.isSubType(defs.SCALAOBJECT_TYPE()))
        return false;
      var cases = 0;
      while (alts != null) {
        alts match {
          case ConstrPat(_) =>
            if (alts.getTpe().symbol().isCaseClass())
              cases = cases +1;
            else
              return false;

          case DefaultPat() =>
            ;
          case _ =>
            return false;
        }
        alts = alts.or;
      }
      return cases > 2;
    }

    var node = node1;
    var res = gen.mkBooleanLit(node.pos, false);

    while (node != null)
      node match {
        case _h:Header =>
          val selector = _h.selector;
          val next = _h.next;
          //res = cf.And(mkNegate(res), toTree(node.or, selector));
          //Console.println("HEADER TYPE = " + selector.type);

        /* TEST
         next match {
         case _b:Body if ((_b.guard.length > 1) ||
         (_b.guard(0) != Tree.Empty)) =>
         Console.println("guard! = "+_b.guard(0));
         case _ =>
         }
         */

          if (optimize1(node.getTpe(), node.or))
            res = cf.Or(res, toOptTree(node.or, selector));
          else
            res = cf.Or(res, toTree(node.or, selector));
          node = next;

        case _b:Body =>
          var bound = _b.bound;
          val guard = _b.guard;
          val body  = _b.body;
          if ((bound.length == 0) &&
              (guard.length == 0) &&
              (body.length == 0)) {
                return gen.mkBooleanLit(node.pos, true); // cf.Or(res, gen.mkBooleanLit(node.pos, true));
              } else if (!doBinding)
                bound = Predef.Array[Array[ValDef]]( Predef.Array[ValDef]() );
          var i = guard.length - 1; while(i >= 0) {
            val ts = bound(i).asInstanceOf[Array[Tree]];
            var res0 = gen.mkBlock(gen.Assign(gen.Ident(body(i).pos, resultVar),
                                              body(i)),
                                   gen.mkBooleanLit(body(i).pos, true));
            if (guard(i) != Tree.Empty)
              res0 = cf.And(guard(i), res0);
            res = cf.Or(gen.mkBlock(body(i).pos, ts, res0), res);
            i = i - 1
          }
        return res;
      }
    return res;
  }

  protected def toOptTree(node1: PatternNode, selector: Tree): Tree = {

    class TagNodePair(tag1: int, node1: PatternNode, next1: TagNodePair) {
      var tag: int = tag1;
      var node: PatternNode = node1;
      var next: TagNodePair = next1;

      def  length(): Int = {
        return if (null == next)  1 else (next.length() + 1);
      }
    }
    def  insertNode(tag:int , node:PatternNode , current:TagNodePair ): TagNodePair = {
      def insert(tag: Int, node: PatternNode, current: TagNodePair): TagNodePair = {
        if (current == null)
          return new TagNodePair(tag, node, null);
        else if (tag > current.tag)
          return new TagNodePair(current.tag, current.node, insert(tag, node, current.next));
        else if (tag == current.tag) {
          val old = current.node;
          current.node = node;
          node.or = old;
          return current;
        } else
          return new TagNodePair(tag, node, current);
      } // def insert
      val newnode = node.dup();
      newnode.or = null;
      return insert(tag, newnode, current);
    } // def insertNode

    var node = node1;
    //System.err.println("pm.toOptTree called"+node);
    var cases: TagNodePair  = null;
    var defaultCase: PatternNode  = null;
    while (node != null)
    node match {
      case ConstrPat(casted) =>
        cases = insertNode(node.getTpe().symbol().tag(), node, cases);
        node = node.or;

      case DefaultPat() =>
        defaultCase = node;
        node = node.or;
    }
    var n = cases.length();
    val tags = new Array[int](n);
    val bodies = new Array[Tree](n);
    n = 0;
    while (null != cases) {
      tags(n) = cases.tag;
      bodies(n) = toTree(cases.node, selector);
      n = n + 1;
      cases = cases.next;
    }
    return gen.Switch(gen.mkApply__(gen.Select(selector.duplicate(), defs.SCALAOBJECT_TAG())),
                      tags,
                      bodies,
                      { if (defaultCase == null) gen.mkBooleanLit(selector.pos, false) else toTree(defaultCase.and) },
                      defs.boolean_TYPE());
  }

    protected def toTree(node:PatternNode , selector:Tree ): Tree = {
      //System.err.println("pm.toTree("+node+","+selector+")");
      if (node == null)
        return gen.mkBooleanLit(selector.pos, false);
      else
        node match {
          case DefaultPat() =>
            return toTree(node.and);

          case ConstrPat(casted) =>
            return gen.If(gen.mkIsInstanceOf(selector.duplicate(), node.getTpe()),
                          gen.mkBlock(gen.ValDef(casted,
                                                 gen.mkAsInstanceOf(selector.pos, selector.duplicate(), node.getTpe(), true)),
                                      toTree(node.and)),
                          toTree(node.or, selector.duplicate()));
          case SequencePat(casted, len) =>
            return
          cf.Or(
            cf.And(
              cf.And(gen.mkIsInstanceOf(selector.duplicate(), node.getTpe()),
                     cf.Equals(gen.mkApply__(gen.Select(gen.mkAsInstanceOf(selector.pos, selector.duplicate(), node.getTpe(), true),
                                                        defs.SEQ_LENGTH())),
                               gen.mkIntLit(selector.pos, len))),
              gen.mkBlock(gen.ValDef(casted,
                                     gen.mkAsInstanceOf(selector.pos, selector.duplicate(), node.getTpe(), true)),
                          toTree(node.and))),
            toTree(node.or, selector.duplicate()));
          case ConstantPat(value) =>
            return gen.If(cf.Equals(selector.duplicate(),
                                    gen.Literal(selector.pos, value)),
                          toTree(node.and),
                          toTree(node.or, selector.duplicate()));
          case VariablePat(tree) =>
            return gen.If(cf.Equals(selector.duplicate(), tree),
                          toTree(node.and),
                          toTree(node.or, selector.duplicate()));
          /*
          case AltPat(header) =>
            return gen.If(toTree(header),
                          toTree(node.and),
                          toTree(node.or, selector.duplicate()));
                          */
        }
    }


  protected def isSimpleSwitch(): Boolean  = {
    /*

  case class Break(res:Boolean) extends java.lang.Throwable;
  case class Break2() extends java.lang.Throwable;


    print();
    var patNode = root.and;
    while (patNode != null) {
      var node = patNode;
      while (({node = node.or; node}) != null) {
        node match {
          case VariablePat(tree) =>
            Console.println(((tree.symbol().flags & Modifiers.CASE) != 0));
          case ConstrPat(_) =>
            Console.println(node.getTpe().toString() + " / " + ((node.getTpe().symbol().flags & Modifiers.CASE) != 0));
          var inner = node.and;
          def funct(inner: PatternNode): Boolean = {
            //outer: while (true) {
            inner match {
              case _h:Header =>
                if (_h.next != null)
                  throw Break(false);
              funct(inner.or)

              case DefaultPat() =>
                funct(inner.and);

              case b:Body =>
                if ((b.guard.length > 1) ||
                    (b.guard(0) != Tree.Empty))
                  throw Break(false);

              throw Break2() // break outer
              case _ =>
                Console.println(inner);
              throw Break(false);
            }
          }
          var res = false;
          var set = false;
          try {
            funct(inner)
          } catch {
            case Break(res1) =>
              res = res1;
              set = true;
              case Break2() =>
          }
          if(set) return res;
          case _ =>
            return false;
        }
        }
      patNode = patNode.nextH();
      }
    return true;
    */
    return false;
  }

  def switchToTree(): Tree = {
    throw new Error();
  }

}
}

/* idea::

  case class BooleanTree(x:Tree) {
    def &&&(z:BooleanTree) = new BooleanTree(cf.And(x,z.x));
    def |||(z:BooleanTree) = new BooleanTree(cf.Or(x,z.x));
  }
  def view(x:Tree): BooleanTree = new BooleanTree(x);
  def view(x:BooleanTree): Tree = x.x;

  def view(x:Symbol): AssignableTree = new AssignableTree(x);
  case class AssignableTree(x:Symbol) {
    def := (z:Tree) = gen.Assign(gen.Ident(z.pos, x), z);
  }

  def view(x:Tree): TestableTree = new TestableTree(x);
  case class TestableTree(x:Tree) {
    def __isOf__ (z:Type) = gen.mkIsInstanceOf(x, z)
  }

  def view(x:Tree): CastableTree = new CastableTree(x);
  case class CastableTree(x:Tree) {
    def __castTo__ (z:Type) = gen.mkAsInstanceOf(x, z)
  }

  def view(x:Tree): EquatableTree = new EquatableTree(x);
  case class EquatableTree(x:Tree) {
    def === (z:Tree) = cf.Equals(x,z)
  }

  def view(x:Tree): SelectableTree = new SelectableTree(x);
  case class SelectableTree(x:Tree) {
    def __dot__ (z:Symbol) = gen.Select(x, z)
  }

  def view(x:Symbol): ValDefableTree = new ValDefableTree(x);
  case class ValDefableTree(x:Symbol) {
    def __valdef__ (z:Tree) = gen.ValDef(x,z);
  }

  protected def toTree(node:PatternNode , _selector:Tree ): Tree = {

    def selector() = _selector.duplicate();
    def pos() = _selector.pos;

      //System.err.println("pm.toTree("+node+","+selector+")");
      if (node == null)
        return gen.mkBooleanLit(pos(), false);
      else
        node match {
          case DefaultPat() =>
            return toTree(node.and);

          case ConstrPat(casted) =>
            return gen.If(selector() __isOf__ node.getTpe(),
                          gen.mkBlock(casted __valdef__ (selector() __castTo__ node.getTpe()),
                                      toTree(node.and)),
                          toTree(node.or, selector()));

          case SequencePat(casted, len) =>
            return gen.If((selector() __isOf__ node.getTpe()) &&&
                          (gen.mkApply__(selector() __castTo__ node.getTpe() __dot__ (defs.SEQ_LENGTH())) === gen.mkIntLit(pos(), len)),
                          gen.mkBlock(casted __valdef__ (selector() __castTo__ node.getTpe()),
                                      toTree(node.and)),
                          toTree(node.or, selector()));
          case ConstantPat(value) =>
            return gen.If(selector() === gen.Literal(pos(), value),
                          toTree(node.and),
                          toTree(node.or, selector()));
          case VariablePat(tree) =>
            return gen.If(selector() === tree,
                          toTree(node.and),
                          toTree(node.or, selector()));
          case AltPat(header) =>
            return gen.If(toTree(header),
                          toTree(node.and),
                          toTree(node.or, selector()));
          case _ =>
            throw new ApplicationError();
        }
    }
*/
