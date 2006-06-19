/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author buraq
 */
// $Id$

package scala.tools.nsc.matching

/** the pattern matcher, tweaked to work with regular patterns
 *  @author Burak Emir
 */
trait AlgebraicMatchers requires TransMatcher {

  import global._

  class AlgebraicMatcher extends PatternMatcher {

    import java.util.Vector
    import java.util.Iterator

    var _m: PartialMatcher = _

    /*override protected var*/ optimize = false

    /** constructs an algebraic pattern matcher from cases */
    def construct(m: PartialMatcher, cases: List[CaseDef]): Unit =
      construct(m, cases, true)

    /** constructs an algebraic pattern matcher from cases */
    def construct(m: PartialMatcher, cases: List[CaseDef], doBinding: Boolean): Unit = {
      this._m = m
      super.initialize(_m.selector, _m.owner, doBinding)

      val it = cases.elements
      while (it.hasNext) {
        val cdef = it.next
        /*
        if(cdef != null)
        Console.println("algebraic matcher: "+cdef.toString()); // DEBUG
        else
        scala.Predef.error("got CaseDef null in alg matcher!");
        */
        enter(cdef)
      }

      //if (unit.global.log()) {
      //  unit.global.log("internal pattern matching structure");
      //  print();
      // }
      _m.tree = toTree()
    }

    /** initializes this AlgebraicMatcher, see Matcher.initialize
    void initialize() {}
    */
    /*
    def isStarApply(tree: Tree.Apply): Boolean = {
    val params:Array[Symbol] = tree.fun.getType().valueParams();
    //System.err.println( tree.fun.type.resultType().symbol() );
    (tree.args.length == 1)
    && (tree.getType().symbol().flags & Modifiers.CASE) != 0
    && params.length > 0
    && (params(params.length-1).flags & Modifiers.REPEATED) != 0;
    }
    */
    //////////// generator methods

    override def toTree(): Tree = {

      this.exit = currentOwner.newLabel(root.pos, "exitA")
        .setInfo(new MethodType(List(resultType), resultType))

      val result = exit.newValueParameter(root.pos, "resultA").setInfo(resultType)

      Block(
        List(
          ValDef(root.symbol, _m.selector)
        ),
        If( super.toTree(root.and),
           LabelDef(exit, List(result), Ident(result)),
           ThrowMatchError( _m.pos, Ident(root.symbol) ))
      )
    }

    protected override def toTree(node: PatternNode, selector: Tree): Tree = {
      //System.err.println("AM.toTree called"+node);
      if (node == null)
        Literal(false)
      else node match {
        case SeqContainerPat( _, _ ) =>
          callSequenceMatcher(node, selector)
        case _ =>
          super.toTree(node, selector)
      }
    }

    /** collects all sequence patterns and returns the default
     */
    def collectSeqPats(node1: PatternNode, seqPatNodes: Vector, bodies: Vector): PatternNode = {
      var node = node1
      var defaultNode: PatternNode = null
      var exit = false
        do {
          if (node == null)
            exit = true //defaultNode = node; // break
          else
            node match {
              case SeqContainerPat( _, _ ) =>
                seqPatNodes.add(node)
                bodies.add(super.toTree(node.and))
                node = node.or
                exit //defaultNode = node; break;
              case _ =>
                defaultNode = node
            }
        } while (!exit && (null == defaultNode))

      defaultNode
    }

    def callSequenceMatcher(node: PatternNode, selector1: Tree): Tree = {

      //Console.println("calling sequent matcher for"+node);
      /*    ???????????????????????? necessary to test whether is a Seq?
       gen.If(selector.pos, maybe And( Is(selector, seqpat.type()) )???)
      */

      // translate the _.and subtree of this SeqContainerPat

      val seqPatNodes = new Vector()
      val bodies      = new Vector()

      var defaultNode = collectSeqPats(node, seqPatNodes, bodies)

      val defaultCase = toTree(defaultNode, selector1)

      val wordRec = new SequenceMatcher()

      val m = new PartialMatcher {
        val owner = _m.owner
        val selector = selector1
      }

      var pats: scala.List[Tree] = Nil
      var body: scala.List[Tree] = Nil

      val tmp = bodies.toArray()
      var j = 0
      val it = seqPatNodes.iterator()
      while (it.hasNext()) {
        //pats(j) = it.next().asInstanceOf[SeqContainerPat].seqpat;
        pats = it.next().asInstanceOf[SeqContainerPat].seqpat :: pats
        //body(j) = tmp(j).asInstanceOf[Tree];
        body = tmp(j).asInstanceOf[Tree] :: body
        j = j + 1
      }
      //Tree defaultTree = toTree(node.or, selector); // cdef.body ;

      wordRec.construct(m, pats.reverse, body.reverse, defaultCase, doBinding)

      //_m.defs.addAll(m.defs);

      m.tree
    }

  } // class AlgebraicMatcher

}
