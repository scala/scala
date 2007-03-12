/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
//todo: allow infix type patterns


package scala.tools.nsc.ast.parser

import scala.tools.nsc.util.{ListBuffer, Position}
import symtab.Flags
import Tokens._

//todo verify when stableId's should be just plain qualified type ids

/** <p>Performs the following context-free rewritings:</p>
 *  <ol>
 *    <li>
 *      Places all pattern variables in Bind nodes. In a pattern, for
 *      identifiers <code>x</code>:<pre>
 *                 x  => x @ _
 *               x:T  => x @ (_ : T)</pre>
 *    </li>
 *    <li>Removes pattern definitions (PatDef's) as follows:
 *      If pattern is a simple (typed) identifier:<pre>
 *        <b>val</b> x = e     ==>  <b>val</b> x = e
 *        <b>val</b> x: T = e  ==>  <b>val</b> x: T = e</pre>
 *
 *      if there are no variables in pattern<pre>
 *        <b>val</b> p = e  ==>  e match (case p => ())</pre>
 *
 *      if there is exactly one variable in pattern<pre>
 *        <b>val</b> x_1 = e <b>match</b> (case p => (x_1))</pre>
 *
 *      if there is more than one variable in pattern<pre>
 *        <b>val</b> p = e  ==>  <b>private synthetic val</b> t$ = e <b>match</b> (case p => (x_1, ..., x_N))
 *                        <b>val</b> x_1 = t$._1
 *                        ...
 *                        <b>val</b> x_N = t$._N</pre>
 *    </li>
 *    <li>
 *       Removes function types as follows:<pre>
 *        (argtpes) => restpe   ==>   scala.Function_n[argtpes, restpe]</pre>
 *    </li>
 *    <li>
 *      Wraps naked case definitions in a match as follows:<pre>
 *        { cases }   ==>   (x => x.match {cases})<span style="font-family:normal;">, except when already argument to match</span></pre>
 *    </li>
 *  </ol>
 */
trait Parsers requires SyntaxAnalyzer {

  import global._
  import RequiresIntsAsPositions._
  private val glob: global.type = global
  import global.posAssigner.atPos

  class Parser(unit: global.CompilationUnit) {

    val in = new Scanner(unit)

    /** the markup parser */
    val xmlp = new MarkupParser(unit, in, Parser.this, true)

    object treeBuilder extends TreeBuilder {
      val global: Parsers.this.global.type = Parsers.this.global
      def freshName(prefix: String): Name = unit.fresh.newName(prefix)
    }
    import treeBuilder._

    object symbXMLBuilder extends SymbolicXMLBuilder(treeBuilder, Parser.this, true) { // DEBUG choices
      val global: Parsers.this.global.type = Parsers.this.global
      def freshName(prefix: String): Name = unit.fresh.newName(prefix)
    }

    /** The implicit view parameters of the surrounding class */
    var implicitClassViews: List[Tree] = Nil

    /** The implicit parameters introduced by `_' in the current expression.
     *  Parameters appear in reverse order
     */
    var implicitParams: List[ValDef] = Nil

    /** this is the general parse method
     */
    def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    }

/////// ERROR HANDLING //////////////////////////////////////////////////////

    private def skip(): unit = {
      //System.out.println("<skipping> " + in.token2string(in.token))//DEBUG
      var nparens = 0
      var nbraces = 0
      while (true) {
        in.token match {
          case EOF =>
            return
          case SEMI =>
            if (nparens == 0 && nbraces == 0) return
          case NEWLINE =>
            if (nparens == 0 && nbraces == 0) return
          case NEWLINES =>
            if (nparens == 0 && nbraces == 0) return
          case RPAREN =>
            nparens = nparens - 1
          case RBRACE =>
            if (nbraces == 0) return
            nbraces = nbraces - 1
          case LPAREN =>
            nparens = nparens + 1
          case LBRACE =>
            nbraces = nbraces + 1
          case _ =>
        }
        in.nextToken()
      }
    }

    def syntaxError(msg: String, skipIt: boolean): unit =
      syntaxError(in.currentPos, msg, skipIt)

    def syntaxError(pos: int, msg: String, skipIt: boolean): unit = {
      if (pos != in.errpos) {
        unit.error(pos, msg)
        in.errpos = pos
      }
      if (skipIt) {
        in.skipping = true
        skip()
        in.skipping = false
      }
    }

    def syntaxErrorMigrate(msg: String) =
      syntaxError(in.currentPos, migrateMsg + msg, false)

    def warning(msg: String) =
      if (in.currentPos != in.errpos) {
        unit.warning(in.currentPos, msg)
        in.errpos = in.currentPos
      }

    def incompleteInputError(pos: int, msg: String): unit = {
      if (pos != in.errpos) {
        unit.incompleteInputError(pos, msg)
        in.errpos = pos
      }
    }

    def incompleteInputError(msg: String): unit =
      incompleteInputError(in.currentPos, msg)  // in.currentPos should be at the EOF

    def syntaxErrorOrIncomplete(msg: String, skipIt: Boolean): unit = {
      if(in.token == EOF)
        incompleteInputError(msg)
      else
        syntaxError(in.currentPos, msg, skipIt)
    }

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      */
    def accept(token: int): int = {
      val pos = in.currentPos
      if (in.token != token) {
        val posToReport =
          if (Position.line(unit.source, in.currentPos) >
              Position.line(unit.source, in.lastPos))
            in.lastPos
          else
            in.currentPos
        val msg =
          in.token2string(token) + " expected but " +
          in.token2string(in.token) + " found."

        if(in.token == EOF)
          incompleteInputError(posToReport, msg)
        else
          syntaxError(posToReport, msg, true)
      }
      if (in.token == token) in.nextToken()
      pos
    }

    /** semi = nl {nl} | `;'
     *  nl  = `\n' // where allowed
     */
    def acceptStatSep(): unit =
      if (in.token == NEWLINE || in.token == NEWLINES) in.nextToken()
      else accept(SEMI)

    def errorTypeTree = TypeTree().setType(ErrorType).setPos(in.currentPos)
    def errorTermTree = Literal(Constant(null)).setPos(in.currentPos)
    def errorPatternTree = Ident(nme.WILDCARD).setPos(in.currentPos)

/////// TOKEN CLASSES //////////////////////////////////////////////////////

    def isModifier: boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | PRIVATE | PROTECTED | OVERRIDE | IMPLICIT => true
      case _ => false
    }

    def isLocalModifier: boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | IMPLICIT => true
      case _ => false
    }

    def isDefIntro: boolean = in.token match {
      case VAL | VAR | DEF | TYPE | OBJECT |
           CASEOBJECT | CLASS | CASECLASS | TRAIT => true
      case _ => false
    }

    def isDclIntro: boolean = in.token match {
      case VAL | VAR | DEF | TYPE => true
      case _ => false
    }

    def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT

    def isExprIntroToken(token: int): boolean = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
           STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL | IDENTIFIER | BACKQUOTED_IDENT |
           THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
           DO | RETURN | THROW | LPAREN | LBRACE | XMLSTART => true
      case _ => false
    }

    def isExprIntro: boolean = isExprIntroToken(in.token)

    def isTypeIntroToken(token: int): boolean = token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER | USCORE | LPAREN | AT => true
      case _ => false
    }

    def isTypeIntro: boolean = isTypeIntroToken(in.token)

    def isStatSep(token: int): boolean =
      token == NEWLINE || token == NEWLINES || token == SEMI

    def isStatSep: boolean = isStatSep(in.token)


/////// COMMENT AND ATTRIBUTE COLLECTION //////////////////////////////////////

    /** Join the comment associated with a definition
    */
    def joinComment(trees: => List[Tree]): List[Tree] = {
      val buf = in.docBuffer
      if (buf ne null) {
        in.docBuffer = null
        trees map (t => DocDef(buf.toString(), t) setPos t.pos)
      } else trees
    }

/////// TREE CONSTRUCTION ////////////////////////////////////////////////////

    /** Convert tree to formal parameter list
    */
    def convertToParams(tree: Tree): List[ValDef] = tree match {
      case Parens(ts) =>
        ts map convertToParam
      case _ =>
        List(convertToParam(tree))
    }

    /** Convert tree to formal parameter
    */
    def convertToParam(tree: Tree): ValDef =
      atPos(tree.pos) {
        tree match {
          case Ident(name) =>
            ValDef(Modifiers(Flags.PARAM), name, TypeTree(), EmptyTree)
          case Typed(Ident(name), tpe) =>
            ValDef(Modifiers(Flags.PARAM), name, tpe, EmptyTree)
          case _ =>
            syntaxError(tree.pos, "not a legal formal parameter", false)
            ValDef(Modifiers(Flags.PARAM), nme.ERROR, errorTypeTree, EmptyTree)
        }
      }

    /** Convert (qual)ident to type identifier
     */
    def convertToTypeId(tree: Tree): Tree = tree match {
      case Ident(name) =>
        Ident(name.toTypeName).setPos(tree.pos)
      case Select(qual, name) =>
        Select(qual, name.toTypeName).setPos(tree.pos)
      case _ =>
        syntaxError(tree.pos, "identifier expected", false)
        errorTypeTree
    }

    /** make closure from tree staring with a `.' */
    def makeDotClosure(tree: Tree): Tree = {
      val pname = unit.fresh.newName("x$")
      def insertParam(tree: Tree): Tree = atPos(tree.pos) {
        tree match {
          case Ident(name) =>
            Select(Ident(pname), name)
          case Select(qual, name) =>
            Select(insertParam(qual), name)
          case Apply(fn, args) =>
            Apply(insertParam(fn), args)
          case TypeApply(fn, args) =>
            TypeApply(insertParam(fn), args)
          case _ =>
            syntaxError(tree.pos, "cannot convert to closure", false)
            errorTermTree
        }
      }
      Function(List(makeSyntheticParam(pname)), insertParam(tree))
    }

/////// OPERAND/OPERATOR STACK /////////////////////////////////////////////////

    case class OpInfo(operand: Tree, operator: Name, pos: int)
    var opstack: List[OpInfo] = Nil

    def precedence(operator: Name): int =
      if (operator eq nme.ERROR) -1
      else {
        val firstCh = operator(0)
        if (((firstCh >= 'A') && (firstCh <= 'Z')) ||
            ((firstCh >= 'a') && (firstCh <= 'z')))
          1
        else
          firstCh match {
            case '|'             => 2
            case '^'             => 3
            case '&'             => 4
            case '=' | '!'       => 5
            case '<' | '>'       => 6
            case ':'             => 7
            case '+' | '-'       => 8
            case '*' | '/' | '%' => 9
            case _               => 10
          }
      }

    def checkSize(kind: String, size: int, max: int) {
      if (size > max) syntaxError("too many "+kind+", maximum = "+max, false)
    }

    def checkAssoc(pos: int, op: Name, leftAssoc: boolean) =
      if (treeInfo.isLeftAssoc(op) != leftAssoc)
        syntaxError(
          pos, "left- and right-associative operators with same precedence may not be mixed", false)

    def reduceStack(isExpr: boolean, base: List[OpInfo], top0: Tree, prec: int, leftAssoc: boolean): Tree = {
      var top = top0
      if (opstack != base && precedence(opstack.head.operator) == prec)
        checkAssoc(opstack.head.pos, opstack.head.operator, leftAssoc)
      while (opstack != base &&
             (prec < precedence(opstack.head.operator) ||
              (leftAssoc && prec == precedence(opstack.head.operator)))) {
        top = atPos(opstack.head.pos) {
          makeBinop(isExpr, opstack.head.operand, opstack.head.operator, top)
        }
        opstack = opstack.tail
      }
      top
    }

/////// IDENTIFIERS AND LITERALS ////////////////////////////////////////////////////////////

    final val MINUS: Name = "-"
    final val PLUS : Name = "+"
    final val BANG : Name = "!"
    final val TILDE: Name = "~"
    final val AMP  : Name = "&"
    final val SLASH: Name = "/"
    final val STAR : Name = "*"
    final val BAR  : Name = "|"
    final val OPT  : Name = "?"
    final val LT   : Name = "<"

    def ident(): Name =
      if (in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT) {
        val name = in.name.encode
        in.nextToken()
        name
      } else {
        if (settings.migrate.value && in.token == MATCH || in.token == REQUIRES || in.token == IMPLICIT)
          syntaxErrorMigrate(""+in+" is now a reserved word; cannot be used as identifier")
        accept(IDENTIFIER)
        nme.ERROR
      }

    def selector(t: Tree) = {
      if (in.token == MATCH && settings.migrate.value)
        syntaxErrorMigrate("Period should be omitted before `match'")
      Select(t, ident())
    }

    /** Path       ::= StableId
     *              |  [Ident `.'] this
     *  AnnotType ::= Path [`.' type]
     */
    def path(thisOK: boolean, typeOK: boolean): Tree = {
      var t: Tree = null
      if (in.token == THIS) {
        t = atPos(in.skipToken()) { This(nme.EMPTY.toTypeName) }
        if (!thisOK || in.token == DOT)
          t =  selectors(t, typeOK, accept(DOT))
      } else if (in.token == SUPER) {
        t = atPos(in.skipToken()) {
          Super(nme.EMPTY.toTypeName, mixinQualifierOpt())
        }
        t = atPos(accept(DOT)) { selector(t) }
        if (in.token == DOT)
          t = selectors(t, typeOK, in.skipToken())
      } else {
        val i = atPos(in.currentPos) {
          if (in.token == BACKQUOTED_IDENT) new BackQuotedIdent(ident())
          else Ident(ident())
        }
        t = i
        if (in.token == DOT) {
          val pos = in.skipToken()
          if (in.token == THIS) {
            in.nextToken()
            t = atPos(i.pos) { This(i.name.toTypeName) }
            if (!thisOK || in.token == DOT)
              t = selectors(t, typeOK, accept(DOT))
          } else if (in.token == SUPER) {
            in.nextToken()
            t = atPos(i.pos) { Super(i.name.toTypeName, mixinQualifierOpt()) }
            t = atPos(accept(DOT)) {selector(t)}
            if (in.token == DOT)
              t = selectors(t, typeOK, in.skipToken())
          } else {
            t = selectors(t, typeOK, pos)
          }
        }
      }
      t
    }

    def selectors(t: Tree, typeOK: boolean, pos : Int): Tree =
      if (typeOK && in.token == TYPE) {
        in.nextToken()
        atPos(pos) { SingletonTypeTree(t) }
      } else {
        val t1 = atPos(pos) { selector(t); }
        if (in.token == DOT) { selectors(t1, typeOK, in.skipToken()) }
        else t1
      }

    /** MixinQualifier ::= `[' Id `]'
    */
    def mixinQualifierOpt(): Name =
      if (in.token == LBRACKET) {
        in.nextToken()
        val name = ident().toTypeName
        accept(RBRACKET)
        name
      } else {
        nme.EMPTY.toTypeName
      }

    /** StableId ::= Id
     *            |  Path `.' Id
     *            |  [id '.'] super [`[' id `]']`.' id
     */
    def stableId(): Tree =
      path(false, false)

    /** QualId ::= Id {`.' Id}
    */
    def qualId(): Tree = {
      val id = atPos(in.currentPos) { Ident(ident()) }
      if (in.token == DOT) { selectors(id, false, in.skipToken()) }
      else id
    }

    /** SimpleExpr    ::= literal
    *                  | symbol [ArgumentExprs]
    *                  | null
    */
    def literal(isPattern: boolean, isNegated: boolean): Tree = {
      def litToTree() = atPos(in.currentPos) {
        Literal(
          in.token match {
            case CHARLIT =>
              Constant(in.intVal.asInstanceOf[char])
            case INTLIT =>
              Constant(in.intVal(isNegated).asInstanceOf[int])
            case LONGLIT =>
              Constant(in.intVal(isNegated))
            case FLOATLIT =>
              Constant(in.floatVal(isNegated).asInstanceOf[float])
            case DOUBLELIT =>
              Constant(in.floatVal(isNegated))
            case STRINGLIT | SYMBOLLIT =>
              Constant(in.name.toString())
            case TRUE =>
              Constant(true)
            case FALSE =>
              Constant(false)
            case NULL =>
              Constant(null)
            case _ =>
              syntaxErrorOrIncomplete("illegal literal", true)
              null
          })
      }

      val isSymLit = in.token == SYMBOLLIT
      val t = litToTree()
      val pos = in.skipToken()
      if (isSymLit) {
        atPos(pos) {
          var symid = scalaDot(nme.Symbol)
          if (isPattern) { symid = /*convertToTypeId*/(symid) }
          val symobj = Apply(symid, List(t))
          if (isPattern) symobj else Select(symobj, nme.intern)
        }
      } else {
        t
      }
    }

    def newLineOpt(): unit =
      if (in.token == NEWLINE) {
        if (settings.migrate.value) in.newNewLine = false
        in.nextToken()
      }

    def newLinesOpt(): unit =
      if (in.token == NEWLINE || in.token == NEWLINES) {
        if (settings.migrate.value) in.newNewLine = false
        in.nextToken()
      }

    def newLineOptWhenFollowedBy(token: int): unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && in.next.token == token) newLineOpt()
    }

    def newLineOptWhenFollowing(p: int => boolean): unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && p(in.next.token)) newLineOpt()
    }

//////// TYPES ///////////////////////////////////////////////////////////////

    /** TypedOpt ::= [`:' Type]
    */
    def typedOpt(): Tree =
      if (in.token == COLON) { in.nextToken(); typ() }
      else TypeTree()

    /** RequiresTypedOpt ::= [requires AnnotType]
    */
    def requiresTypeOpt(): Tree =
      if (in.token == COLON | in.token == REQUIRES) {
        if (in.token == COLON)
          warning("`:' has been deprecated; use `requires' instead")
        in.nextToken(); annotType(false)
      }
      else TypeTree()

    /** Types ::= Type {`,' Type}
     *  ArgTypePats ::=  ArgTypePat {`,' ArgTypePat}
     *
     *  (also eats trailing comma if it finds one)
     */
    def types(isPattern: boolean): List[Tree] = {
      val ts = new ListBuffer[Tree] + argType(isPattern)
      while (in.token == COMMA) {
        in.nextToken()
        if (in.token == RPAREN) return List(makeTupleType(ts.toList, false))
        ts += argType(isPattern)
      }
      ts.toList
    }

    /** modes for infix types */
    private final val FirstOp = 0   // first operand
    private final val LeftOp = 1    // left associative
    private final val RightOp = 2   // right associative

    /** Type ::= InfixType [`=>' Type]
     *         | `(' [`=>' Type] `)' `=>' Type
     */
    def typ(): Tree = {
      val t =
        if (in.token == LPAREN) {
          val pos = in.skipToken()
          if (in.token == RPAREN) {
            in.nextToken()
            atPos(accept(ARROW)) { makeFunctionTypeTree(List(), typ()) }
          } else if (in.token == ARROW) {
            in.nextToken()
            val t0 = typ()
            accept(RPAREN)
            atPos(accept(ARROW)) { makeByNameFunctionTypeTree(t0, typ()) }
          } else {
            val ts = types(false)
            accept(RPAREN)
            if (in.token == ARROW) atPos(in.skipToken()) { makeFunctionTypeTree(ts, typ()) }
            else infixTypeRest(pos, makeTupleType(ts, true), false, FirstOp)
          }
        } else {
          infixType(false, FirstOp)
        }
      if (in.token == ARROW) atPos(in.skipToken()) { makeFunctionTypeTree(List(t), typ()) }
      else t
    }

    /** InfixType ::= CompoundType {id [nl] CompoundType}
     *  TypePat ::= CompoundTypePat {id [nl] CompoundTypePat
     */
    def infixType(isPattern: boolean, mode: int): Tree =
      infixTypeRest(in.currentPos, annotType(isPattern), isPattern, mode)

    def infixTypeRest(pos: int, t0: Tree, isPattern: boolean, mode: int): Tree = {
      val t = compoundTypeRest(pos, t0, isPattern)
      if (isIdent && in.name != nme.STAR) {
        val opPos = in.currentPos
        val leftAssoc = treeInfo.isLeftAssoc(in.name)
        if (mode == LeftOp) checkAssoc(opPos, in.name, true)
        else if (mode == RightOp) checkAssoc(opPos, in.name, false)
        val op = ident()
        newLineOptWhenFollowing(isTypeIntroToken)
        def mkOp(t1: Tree) = atPos(opPos) { AppliedTypeTree(Ident(op.toTypeName), List(t, t1)) }
        if (leftAssoc)
          infixTypeRest(in.currentPos, mkOp(compoundType(isPattern)), isPattern, LeftOp)
        else
          mkOp(infixType(isPattern, RightOp))
      } else t
    }

    /** CompoundType ::= AnnotType {with AnnotType} [Refinement]
     *  CompoundTypePat ::= AnnotTypePat {with AnnotTypePat}
     */
    def compoundType(isPattern: boolean): Tree =
      compoundTypeRest(in.currentPos, annotType(isPattern), isPattern)

    def compoundTypeRest(pos: int, t: Tree, isPattern: boolean): Tree = {
      var ts = new ListBuffer[Tree] + t
      while (in.token == WITH) {
        in.nextToken(); ts += annotType(isPattern)
      }
      newLineOptWhenFollowedBy(LBRACE)
      atPos(pos) {
        if (in.token == LBRACE && !isPattern) CompoundTypeTree(Template(ts.toList, refinement()))
        else makeIntersectionTypeTree(ts.toList)
      }
    }

    /** AnnotType        ::=  Annotations SimpleType
     *  SimpleType       ::=  SimpleType TypeArgs
     *                     |   SimpleType `#' Id
     *                     |   StableId
     *                     |   Path `.' type
     *                     |   `(' Types [`,'] `)'
     * AnnotTypePat      ::=  Annotations SimpleTypePat
     * SimpleTypePat     ::=  SimpleTypePat1 [TypePatArgs]
     * SimpleTypePat1    ::=  SimpleTypePat1 "#" Id
     *                     |   StableId
     *                     |   Path `.' type
     *                     |   `(' ArgTypePats [`,'] `)'
     */
    def annotType(isPattern: boolean): Tree = {
      val annots = annotations()
      val pos = in.currentPos
      var t: Tree =
        if (in.token == LPAREN) {
          in.nextToken()
          val ts = types(isPattern)
          accept(RPAREN)
          atPos(pos) { makeTupleType(ts, true) }
        } else {
          val r = path(false, true)
          val x = r match {
            case SingletonTypeTree(_) => r
            case _ => convertToTypeId(r)
          }
          // System.err.println("SIMPLE_TYPE: " + r.pos + " " + r + " => " + x.pos + " " + x)
          x
        }

      // scan for # and []
      var done = false
      while (!done) {
        if (in.token == HASH) {
          t = atPos(in.skipToken()) {
            SelectFromTypeTree(t, ident().toTypeName)
          }
        } else if (in.token == LBRACKET) {
          t = atPos(pos) { AppliedTypeTree(t, typeArgs(isPattern)) }
          if (isPattern) done=true
        } else
          done=true
      }
      (t /: annots) (makeAnnotated)
    }

    /** TypeArgs    ::= `[' ArgType {`,' ArgType} `]'
     *  TypePatArgs ::= '[' ArgTypePat {`,' ArgTypePat} `]'
     */
    def typeArgs(isPattern: boolean): List[Tree] = {
      accept(LBRACKET)
      val ts = types(isPattern)
      accept(RBRACKET)
      ts
    }

    /** ArgType       ::=  Type
     *  ArgTypePat    ::=  varid
     *                  |  `_'
     *                  |  Type            // for array elements only!
     */
    def argType(isPattern: boolean): Tree =
      if (isPattern) {
        if (in.token == USCORE)
          atPos(in.skipToken()) { Bind(nme.WILDCARD.toTypeName, EmptyTree) }
        else if (in.token == IDENTIFIER && treeInfo.isVariableName(in.name.toTypeName))
          atPos(in.currentPos) { Bind(ident().toTypeName, EmptyTree) }
        else {
          typ()
        }
      } else typ()

//////// EXPRESSIONS ////////////////////////////////////////////////////////

/*
    // XX_LIFTED
    var liftedGenerators = new collection.mutable.ListBuffer[ValFrom]

    // XX_LIFTED
    def wrapLiftedGenerators(t: Tree): Tree =
      if (liftedGenerators.isEmpty) t
      else {
        val t1 = makeLifted(liftedGenerators.toList, t)
        liftedGenerators.clear
        t1
      }

    // XX_LIFTED
    def noLifting(op: => Tree): Tree = {
      val savedLiftedGenerators = liftedGenerators
      if (!savedLiftedGenerators.isEmpty) // optimization to avoid buffer allocation
        liftedGenerators = new collection.mutable.ListBuffer
      val t = op
      if (!liftedGenerators.isEmpty)
        syntaxError(liftedGenerators.toList.head.pos, "no lifted expression allowed here", false)
      liftedGenerators = savedLiftedGenerators
      t
    }

    // XX_LIFTED
    def liftingScope(op: => Tree): Tree = {
      val savedLiftedGenerators = liftedGenerators
      if (!savedLiftedGenerators.isEmpty) // optimization to avoid buffer allocation
        liftedGenerators = new collection.mutable.ListBuffer
      val t = wrapLiftedGenerators(op)
      liftedGenerators = savedLiftedGenerators
      t
    }
*/
    /** EqualsExpr ::= `=' Expr
     */
    def equalsExpr(): Tree = {
      accept(EQUALS)
      expr()
    }

    /** Exprs ::= Expr {`,' Expr}
     *
     *  (also eats trailing comma if it finds one)
     */
    def exprs(): List[Tree] = {
      val savedImplicitParams = implicitParams
      implicitParams = List()
      var first = expr()
      if (!implicitParams.isEmpty) {
        first = Function(implicitParams.reverse, first)
        implicitParams = List()
      }
      val ts = new ListBuffer[Tree] + first
      while (in.token == COMMA) {
        in.nextToken();
        if (in.token == RPAREN) return List(makeTupleTerm(ts.toList, false))
        ts += expr()
        if (!implicitParams.isEmpty) {
          syntaxError(implicitParams.head.pos, "section outside (...)", false)
        }
      }
      implicitParams = savedImplicitParams
      ts.toList
    }

    private final val Local = 0
    private final val InBlock = 1
    private final val InTemplate = 2

    /** Expr       ::= (Bindings | Id)  `=>' Expr
     *               | PostfixExpr `:' Type
     *               | Expr1
     *  ResultExpr ::= (Bindings | Id `:' CompoundType) `=>' Block
     *               | Expr1
     *  Expr1      ::= if `(' Expr `)' {nl} Expr [semi] else Expr]
     *               | try `{' block `}' [catch `{' caseClauses `}'] [finally Expr]
     *               | while `(' Expr `)' {nl} Expr
     *               | do Expr [semi] while `(' Expr `)'
     *               | for (`(' Enumerators `)' | '{' Enumerators '}') {nl} [yield] Expr
     *               | throw Expr
     *               | return [Expr]
     *               | [SimpleExpr `.'] Id `=' Expr
     *               | SimpleExpr1 ArgumentExprs `=' Expr
     *               | `.' SimpleExpr
     *               | PostfixExpr Ascription
     *               | PostfixExpr match `{' CaseClauses `}'
     *               | `.' Id {`.' Id | TypeArgs | ArgumentExprs}
     *  Bindings   ::= `(' [Binding {`,' Binding}] `)'
     *  Binding    ::= Id [`:' Type]
     *  Ascription ::= `:' CompoundType
     *               | `:' Annotation {Annotation}
     *               | `:' `_' `*'
     */
    def expr(): Tree = expr(Local)
    def expr(location: int): Tree = in.token match {
      case IF =>
        val pos = in.skipToken()
        accept(LPAREN)
        val cond = expr()
        accept(RPAREN)
        newLinesOpt()
        val thenp = expr()
        val elsep =
          if (in.token == ELSE) { in.nextToken(); expr() }
          else EmptyTree
        atPos(pos) { If(cond, thenp, elsep) }
      case TRY =>
        atPos(in.skipToken()) {
          accept(LBRACE)
          val body = block()
          accept(RBRACE)
          val catches =
            if (in.token == CATCH) {
              in.nextToken()
              accept(LBRACE)
              val cases = caseClauses()
              accept(RBRACE)
              cases
            } else List()
          val finalizer =
            if (in.token == FINALLY) { in.nextToken(); expr() }
            else EmptyTree
          Try(body, catches, finalizer)
        }
      case WHILE =>
        val lname: Name = unit.fresh.newName("while$")
        val pos = in.skipToken()
        accept(LPAREN)
        val cond = expr()
        accept(RPAREN)
        newLinesOpt()
        val body = expr()
        atPos(pos) { makeWhile(lname, cond, body) }
      case DO =>
        val lname: Name = unit.fresh.newName("doWhile$")
        val pos = in.skipToken()
        val body = expr()
        if (isStatSep) in.nextToken()
        accept(WHILE)
        accept(LPAREN)
        val cond = expr()
        accept(RPAREN)
        atPos(pos) { makeDoWhile(lname, body, cond) }
      case FOR =>
        atPos(in.skipToken()) {
          val startToken = in.token
          accept(if (startToken == LBRACE) LBRACE else LPAREN)
          val enums = enumerators()
          accept(if (startToken == LBRACE) RBRACE else RPAREN)
          newLinesOpt()
          if (in.token == YIELD) {
            in.nextToken(); makeForYield(enums, expr())
          } else makeFor(enums, expr())
        }
      case RETURN =>
        atPos(in.skipToken()) {
          Return(if (isExprIntro) expr() else Literal(()))
        }
      case THROW =>
        atPos(in.skipToken()) {
          Throw(expr())
        }
      case DOT =>
        atPos(in.skipToken()) {
          if (isIdent) {
            makeDotClosure(stripParens(simpleExpr()))
          } else {
            syntaxErrorOrIncomplete("identifier expected", true)
            errorTermTree
          }
        }
      case _ =>
        var t = postfixExpr()
        if (in.token == EQUALS) {
          t match {
            case Ident(_) | Select(_, _) | Apply(_, _) =>
              t = atPos(in.skipToken()) { makeAssign(t, expr()) }
            case _ =>
          }
        } else if (in.token == COLON) {
          t = stripParens(t)
          val pos = in.skipToken()
          val annots = annotations()
          if (in.token == USCORE) {
            val pos1 = in.skipToken()
            if (isIdent && in.name == nme.STAR) {
              in.nextToken()
              t = atPos(pos) {
                Typed(t, atPos(pos1) { Ident(nme.WILDCARD_STAR.toTypeName) })
              }
            } else {
              syntaxErrorOrIncomplete("`*' expected", true)
            }
          } else if (annots.isEmpty || isTypeIntro) {
            t = atPos(pos) {
              val tpt = if (location != Local) compoundType(false) else typ()
              // this does not correspond to syntax, but is necessary to
              // accept closures. We might restrict closures to be between {...} only!
              Typed(t, (tpt /: annots) (makeAnnotated))
            }
          } else {
            t = (t /: annots) (makeAnnotated)
          }
        } else if (in.token == MATCH) {
          t = atPos(in.skipToken()) {
            accept(LBRACE)
            val cases = caseClauses()
            accept(RBRACE)
            Match(stripParens(t), cases)
          }
        }
        if (in.token == ARROW && location != InTemplate) {
          t = atPos(in.skipToken()) {
            Function(convertToParams(t), if (location == Local) expr() else block())
          }
        }
        stripParens(t)
    }

    /** PostfixExpr   ::= InfixExpr [Id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr Id [nl] InfixExpr
     */
    def postfixExpr(): Tree = {
      val base = opstack
      var top = prefixExpr()
      while (isIdent) {
        top = reduceStack(
          true, base, top, precedence(in.name), treeInfo.isLeftAssoc(in.name))
        val op = in.name
        opstack = OpInfo(top, op, in.currentPos) :: opstack
        ident()
        newLineOptWhenFollowing(isExprIntroToken)
        if (isExprIntro) {
          top = prefixExpr()
        } else {
          val topinfo = opstack.head
          opstack = opstack.tail
          return Select(
            stripParens(reduceStack(true, base, topinfo.operand, 0, true)),
            topinfo.operator.encode).setPos(topinfo.pos)
        }
      }
      reduceStack(true, base, top, 0, true)
    }

    /** PrefixExpr   ::= [`-' | `+' | `~' | `!' | `&'] SimpleExpr
    */
    def prefixExpr(): Tree = {
      def unaryOp(): Name = "unary_" + ident()
      if (isIdent && in.name == MINUS) {
        val name = unaryOp()
        in.token match {
          case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT => literal(false, true)
          case _ => atPos(in.currentPos) { Select(stripParens(simpleExpr()), name) }
        }
      } else if (isIdent && (in.name == PLUS || in.name == TILDE || in.name == BANG)) {
        val pos = in.currentPos
        val name = unaryOp()
        atPos(pos) { Select(stripParens(simpleExpr()), name) }
      } else if (isIdent && in.name == AMP) {
        val pos = in.currentPos
        val name = ident()
        atPos(pos) { Typed(stripParens(simpleExpr()), Function(List(), EmptyTree)) }
/* XX-LIFTING
      } else if (settings.Xexperimental.value && isIdent && in.name == SLASH) {
        val pos = in.skipToken()
        val name = freshName()
        liftedGenerators += ValFrom(pos, Bind(name, Ident(nme.WILDCARD)), simpleExpr())
        Ident(name) setPos pos
*/
      } else {
        simpleExpr()
      }
    }

    /* SimpleExpr    ::= new AnnotType {`(' [Exprs [`,']] `)'} {`with' AnnotType} [TemplateBody]
     *                |  BlockExpr
     *                |  SimpleExpr1
     * SimpleExpr1   ::= literal
     *                |  xLiteral
     *                |  Path
     *                |  `(' [Exprs [`,']] `)'
     *                |  SimpleExpr `.' Id
     *                |  SimpleExpr TypeArgs
     *                |  SimpleExpr1 ArgumentExprs
     */
    def simpleExpr(): Tree = {
      var t: Tree = null
      var canApply = true
      in.token match {
        case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT |
             SYMBOLLIT | TRUE | FALSE | NULL =>
          t = literal(false, false)
        case XMLSTART =>
          t = xmlp.xLiteral
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          t = path(true, false)
        case USCORE =>
          val pname = unit.fresh.newName("x$")
          val pos = in.skipToken()
          val param = makeSyntheticParam(pname) setPos pos
          implicitParams = param :: implicitParams
          t = atPos(pos) { Ident(pname) }
        case LPAREN =>
          val pos = in.skipToken()
          val ts = if (in.token == RPAREN) List() else exprs()
          accept(RPAREN)
          t = Parens(ts) setPos pos
        case LBRACE =>
          t = blockExpr()
          canApply = false
        case NEW =>
          t = atPos(in.skipToken()) {
            val parents = new ListBuffer[Tree] + annotType(false)
            val argss = new ListBuffer[List[Tree]]
            if (in.token == LPAREN)
              do { argss += argumentExprs() } while (in.token == LPAREN)
            else argss += List()
            while (in.token == WITH) {
              in.nextToken()
              parents += annotType(false)
            }
            newLineOptWhenFollowedBy(LBRACE)
            val (self, stats) = if (in.token == LBRACE) templateBody()
                                    else (emptyValDef, List())
            makeNew(parents.toList, self, stats, argss.toList)
          }
          canApply = false
        case _ =>
          if (settings.migrate.value) {
            if (in.token == MATCH)
              syntaxErrorMigrate("`match' must be preceded by a selector expression")
            else if (in.token == REQUIRES || in.token == IMPLICIT)
              syntaxErrorMigrate(""+in+" is now a reserved word; cannot be used as identifier")
          }
          syntaxErrorOrIncomplete("illegal start of simple expression", true)
          t = errorTermTree
      }
      simpleExprRest(t, canApply)
    }

    def simpleExprRest(t: Tree, canApply: boolean): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case DOT =>
          simpleExprRest(atPos(in.skipToken()) { selector(stripParens(t)) }, true)
        case LBRACKET =>
          t match {
            case Ident(_) | Select(_, _) =>
              simpleExprRest(atPos(in.currentPos) { TypeApply(t, typeArgs(false)) }, true)
            case _ =>
              t
          }
        case LPAREN | LBRACE if (canApply) =>
          simpleExprRest(atPos(in.currentPos) { Apply(stripParens(t), argumentExprs()) }, true)
        case _ =>
          t
      }
    }

    /** ArgumentExprs ::= `(' [Exprs [`,']] `)'
      *                 | [nl] BlockExpr
     */
    def argumentExprs(): List[Tree] = {
      if (in.token == LBRACE) {
        List(blockExpr())
      } else {
        accept(LPAREN)
        val ts = if (in.token == RPAREN) List() else exprs()
        accept(RPAREN)
        ts
      }
    }

    /** BlockExpr ::= `{' (CaseClauses | Block) `}'
     */
    def blockExpr(): Tree = {
      val res = atPos(accept(LBRACE)) {
        if (in.token == CASE) makeVisitor(caseClauses(), true)
        else block()
      }
      accept(RBRACE)
      res
    }

    /** Block ::= BlockStatSeq
     */
    def block(): Tree =
      makeBlock(blockStatSeq(new ListBuffer[Tree]))

   /** CaseClauses ::= CaseClause {CaseClause}
    */
    def caseClauses(): List[CaseDef] = {
      val ts = new ListBuffer[CaseDef]
      do { ts += caseClause()
      } while (in.token == CASE)
      ts.toList
    }

    /** CaseClause : =>= case Pattern [if PostfixExpr] `=>' Block
     */
    def caseClause(): CaseDef =
      atPos(accept(CASE)) {
        val pat = pattern()
        val guard =
          if (in.token == IF) { in.nextToken(); stripParens(postfixExpr()) }
          else EmptyTree
        makeCaseDef(pat, guard, atPos(accept(ARROW))(block()))
      }

    /** Enumerators ::= Generator {semi Enumerator}
     *  Enumerator  ::= Generator
     *                | val Pattern1 `=' Expr
     *                | Expr
     */
    def enumerators(): List[Enumerator] = {
      val enums = new ListBuffer[Enumerator] + generator(false)
      while (isStatSep) {
        in.nextToken()
        enums += (if (in.token == VAL) generator(true) else Filter(expr()))
      }
      enums.toList
    }

    /** Generator ::= val Pattern1 `<-' Expr
     */
    def generator(eqOK: boolean): Enumerator = {
      val pos = accept(VAL)
      val pat = pattern1(false)
      val tok = in.token
      if (tok == EQUALS && eqOK) in.nextToken()
      else accept(LARROW)
      makeGenerator(pos, pat, tok == EQUALS, expr)
    }

//////// PATTERNS ////////////////////////////////////////////////////////////

    /**   Patterns ::= Pattern { `,' Pattern }
     *    SeqPatterns ::= SeqPattern { `,' SeqPattern }
     *
     *  (also eats trailing comma if it finds one)
     */
    def patterns(seqOK: boolean): List[Tree] = {
      val ts = new ListBuffer[Tree] + pattern(seqOK)
      while (in.token == COMMA) {
        in.nextToken();
        if (in.token == RPAREN) return List(makeTupleTerm(ts.toList, false))
        ts += pattern(seqOK)
      }
      ts.toList
    }

    /**   Pattern  ::=  Pattern1 { `|' Pattern1 }
     *    SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
     */
    def pattern(seqOK: boolean): Tree = {
      val pos = in.currentPos
      val t = pattern1(seqOK)
      if (isIdent && in.name == BAR) {
        val ts = new ListBuffer[Tree] + t
        while (isIdent && in.name == BAR) {
          in.nextToken(); ts += pattern1(seqOK)
        }
        atPos(pos) { makeAlternative(ts.toList) }
      } else t
    }

    def pattern(): Tree = pattern(false)

    /**   Pattern1    ::= varid `:' TypePat
     *                 |  `_' `:' TypePat
     *                 |  Pattern2
     *    SeqPattern1 ::= varid `:' TypePat
     *                 |  `_' `:' TypePat
     *                 |  [SeqPattern2]
     */
    def pattern1(seqOK: boolean): Tree = {
      //if (false && /*disabled, no regexp matching*/ seqOK && !isExprIntro) {
        //atPos(in.currentPos) { Sequence(List()) }
      //} else {
        val p = pattern2(seqOK)
        p match {
          case Ident(name) if (treeInfo.isVarPattern(p) && in.token == COLON) =>
            atPos(in.skipToken()) { Typed(p, compoundType(true)) }
          case _ =>
            p
        }
      //}
    }

    /*   Pattern2    ::=  varid [ @ Pattern3 ]
     *                |   Pattern3
     *   SeqPattern2 ::=  varid [ @ SeqPattern3 ]
     *                |   SeqPattern3
     */
    def pattern2(seqOK: boolean): Tree = {
      val p = pattern3(seqOK)
      if (in.token == AT) {
        p match {
          case Ident(name) =>
            if (name == nme.WILDCARD) {
              in.nextToken(); pattern3(seqOK)
            } else if (treeInfo.isVarPattern(p)) {
              in.nextToken()
              atPos(p.pos) { Bind(name, pattern3(seqOK)) }
            } else {
              p
            }
          case _ =>
            p
        }
      } else p
    }

    /*   Pattern3    ::= SimplePattern
     *                |  SimplePattern {Id [nl] SimplePattern}
     *   SeqPattern3 ::= SeqSimplePattern [ '*' | '?' | '+' ]
     *                |  SeqSimplePattern {Id [nl] SeqSimplePattern}
     */
    def pattern3(seqOK: boolean): Tree = {
      val base = opstack
      var top = simplePattern(seqOK)
      if (seqOK && isIdent) {
        if (in.name == STAR)
          return atPos(in.skipToken())(Star(top))
        else if (in.name == PLUS)
          return atPos(in.skipToken())(makePlus(top))
        else if (in.name == OPT)
          return atPos(in.skipToken())(makeOpt(top))
      }
      while (isIdent && in.name != BAR) {
        top = reduceStack(
          false, base, top, precedence(in.name), treeInfo.isLeftAssoc(in.name))
        val op = in.name
        opstack = OpInfo(top, op, in.currentPos) :: opstack
        ident()
        top = simplePattern(seqOK)
      }
      stripParens(reduceStack(false, base, top, 0, true))
    }

    /** SimplePattern    ::= varid
     *                    |  `_'
     *                    |  literal
     *                    |  XmlPattern
     *                    |  StableId  [TypePatArgs] [`(' [SeqPatterns [`,']] `)']
     *                    |  `(' [Patterns [`,']] `)'
     *  SimpleSeqPattern ::= varid
     *                    |  `_'
     *                    |  literal
     *                    |  XmlPattern
     *                    |  `<' xLiteralPattern
     *                    |  StableId [TypePatArgs] [`(' [SeqPatterns [`,']] `)']
     *                    |  `(' [SeqPatterns [`,']] `)'
     */
    def simplePattern(seqOK: boolean): Tree = in.token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
        var t = stableId()
        in.token match {
          case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT =>
            t match {
              case Ident(name) if name == nme.MINUS =>
                return literal(true, true)
              case _ =>
            }
          case _ =>
        }
/* not yet
        if (in.token == LBRACKET)
          atPos(in.currentPos) {
            val ts = typeArgs(true)
            accept(LPAREN)
            val ps = if (in.token == RPAREN) List() else patterns(true, false)
            accept(RPAREN)
            Apply(TypeApply(convertToTypeId(t), ts), ps)
          }
        else */
        if (in.token == LPAREN) {
          atPos(in.currentPos) { Apply(t, argumentPatterns()) }
        } else t
      case USCORE =>
        atPos(in.skipToken()) { Ident(nme.WILDCARD) }
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL =>
        literal(true, false)
      case LPAREN =>
        val pos = in.skipToken()
        val ps = if (in.token == RPAREN) List() else patterns(false)
        accept(RPAREN)
        Parens(ps) setPos pos
      case XMLSTART =>
        xmlp.xLiteralPattern
      case _ =>
        if (settings.migrate.value &&
            in.token == MATCH || in.token == REQUIRES || in.token == IMPLICIT)
          syntaxErrorMigrate(""+in+" is now a reserved word; cannot be used as identifier")
        syntaxErrorOrIncomplete("illegal start of simple pattern", true)
        errorPatternTree
    }

    def argumentPatterns(): List[Tree] = {
      accept(LPAREN)
      val ps = if (in.token == RPAREN) List() else patterns(true)
      accept(RPAREN)
      ps
    }

////////// MODIFIERS and ANNOTATIONS /////////////////////////////////////////////////

    private def normalize(mods: Modifiers): Modifiers =
      if ((mods hasFlag Flags.PRIVATE) && mods.privateWithin != nme.EMPTY.toTypeName)
        mods &~ Flags.PRIVATE
      else if ((mods hasFlag Flags.ABSTRACT) && (mods hasFlag Flags.OVERRIDE))
        mods &~ (Flags.ABSTRACT | Flags.OVERRIDE) | Flags.ABSOVERRIDE
      else
        mods

    private def addMod(mods: Modifiers, mod: int): Modifiers = {
      if (mods hasFlag mod) syntaxError(in.currentPos, "repeated modifier", false)
      in.nextToken()
      mods | mod
    }

    /** AccessQualifier ::= "[" (Id | this) "]"
     */
    def accessQualifierOpt(mods: Modifiers) = {
      var result = mods
      if (in.token == LBRACKET) {
        in.nextToken()
        if (mods.privateWithin != nme.EMPTY.toTypeName)
          syntaxError("duplicate private/protected qualifier", false)
        result = if (in.token == THIS) { in.nextToken(); mods | Flags.LOCAL }
                 else Modifiers(mods.flags, ident().toTypeName)
        accept(RBRACKET)
      }
      result
    }

    /** AccessModifier ::= (private | protected) [AccessQualifier]
     */
    def accessModifierOpt(): Modifiers = normalize {
      in.token match {
        case PRIVATE => in.nextToken(); accessQualifierOpt(Modifiers(Flags.PRIVATE))
        case PROTECTED => in.nextToken(); accessQualifierOpt(Modifiers(Flags.PROTECTED))
        case _ => NoMods
      }
    }

    /** Modifiers ::= {Modifier}
     *  Modifier  ::= LocalModifier
     *             |  AccessModifier
     *             |  override
     */
    def modifiers(): Modifiers = normalize {
      def loop(mods: Modifiers): Modifiers = in.token match {
        case ABSTRACT =>
          loop(addMod(mods, Flags.ABSTRACT))
        case FINAL =>
          loop(addMod(mods, Flags.FINAL))
        case SEALED =>
          loop(addMod(mods, Flags.SEALED))
        case PRIVATE =>
          loop(accessQualifierOpt(addMod(mods, Flags.PRIVATE)))
        case PROTECTED =>
          loop(accessQualifierOpt(addMod(mods, Flags.PROTECTED)))
        case OVERRIDE =>
          loop(addMod(mods, Flags.OVERRIDE))
        case IMPLICIT =>
          loop(addMod(mods, Flags.IMPLICIT))
        case _ =>
          mods
      }
      loop(NoMods)
    }

    /** LocalModifiers ::= {LocalModifier}
     *  LocalModifier  ::= abstract | final | sealed | implicit
     */
    def localModifiers(): Modifiers = {
      def loop(mods: Modifiers): Modifiers = in.token match {
        case ABSTRACT =>
          loop(addMod(mods, Flags.ABSTRACT))
        case FINAL =>
          loop(addMod(mods, Flags.FINAL))
        case SEALED =>
          loop(addMod(mods, Flags.SEALED))
        case IMPLICIT =>
          loop(addMod(mods, Flags.IMPLICIT))
        case _ =>
          mods
      }
      loop(NoMods)
    }

    /** Annotations   ::= {Annotation}
     *  Annotation    ::= `@' AnnotationExpr [nl]
     */
    def annotations(): List[Annotation] = {
      var annots = new ListBuffer[Annotation]
      if (in.token == LBRACKET) {
        unit.deprecationWarning(in.pos, "The [attribute] syntax has been deprecated; use @annotation instead")
        while (in.token == LBRACKET) {
          in.nextToken()
          annots += annotation()
          while (in.token == COMMA) {
            in.nextToken()
            annots += annotation()
          }
          accept(RBRACKET)
          newLineOpt()
        }
      } else {
        while (in.token == AT) {
          in.nextToken()
          annots += annotation()
          newLineOpt()
        }
      }
      annots.toList
    }

    /** TypeAttributes     ::= {`[' Exprs `]'}
     *
     * Type attributes may be arbitrary expressions.
     */
    def typeAttributes(): List[Tree] = {
      val exps = new ListBuffer[Tree]
      if (settings.Xplugtypes.value) {
        while(in.token == LBRACKET) {
          accept(LBRACKET)
          exps ++= exprs()
          accept(RBRACKET)
        }
      }
      exps.toList
    }

    /** Annotation ::= StableId [TypeArgs] [`(' [Exprs] `)'] [[nl] `{' {NameValuePair} `}']
     *  NameValuePair ::= val id `=' PrefixExpr
     */
    def annotation(): Annotation = {
      def nameValuePair(): Tree = {
        accept(VAL)
        var pos = in.currentPos
        val aname = atPos(pos) { Ident(ident()) }
        accept(EQUALS)
        atPos(pos) { Assign(aname, stripParens(prefixExpr())) }
      }
      val pos = in.currentPos
      var t: Tree = convertToTypeId(stableId())
      if (in.token == LBRACKET)
        t = atPos(in.currentPos)(AppliedTypeTree(t, typeArgs(false)))
      val args = if (in.token == LPAREN) argumentExprs() else List()
      newLineOptWhenFollowedBy(LBRACE)
      val nameValuePairs: List[Tree] = if (in.token == LBRACE) {
        in.nextToken()
        val nvps = new ListBuffer[Tree] + nameValuePair()
        while (in.token == COMMA) {
          in.nextToken()
          nvps += nameValuePair()
        }
        accept(RBRACE)
        nvps.toList
      } else List()
      val constr = atPos(pos) { New(t, List(args)) }
      Annotation(constr, nameValuePairs) setPos pos
    }

//////// PARAMETERS //////////////////////////////////////////////////////////

    /** ParamClauses      ::= {ParamClause} [[nl] `(' implicit Params `)']
     *  ParamClause       ::= [nl] `(' [Params] ')'
     *  Params            ::= Param {`,' Param}
     *  Param             ::= Annotations Id [`:' ParamType]
     *  ClassParamClauses ::= {ClassParamClause} [[nl] `(' implicit ClassParams `)']
     *  ClassParamClause  ::= [nl] `(' [ClassParams] ')'
     *  ClassParams       ::= ClassParam {`,' ClassParam}
     *  ClassParam        ::= Annotations  [{Modifier} (`val' | `var')] Id [`:' ParamType]
     */
    def paramClauses(owner: Name, implicitViews: List[Tree], ofCaseClass: boolean): List[List[ValDef]] = {
      var implicitmod = 0
      var caseParam = ofCaseClass
      def param(): ValDef = {
        atPos(in.currentPos) {
          val annots = annotations()
          var mods = Modifiers(Flags.PARAM)
          if (owner.isTypeName) {
            mods = modifiers() | Flags.PARAMACCESSOR
            if (in.token == VAL) {
              in.nextToken()
            } else if (in.token == VAR) {
              mods = mods | Flags.MUTABLE
              in.nextToken()
            } else {
              if (mods.flags != Flags.PARAMACCESSOR) accept(VAL)
              if (!(caseParam)) mods = mods | Flags.PRIVATE | Flags.LOCAL
            }
            if (caseParam) mods = mods | Flags.CASEACCESSOR
          }
          val name = ident()
          var bynamemod = 0
          val tpt =
            if (settings.Xexperimental.value && !owner.isTypeName && in.token != COLON) {
              TypeTree()
            } else { // XX-METHOD-INFER
              accept(COLON)
              if (in.token == ARROW) {
                if (owner.isTypeName && !mods.hasFlag(Flags.LOCAL))
                  syntaxError(
                    in.currentPos,
                    (if (mods.hasFlag(Flags.MUTABLE)) "`var'" else "`val'") +
                    " parameters may not be call-by-name", false)
                else bynamemod = Flags.BYNAMEPARAM
              }
              paramType()
            }
          ValDef((mods | implicitmod | bynamemod) withAnnotations annots, name, tpt, EmptyTree)
        }
      }
      def paramClause(): List[ValDef] = {
        val params = new ListBuffer[ValDef]
        if (in.token != RPAREN) {
          if (in.token == IMPLICIT) {
            if (!implicitViews.isEmpty)
              syntaxError("cannot have both view bounds `<%' and implicit parameters", false)
            in.nextToken()
            implicitmod = Flags.IMPLICIT
          }
          params += param()
          while (in.token == COMMA) {
            in.nextToken(); params += param()
          }
        }
        params.toList
      }
      val vds = new ListBuffer[List[ValDef]]
      val pos = in.currentPos
      newLineOptWhenFollowedBy(LPAREN)
      while (implicitmod == 0 && in.token == LPAREN) {
        in.nextToken()
        vds += paramClause()
        accept(RPAREN)
        caseParam = false
        newLineOptWhenFollowedBy(LPAREN)
      }
      val result = vds.toList
      if (owner == nme.CONSTRUCTOR &&
          (result.isEmpty ||
           (!result.head.isEmpty && result.head.head.mods.hasFlag(Flags.IMPLICIT))))
        if (in.token == LBRACKET)
          syntaxError(pos, "no type parameters allowed here", false)
        else if(in.token == EOF)
          incompleteInputError(pos, "auxiliary constructor needs non-implicit parameter list")
        else
          syntaxError(pos, "auxiliary constructor needs non-implicit parameter list", false)
      addImplicitViews(owner, result, implicitViews)
    }

    /** ParamType ::= Type | `=>' Type | Type `*'
     */
    def paramType(): Tree =
      if (in.token == ARROW)
        atPos(in.skipToken()) {
          AppliedTypeTree(
              scalaDot(nme.BYNAME_PARAM_CLASS_NAME.toTypeName), List(typ()))
        }
      else {
        val t = typ()
        if (isIdent && in.name == STAR) {
          in.nextToken()
          atPos(t.pos) {
            AppliedTypeTree(
              scalaDot(nme.REPEATED_PARAM_CLASS_NAME.toTypeName), List(t))
          }
        } else t
      }

    /** TypeParamClauseOpt    ::= [TypeParamClause]
     *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
     *  VariantTypeParam      ::= [`+' | `-'] TypeParam
     *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
     *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
     *  TypeParam             ::= Id TypeBounds [<% Type]
     */
    def typeParamClauseOpt(owner: Name, implicitViewBuf: ListBuffer[Tree]): List[AbsTypeDef] = {
      def typeParam(): AbsTypeDef = {
        var mods = Modifiers(Flags.PARAM)
        if (owner.isTypeName && isIdent) {
          if (in.name == PLUS) {
            in.nextToken()
            mods = mods | Flags.COVARIANT
          } else if (in.name == MINUS) {
            in.nextToken()
            mods = mods | Flags.CONTRAVARIANT
          }
        }
        val pos = in.currentPos
        val pname = ident()
        val param = atPos(pos) { typeBounds(mods, pname) }
        if (in.token == VIEWBOUND && (implicitViewBuf ne null))
          implicitViewBuf += atPos(in.skipToken()) {
            makeFunctionTypeTree(List(Ident(pname.toTypeName)), typ())
          }
        param
      }
      val params = new ListBuffer[AbsTypeDef]
      newLineOptWhenFollowedBy(LBRACKET)
      if (in.token == LBRACKET) {
        in.nextToken()
        params += typeParam()
        while (in.token == COMMA) {
          in.nextToken()
          params += typeParam()
        }
        accept(RBRACKET)
      }
      params.toList
    }

    /** TypeBounds ::= [`>:' Type] [`<:' Type]
     */
    def typeBounds(mods: Modifiers, name: Name): AbsTypeDef =
      AbsTypeDef(mods, name.toTypeName,
                 bound(SUPERTYPE, nme.Nothing),
                 bound(SUBTYPE, nme.Any))

    def bound(tok: int, default: Name): Tree =
      if (in.token == tok) { in.nextToken(); typ() }
      else scalaDot(default.toTypeName)

//////// DEFS ////////////////////////////////////////////////////////////////


    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    def importClause(): List[Tree] = {
      accept(IMPORT)
      val ts = new ListBuffer[Tree] + importExpr()
      while (in.token == COMMA) {
        in.nextToken(); ts += importExpr()
      }
      ts.toList
    }

    /**  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
     */
    def importExpr(): Tree =
      atPos(in.currentPos) {
        var t: Tree = null
        var pos = 0
        if (in.token == THIS) {
          t = atPos(in.currentPos) { This(nme.EMPTY.toTypeName) }
          t = atPos(accept(DOT)) { selector(t) }
          pos = accept(DOT)
        } else {
          val i = atPos(in.currentPos) { Ident(ident()) }
          pos = accept(DOT)
          if (in.token == THIS) {
            in.nextToken()
            t = atPos(i.pos) { This(i.name.toTypeName) }
            t = atPos(accept(DOT)) { selector(t) }
            pos = accept(DOT)
          } else {
            t = i
          }
        }
        def loop: Tree =
          if (in.token == USCORE) {
            in.nextToken()
            Import(t, List((nme.WILDCARD, null)))
          } else if (in.token == LBRACE) {
            Import(t, importSelectors())
          } else {
            val name = ident()
            if (in.token == DOT) {
              t = atPos(pos) { Select(t, name) }
              pos = accept(DOT)
              loop
            } else {
              Import(t, List((name, name)))
            }
          }
        loop
      }

    /** ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     */
    def importSelectors(): List[(Name, Name)] = {
      val names = new ListBuffer[(Name, Name)]
      accept(LBRACE)
      var isLast = importSelector(names)
      while (!isLast && in.token == COMMA) {
        in.nextToken()
        isLast = importSelector(names)
      }
      accept(RBRACE)
      names.toList
    }

    /** ImportSelector ::= Id [`=>' Id | `=>' `_']
     */
    def importSelector(names: ListBuffer[(Name, Name)]): boolean =
      if (in.token == USCORE) {
        in.nextToken(); names += (nme.WILDCARD, null); true
      } else {
        val name = ident()
        names += (
          name,
          if (in.token == ARROW) {
            in.nextToken()
            if (in.token == USCORE) { in.nextToken(); nme.WILDCARD } else ident()
          } else {
            name
          })
        false
      }

    /** Def    ::= val PatDef
     *           | var VarDef
     *           | def FunDef
     *           | type [nl] TypeDef
     *           | TmplDef
     *  Dcl    ::= val ValDcl
     *           | var ValDcl
     *           | def FunDcl
     *           | type [nl] TypeDcl
     */
    def defOrDcl(mods: Modifiers): List[Tree] =
      in.token match {
        case VAL =>
          patDefOrDcl(mods)
        case VAR =>
          varDefOrDcl(mods)
        case DEF =>
          List(funDefOrDcl(mods))
        case TYPE =>
          in.nextToken()
          newLinesOpt()
          List(typeDefOrDcl(mods))
        case _ =>
          List(tmplDef(mods))
      }

    /** PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  ValDcl ::= Id {`,' Id} `:' Type
     */
    def patDefOrDcl(mods: Modifiers): List[Tree] = {
      var newmods = mods
      var lhs = new ListBuffer[Tree]
      do {
        in.nextToken()
        lhs += stripParens(pattern2(false))
      } while (in.token == COMMA)
      val tp = typedOpt()
      val rhs =
        if (tp.isEmpty || in.token == EQUALS) equalsExpr()
        else {
          newmods = newmods | Flags.DEFERRED
          EmptyTree
        }
      def mkDefs(p: Tree): List[Tree] = {
        //Console.println("DEBUG: p = "+p.toString()); // DEBUG
        val trees =
          makePatDef(newmods,
                     if (tp.isEmpty)
                       p
                     else
                       Typed(p, tp),
                     rhs.duplicate) map atPos(p.pos)
        if (rhs == EmptyTree) {
          trees match {
            case List(ValDef(_, _, _, EmptyTree)) =>
            case _ => syntaxError(p.pos, "pattern definition may not be abstract", false)
          }
        }
        trees
      }
      for (val p <- lhs.toList; val d <- mkDefs(p)) yield d
    }

    /** VarDef ::= Id {`,' Id} [`:' Type] `=' Expr
     *           | Id {`,' Id} `:' Type `=' `_'
     *  VarDcl ::= Id {`,' Id} `:' Type
     */
    def varDefOrDcl(mods: Modifiers): List[Tree] = {
      var newmods = mods | Flags.MUTABLE
      val lhs = new ListBuffer[(Int, Name)]
      do {
        lhs += (in.skipToken(), ident())
      } while (in.token == COMMA)
      val tp = typedOpt()
      val rhs = if (tp.isEmpty || in.token == EQUALS) {
        accept(EQUALS)
        if (!tp.isEmpty && in.token == USCORE) {
          in.nextToken()
          EmptyTree
        } else {
          expr()
        }
      } else {
        newmods = newmods | Flags.DEFERRED
        EmptyTree
      }
      for (val (pos, name) <- lhs.toList) yield
        atPos(pos) { ValDef(newmods, name, tp.duplicate, rhs.duplicate) }
    }

    /** FunDef ::= FunSig `:' Type `=' Expr
     *           | FunSig [nl] `{' Block `}'
     *           | this ParamClause ParamClauses (`=' ConstrExpr | [nl] ConstrBlock)
     *  FunDcl ::= FunSig [`:' Type]
     *  FunSig ::= id [FunTypeParamClause] ParamClauses
     */
    def funDefOrDcl(mods: Modifiers): Tree =
      atPos(in.skipToken()) {
        if (in.token == THIS) {
          in.nextToken()
          val vparamss = paramClauses(nme.CONSTRUCTOR, implicitClassViews map (.duplicate), false)
          newLineOptWhenFollowedBy(LBRACE)
          val rhs = if (in.token == LBRACE) constrBlock(vparamss)
                    else { accept(EQUALS); constrExpr(vparamss) }
          DefDef(mods, nme.CONSTRUCTOR, List(), vparamss, TypeTree(), rhs)
        } else {
          var newmods = mods
          val name = ident()
          val implicitViewBuf = new ListBuffer[Tree]
          val tparams = typeParamClauseOpt(name, implicitViewBuf)
          val vparamss = paramClauses(name, implicitViewBuf.toList, false)
          newLineOptWhenFollowedBy(LBRACE)
          var restype = typedOpt()
          val rhs =
            if (isStatSep || in.token == RBRACE) {
              if (restype.isEmpty) restype = scalaUnitConstr
              newmods = newmods | Flags.DEFERRED
              EmptyTree
            } else if (restype.isEmpty && in.token == LBRACE) {
              restype = scalaUnitConstr
              blockExpr()
            } else equalsExpr()
          DefDef(newmods, name, tparams, vparamss, restype, rhs)
        }
      }

    /** ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     */
    def constrExpr(vparamss: List[List[ValDef]]): Tree =
      if (in.token == LBRACE) constrBlock(vparamss) else selfInvocation(vparamss)

    /** SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
     */
    def selfInvocation(vparamss: List[List[ValDef]]): Tree =
      atPos(accept(THIS)) {
        newLineOptWhenFollowedBy(LBRACE)
        var t = Apply(Ident(nme.CONSTRUCTOR), argumentExprs())
        while (in.token == LPAREN || in.token == LBRACE) {
          t = Apply(t, argumentExprs())
          newLineOptWhenFollowedBy(LBRACE)
        }
        if (!implicitClassViews.isEmpty) t = Apply(t, vparamss.last.map(vd => Ident(vd.name)))
        t
      }

    /** ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     */
    def constrBlock(vparamss: List[List[ValDef]]): Tree =
      atPos(in.skipToken()) {
        val statlist = new ListBuffer[Tree]
        statlist += selfInvocation(vparamss)
        val stats = if (isStatSep) { in.nextToken(); blockStatSeq(statlist) }
                    else statlist.toList
        accept(RBRACE)
        makeBlock(stats)
      }

    /** TypeDef ::= Id [TypeParamClause] `=' Type
     *  TypeDcl ::= Id TypeBounds
     */
    def typeDefOrDcl(mods: Modifiers): Tree =
      atPos(in.currentPos) {
        val name = ident().toTypeName
        in.token match {
          case LBRACKET =>
            val tparams = typeParamClauseOpt(name, null)
            accept(EQUALS)
            AliasTypeDef(mods, name, tparams, typ())
          case EQUALS =>
            in.nextToken()
            AliasTypeDef(mods, name, List(), typ())
          case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE =>
            typeBounds(mods | Flags.DEFERRED, name)
          case _ =>
            syntaxErrorOrIncomplete("`=', `>:', or `<:' expected", true)
            EmptyTree
        }
      }

    /**  TmplDef ::= [case] class ClassDef
     *            |  [case] object ObjectDef
     *            |  trait TraitDef
     */
    def tmplDef(mods: Modifiers): Tree = in.token match {
      case TRAIT =>
        classDef(mods | Flags.TRAIT | Flags.ABSTRACT)
      case CLASS =>
        classDef(mods)
      case CASECLASS =>
        classDef(mods | Flags.CASE)
      case OBJECT =>
        objectDef(mods)
      case CASEOBJECT =>
        objectDef(mods | Flags.CASE)
      case _ =>
        syntaxErrorOrIncomplete("expected start of definition", true)
        EmptyTree
    }

    /** ClassDef ::= Id [TypeParamClause] Annotations
                     [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplate
     *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplate
     */
    def classDef(mods: Modifiers): ClassDef =
      atPos(in.skipToken()) {
        val name = ident().toTypeName
        val savedViews = implicitClassViews
        val implicitViewBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, implicitViewBuf)
        implicitClassViews = implicitViewBuf.toList
        if (!implicitClassViews.isEmpty && mods.hasFlag(Flags.TRAIT)) {
          syntaxError("traits cannot have type parameters with <% bounds", false)
          implicitClassViews = List()
        }
        //if (mods.hasFlag(Flags.CASE) && in.token != LPAREN) accept(LPAREN)
        val constrAnnots = annotations()
        val (constrMods, vparamss) =
          if (mods.hasFlag(Flags.TRAIT)) (NoMods, List())
          else (accessModifierOpt(),
                    paramClauses(name, implicitClassViews, mods.hasFlag(Flags.CASE)))
        val thistpe = requiresTypeOpt()
        val (self0, template) =
          classTemplate(mods, name, constrMods withAnnotations constrAnnots, vparamss)
        val mods1 = if (mods.hasFlag(Flags.TRAIT) &&
                        (template.body forall treeInfo.isInterfaceMember))
                      mods | Flags.INTERFACE
                    else mods
        val self = if (thistpe.isEmpty) self0 else makeSelfDef(nme.WILDCARD, thistpe)
        val result = ClassDef(mods1, name, tparams, self, template)
        implicitClassViews = savedViews
        result
      }

    /** ObjectDef       ::= Id ClassTemplate
     */
    def objectDef(mods: Modifiers): ModuleDef =
      atPos(in.skipToken()) {
        val name = ident()
        val (self, template0) = classTemplate(mods, name, NoMods, List())
        val template = self match {
          case ValDef(mods, name, tpt, EmptyTree) if (name != nme.WILDCARD) =>
            val vd = ValDef(mods, name, tpt, This(nme.EMPTY.toTypeName)) setPos self.pos
            Template(template0.parents, vd :: template0.body)
          case _ =>
            template0
        }
        ModuleDef(mods, name, template)
      }

    /** ClassTemplate      ::= [extends TemplateParents] [TemplateBody]
     *  TemplateParents    ::= AnnotType {`(' [Exprs] `)'} {`with' AnnotType}
     *  TraitTemplate      ::= [extends MixinParents] [TemplateBody]
     *  MixinParents       ::= AnnotType {with AnnotType}
     */
    def classTemplate(mods: Modifiers, name: Name, constrMods: Modifiers, vparamss: List[List[ValDef]]): (ValDef, Template) = {
      val pos = in.currentPos;
      def acceptEmptyTemplateBody(msg: String): unit = {
        if (in.token == LPAREN && settings.migrate.value)
          syntaxErrorMigrate("traits may not have parameters")
        if (!(isStatSep || in.token == COMMA || in.token == RBRACE || in.token == EOF))
          syntaxError(msg, true)
      }
      val parents = new ListBuffer[Tree]
      val argss = new ListBuffer[List[Tree]]
      if (in.token == EXTENDS) {
        in.nextToken()
        val parent = annotType(false)
        // System.err.println("classTempl: " + parent)
        parents += parent
        if (in.token == LPAREN && !mods.hasFlag(Flags.TRAIT))
          do { argss += argumentExprs() } while (in.token == LPAREN)
        else argss += List()
        while (in.token == WITH) {
          in.nextToken()
          parents += annotType(false)
        }
      } else {
        if (in.token == WITH && settings.migrate.value)
          syntaxErrorMigrate("`extends' needed before `with'")
        newLineOptWhenFollowedBy(LBRACE)
        if (in.token != LBRACE) acceptEmptyTemplateBody("`extends' or `{' expected")
        argss += List()
      }
      if (name != nme.ScalaObject.toTypeName)
        parents += scalaScalaObjectConstr
      if (mods.hasFlag(Flags.CASE)) {
        parents += productConstr
      }
      val ps = parents.toList
      newLineOptWhenFollowedBy(LBRACE)
      val (self, body) =
        if (in.token == LBRACE) templateBody()
        else { acceptEmptyTemplateBody("`{' expected"); (emptyValDef, List()) }
      (self,
           atPos(pos) {
             if (!mods.hasFlag(Flags.TRAIT)) Template(ps, constrMods, vparamss, argss.toList, body)
             else Template(ps, body)
           })
    }

////////// TEMPLATES ////////////////////////////////////////////////////////////

    /** TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     */
    def templateBody(): (ValDef, List[Tree]) = {
      accept(LBRACE)
      val result @ (self, stats) = templateStatSeq()
      accept(RBRACE)
      if (stats.isEmpty) (self, List(EmptyTree)) else result
    }

    /** Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
     */
    def refinement(): List[Tree] = {
      accept(LBRACE)
      val body = refineStatSeq()
      accept(RBRACE)
      body
    }

/////// STATSEQS //////////////////////////////////////////////////////////////

    /** Packaging ::= package QualId [nl] `{' TopStatSeq `}'
     */
    def packaging(): Tree = {
      atPos(accept(PACKAGE)) {
        val pkg = qualId()
        newLineOptWhenFollowedBy(LBRACE)
        accept(LBRACE)
        val stats = topStatSeq()
        accept(RBRACE)
        makePackaging(pkg, stats)
      }
    }

    /** TopStatSeq ::= TopStat {semi TopStat}
     *  TopStat ::= Annotations Modifiers TmplDef
     *            | Packaging
     *            | Import
     *            |
     */
    def topStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (in.token != RBRACE && in.token != EOF) {
        if (in.token == PACKAGE) {
          stats += packaging()
        } else if (in.token == IMPORT) {
          stats ++= importClause()
        } else if (in.token == CLASS ||
                   in.token == CASECLASS ||
                   in.token == TRAIT ||
                   in.token == OBJECT ||
                   in.token == CASEOBJECT ||
                   in.token == LBRACKET || //todo: remove
                   in.token == AT ||
                   isModifier) {
          val annots = annotations()
          stats ++ joinComment(List(tmplDef(modifiers() withAnnotations annots)))
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("expected class or object definition", true)
        }
        if (in.token != RBRACE && in.token != EOF) acceptStatSep()
      }
      stats.toList
    }

    /** TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStat {semi TemplateStat}
     *  TemplateStat     ::= Import
     *                     | Annotations Modifiers Def
     *                     | Annotations Modifiers Dcl
     *                     | Expr1
     *                     |
     */
    def templateStatSeq(): (ValDef, List[Tree]) = {
      var self: ValDef = emptyValDef
      val stats = new ListBuffer[Tree]
      if (isExprIntro) {
        val first = expr(InTemplate)
        if (in.token == ARROW) {
          convertToParam(first) match {
            case ValDef(_, name, tpt, EmptyTree) if (name != nme.ERROR) =>
              self = makeSelfDef(name, tpt)
            case _ =>
          }
          in.nextToken()
        } else stats += first
      }
      while (in.token != RBRACE && in.token != EOF) {
        if (in.token == IMPORT) {
          stats ++= importClause()
        } else if (isExprIntro) {
          stats += expr(InTemplate)
        } else if (isDefIntro || isModifier || in.token == LBRACKET /*todo: remove */ || in.token == AT) {
          val annots = annotations()
          stats ++ joinComment(defOrDcl(modifiers() withAnnotations annots))
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("illegal start of definition", true)
        }
        if (in.token != RBRACE && in.token != EOF) acceptStatSep()
      }
      (self, stats.toList)
    }

    /** RefineStatSeq    ::= RefineStat {semi RefineStat}
     *  RefineStat       ::= Dcl
     *                     | type TypeDef
     *                     |
     */
    def refineStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (in.token != RBRACE && in.token != EOF) {
        if (isDclIntro) {
          stats ++= joinComment(defOrDcl(NoMods))
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("illegal start of declaration", true)
        }
        if (in.token != RBRACE) acceptStatSep()
      }
      stats.toList
    }

    /** BlockStatSeq ::= { BlockStat semi } [ResultExpr]
     *  BlockStat    ::= Import
     *                 | [implicit] Def
     *                 | LocalModifiers TmplDef
     *                 | Expr1
     *                 |
     */
    def blockStatSeq(stats: ListBuffer[Tree]): List[Tree] = {
      def localDef(mods: Modifiers) = {
        if (!(mods hasFlag ~Flags.IMPLICIT)) stats ++= defOrDcl(mods)
        else stats += tmplDef(mods)
        if (in.token == RBRACE || in.token == CASE)
          syntaxError("block must end in result expression, not in definition", false)
        else
          acceptStatSep()
        if (in.token == RBRACE || in.token == CASE)
          stats += Literal(()).setPos(in.currentPos)
      }
      var last = false
      while ((in.token != RBRACE) && (in.token != EOF) && (in.token != CASE) && !last) {
        if (in.token == IMPORT) {
          stats ++= importClause()
          acceptStatSep()
        } else if (isExprIntro) {
          stats += expr(InBlock)
          if (in.token != RBRACE && in.token != CASE) acceptStatSep()
        } else if (isDefIntro) {
          localDef(NoMods)
        } else if (isLocalModifier) {
          localDef(localModifiers())
        } else if (isStatSep) {
          in.nextToken()
        } else {
          syntaxErrorOrIncomplete("illegal start of statement", true)
        }
      }
      stats.toList
    }

    /** CompilationUnit ::= [package QualId semi] TopStatSeq
     */
    def compilationUnit(): Tree =
      atPos(in.currentPos) {
        val ts = new ListBuffer[Tree]
        if (in.token == PACKAGE) {
          in.nextToken()
          val pkg = qualId()
          newLineOptWhenFollowedBy(LBRACE)
          if (in.token == EOF) {
            ts += makePackaging(pkg, List())
          } else if (isStatSep) {
            in.nextToken()
            ts += makePackaging(pkg, topStatSeq())
          } else {
            accept(LBRACE)
            ts += makePackaging(pkg, topStatSeq())
            accept(RBRACE)
            ts ++= topStatSeq()
          }
        } else {
          ts ++= topStatSeq()
        }
        val stats = ts.toList
        stats match {
          case List(stat @ PackageDef(_, _)) => stat
          case _ => makePackaging(Ident(nme.EMPTY_PACKAGE_NAME), stats)
        }
      }
  }
}
