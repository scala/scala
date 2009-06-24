/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: Parsers.scala 17756 2009-05-18 14:28:59Z rytz $
//todo: allow infix type patterns


package scala.tools.nsc.ast.parser

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.{Position, OffsetPosition, NoPosition, BatchSourceFile}
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
trait Parsers extends Scanners with MarkupParsers {
self =>
  val global: Global
  import global._

  private val glob: global.type = global

  case class OpInfo(operand: Tree, operator: Name, offset: Offset)

  class UnitParser(val unit: global.CompilationUnit, patches: List[BracePatch]) extends Parser {

    def this(unit: global.CompilationUnit) = this(unit, List())

    val in = new UnitScanner(unit, patches)
    in.init()

    def freshName(pos: Position, prefix: String): Name =
      unit.fresh.newName(pos, prefix)

    def o2p(offset: Int): Position = new OffsetPosition(unit.source,offset)
    def r2p(start: Int, mid: Int, end: Int): Position = rangePos(unit.source, start, mid, end)

    def warning(offset: Int, msg: String) { unit.warning(o2p(offset), msg) }

    def deprecationWarning(offset: Int, msg: String) {
      unit.deprecationWarning(o2p(offset), msg)
    }

    var smartParsing = false

    val syntaxErrors = new ListBuffer[(Int, String)]

    def incompleteInputError(msg: String) {
      val offset = unit.source.asInstanceOf[BatchSourceFile].content.length - 1
      if (smartParsing) syntaxErrors += ((offset, msg))
      else unit.incompleteInputError(o2p(offset), msg)
    }

    def syntaxError(offset: Int, msg: String) {
      if (smartParsing) syntaxErrors += ((offset, msg))
      else unit.error(o2p(offset), msg)
    }

    /** parse unit. If there are inbalanced braces,
     *  try to correct them and reparse.
     */
    def smartParse(): Tree = try {
      smartParsing = true
      val firstTry = parse()
      if (syntaxErrors.isEmpty) firstTry
      else {
        val patches = in.healBraces()
        if (patches.isEmpty) {
          for ((offset, msg) <- syntaxErrors) unit.error(o2p(offset), msg)
          firstTry
        } else {
          println(patches)
          new UnitParser(unit, patches).parse()
        }
      }
    } finally {
      smartParsing = false
    }

    /** the markup parser */
    lazy val xmlp = new MarkupParser(this, true)

    object symbXMLBuilder extends SymbolicXMLBuilder(treeBuilder, this, true) { // DEBUG choices
      val global: self.global.type = self.global
      def freshName(prefix: String): Name = UnitParser.this.freshName(prefix)
    }

    def xmlLiteral : Tree = xmlp.xLiteral

    def xmlLiteralPattern : Tree = xmlp.xLiteralPattern
  }

  final val Local = 0
  final val InBlock = 1
  final val InTemplate = 2
  final val MINUS: Name = "-"
  final val PLUS : Name = "+"
  final val BANG : Name = "!"
  final val TILDE: Name = "~"
  final val AMP  : Name = "&"
  final val SLASH: Name = "/"
  final val STAR : Name = "*"
  final val BAR  : Name = "|"
  final val LT   : Name = "<"

  abstract class Parser {
    val in: Scanner
    //val unit : CompilationUnit
    //import in.ScanPosition
    def freshName(pos: Position, prefix: String): Name
    def freshName(prefix: String): Name = freshName(NoPosition, prefix) // todo get rid of position

    def o2p(offset: Int): Position
    def r2p(start: Int, mid: Int, end: Int): Position
    def t2p(tree: Tree): Position = SyntheticAliasPosition(tree)
    //private implicit def p2i(pos: Position) = pos.offset.get

    /** whether a non-continuable syntax error has been seen */
    //private var syntaxErrorSeen = false

    private var lastErrorOffset : Int = -1

    object treeBuilder extends TreeBuilder {
      val global: self.global.type = self.global
      def freshName(prefix: String): Name = Parser.this.freshName(prefix)
    }
    import treeBuilder.{global => _, _}

    /** The implicit view parameters of the surrounding class */
    var implicitClassViews: List[Tree] = Nil

    /** this is the general parse method
     */
    def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    }

/* --------------- PLACEHOLDERS ------------------------------------------- */

    /** The implicit parameters introduced by `_' in the current expression.
     *  Parameters appear in reverse order
     */
    var placeholderParams: List[ValDef] = Nil

    /** The placeholderTypes introduced by `_' in the current type.
     *  Parameters appear in reverse order
     */
    var placeholderTypes: List[TypeDef] = Nil

    def checkNoEscapingPlaceholders[T](op: => T): T = {
      val savedPlaceholderParams = placeholderParams
      val savedPlaceholderTypes = placeholderTypes
      placeholderParams = List()
      placeholderTypes = List()

      val res = op

      placeholderParams match {
        case vd :: _ =>
          syntaxError(vd.pos, "unbound placeholder parameter", false)
          placeholderParams = List()
        case _ =>
      }
      placeholderTypes match {
        case td :: _ =>
          syntaxError(td.pos, "unbound wildcard type", false)
          placeholderTypes = List()
        case _ =>
      }
      placeholderParams = savedPlaceholderParams
      placeholderTypes = savedPlaceholderTypes

      res
    }

    def placeholderTypeBoundary(op: => Tree): Tree = {
      val savedPlaceholderTypes = placeholderTypes
      placeholderTypes = List()
      var t = op
      if (!placeholderTypes.isEmpty && t.isInstanceOf[AppliedTypeTree]) {
        t = atPos(t.pos) { ExistentialTypeTree(t, placeholderTypes.reverse) }
        placeholderTypes = List()
      }
      placeholderTypes = placeholderTypes ::: savedPlaceholderTypes
      t
    }

    def isWildcard(t: Tree): Boolean = t match {
      case Ident(name1) => !placeholderParams.isEmpty && name1 == placeholderParams.head.name
      case Typed(t1, _) => isWildcard(t1)
      case Annotated(t1, _) => isWildcard(t1)
      case _ => false
    }

/* ------------- ERROR HANDLING ------------------------------------------- */

    var assumedClosingParens = collection.mutable.Map(RPAREN -> 0, RBRACKET -> 0, RBRACE -> 0)

    protected def skip(targetToken: Int) {
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
            nparens -= 1
          case RBRACE =>
            if (nbraces == 0) return
            nbraces -= 1
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        if (targetToken == in.token && nparens == 0 && nbraces == 0) return
        in.nextToken()
      }
    }
    def warning(offset: Int, msg: String): Unit
    def incompleteInputError(msg: String): Unit
    def deprecationWarning(offset: Int, msg: String): Unit
    private def syntaxError(pos: Position, msg: String, skipIt: Boolean) {
      pos.offset match {
        case None => syntaxError(msg,skipIt)
        case Some(offset) => syntaxError(offset, msg, skipIt)
      }
    }
    def syntaxError(offset: Int, msg: String): Unit
    def syntaxError(msg: String, skipIt: Boolean) {
      syntaxError(in.offset, msg, skipIt)
    }

    def syntaxError(offset: Int, msg: String, skipIt: Boolean) {
      if (offset > lastErrorOffset) {
        syntaxError(offset, msg)
        // no more errors on this token.
        lastErrorOffset = in.offset
      }
      if (skipIt)
        skip(UNDEF)
    }

    def warning(msg: String) { warning(in.offset, msg) }

    def syntaxErrorOrIncomplete(msg: String, skipIt: Boolean) {
      if (in.token == EOF)
        incompleteInputError(msg)
      else
        syntaxError(in.offset, msg, skipIt)
    }

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      */
    def accept(token: Int): Int = {
      val offset = in.offset
      if (in.token != token) {
        val msg =
          token2string(token) + " expected but " +token2string(in.token) + " found."
        syntaxErrorOrIncomplete(msg, true)
        if (in.token == EOF) incompleteInputError(msg)
        else syntaxError(in.offset, msg, false)
        if ((token == RPAREN || token == RBRACE || token == RBRACKET))
          if (in.parenBalance(token) + assumedClosingParens(token) < 0)
            assumedClosingParens(token) += 1
          else
            skip(token)
        else
          skip(UNDEF)
      }
      if (in.token == token) in.nextToken()
      offset
    }

    def surround[T](open: Int, close: Int)(f: => T, orElse: T): T = {
      val wasOpened = in.token == open
      accept(open)
      if (wasOpened) {
        val ret = f
        accept(close)
        ret
      } else orElse
    }

    /** semi = nl {nl} | `;'
     *  nl  = `\n' // where allowed
     */
    def acceptStatSep(): Boolean =
      if (in.token == NEWLINE || in.token == NEWLINES) { in.nextToken(); true }
      else {
        val ret = in.token == SEMI
        accept(SEMI)
        ret
      }

    def errorTypeTree = TypeTree().setType(ErrorType).setPos(o2p(in.offset))
    def errorTermTree = Literal(Constant(null)).setPos(o2p(in.offset))
    def errorPatternTree = Ident(nme.WILDCARD).setPos(o2p(in.offset))

    /** Check that type parameter is not by name T* */
    def checkNotByName(t: Tree) = t match {
      case AppliedTypeTree(Select(_, n), _) if (n == nme.BYNAME_PARAM_CLASS_NAME.toTypeName) =>
        syntaxError(t.pos, "no by-name parameter type allowed here", false)
      case _ =>
    }

    /** Check that tree is a legal clause of a forSome */
    def checkLegalExistential(t: Tree) = t match {
      case TypeDef(_, _, _, TypeBoundsTree(_, _)) |
           ValDef(_, _, _, EmptyTree) | EmptyTree =>
             ;
      case _ =>
        syntaxError(t.pos, "not a legal existential clause", false)
    }

/* -------------- TOKEN CLASSES ------------------------------------------- */

    def isModifier: Boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | PRIVATE |
           PROTECTED | OVERRIDE | IMPLICIT | LAZY => true
      case _ => false
    }

    def isLocalModifier: Boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY => true
      case _ => false
    }

    def isDefIntro: Boolean = in.token match {
      case VAL | VAR | DEF | TYPE | OBJECT |
           CASEOBJECT | CLASS | CASECLASS | TRAIT => true
      case _ => false
    }

    def isDclIntro: Boolean = in.token match {
      case VAL | VAR | DEF | TYPE => true
      case _ => false
    }

    def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT

    def isExprIntroToken(token: Int): Boolean = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
           STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL | IDENTIFIER | BACKQUOTED_IDENT |
           THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
           DO | RETURN | THROW | LPAREN | LBRACE | XMLSTART => true
      case _ => false
    }

    def isExprIntro: Boolean = isExprIntroToken(in.token)

    def isTypeIntroToken(token: Int): Boolean = token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS |
           SUPER | USCORE | LPAREN | AT => true
      case _ => false
    }

    def isTypeIntro: Boolean = isTypeIntroToken(in.token)

    def isStatSep(token: Int): Boolean =
      token == NEWLINE || token == NEWLINES || token == SEMI

    def isStatSep: Boolean = isStatSep(in.token)


/* --------- COMMENT AND ATTRIBUTE COLLECTION ----------------------------- */

    /** Join the comment associated with a definition
    */
    def joinComment(trees: => List[Tree]): List[Tree] = {
      val buf = in.flushDoc
      if ((buf ne null) && buf.length > 0) trees map (t => DocDef(buf, t) setPos t.pos) // !!! take true comment position
      else trees
    }

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

    def atPos[T <: Tree](offset: Int)(t: T): T =
      global.atPos(r2p(offset, offset, in.lastOffset))(t)
    def atPos[T <: Tree](start: Int, point: Int)(t: T): T =
      global.atPos(r2p(start, point, in.lastOffset))(t)
    def atPos[T <: Tree](start: Int, point: Int, end: Int)(t: T): T =
      global.atPos(r2p(start, point, end))(t)
    def atPos[T <: Tree](pos: Position)(t: T): T =
      global.atPos(pos)(t)

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
    def convertToParam(tree: Tree): ValDef = atPos(tree.pos) {
      def removeAsPlaceholder(name: Name) {
        placeholderParams = placeholderParams filter (_.name != name)
      }
      tree match {
        case Ident(name) =>
          removeAsPlaceholder(name)
          ValDef(Modifiers(Flags.PARAM), name, TypeTree() setPos o2p(tree.pos.end), EmptyTree)
        case Typed(tree @ Ident(name), tpe) if (tpe.isType) => // get the ident!
          removeAsPlaceholder(name)
          ValDef(Modifiers(Flags.PARAM), name, tpe, EmptyTree)
        case _ =>
          syntaxError(tree.pos, "not a legal formal parameter", false)
          ValDef(Modifiers(Flags.PARAM), nme.ERROR, errorTypeTree setPos o2p(tree.pos.end), EmptyTree)
      }
    }

    /** Convert (qual)ident to type identifier
     */
    def convertToTypeId(tree: Tree): Tree = atPos(tree.pos) {
      tree match {
        case Ident(name) =>
          Ident(name.toTypeName)
        case Select(qual, name) =>
          Select(qual, name.toTypeName)
        case _ =>
          syntaxError(tree.pos, "identifier expected", false)
          errorTypeTree
      }
    }

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */

    /** modes for infix types */
    object InfixMode extends Enumeration {
      val FirstOp, LeftOp, RightOp = Value
    }

    var opstack: List[OpInfo] = Nil

    def precedence(operator: Name): Int =
      if (operator eq nme.ERROR) -1
      else {
        val firstCh = operator(0)
        if (((firstCh >= 'A') && (firstCh <= 'Z')) ||
            ((firstCh >= 'a') && (firstCh <= 'z')))
          1
        else if (nme.isOpAssignmentName(operator))
          0
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

    def checkSize(kind: String, size: Int, max: Int) {
      if (size > max) syntaxError("too many "+kind+", maximum = "+max, false)
    }

    def checkAssoc(offset: Int, op: Name, leftAssoc: Boolean) =
      if (treeInfo.isLeftAssoc(op) != leftAssoc)
        syntaxError(
          offset, "left- and right-associative operators with same precedence may not be mixed", false)

    def reduceStack(isExpr: Boolean, base: List[OpInfo], top0: Tree,
                    prec: Int, leftAssoc: Boolean): Tree = {
      var top = top0
      if (opstack != base && precedence(opstack.head.operator) == prec)
        checkAssoc(opstack.head.offset, opstack.head.operator, leftAssoc)
      while (opstack != base &&
             (prec < precedence(opstack.head.operator) ||
              leftAssoc && prec == precedence(opstack.head.operator))) {
        val opinfo = opstack.head
        opstack = opstack.tail
        top = atPos(opinfo.operand.pos.start, opinfo.offset) {
          makeBinop(isExpr, opinfo.operand, opinfo.operator, top)
        }
      }
      top
    }

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

    def ident(): Name =
      if (in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT) {
        val name = in.name.encode
        in.nextToken()
        name
      } else {
        accept(IDENTIFIER)
        nme.ERROR
      }

    def selector(t: Tree): Tree = {
      val point = in.offset
      Select(t, ident()) setPos r2p(t.pos.start, point, in.lastOffset)
    }

    /** Path       ::= StableId
     *              |  [Ident `.'] this
     *  AnnotType ::= Path [`.' type]
     */
    def path(thisOK: Boolean, typeOK: Boolean): Tree = {
      val start = in.offset
      var t: Tree = null
      if (in.token == THIS) {
        in.nextToken()
        t = atPos(start) { This(nme.EMPTY.toTypeName) }
        if (!thisOK || in.token == DOT) {
          t = selectors(t, typeOK, accept(DOT))
        }
      } else if (in.token == SUPER) {
        in.nextToken()
        t = atPos(start) { Super(nme.EMPTY.toTypeName, mixinQualifierOpt()) }
        accept(DOT)
        t = selector(t)
        if (in.token == DOT) t = selectors(t, typeOK, in.skipToken())
      } else {
        val tok = in.token
        val name = ident()
        t = atPos(start) {
          if (tok == BACKQUOTED_IDENT) new BackQuotedIdent(name)
          else Ident(name)
        }
        if (in.token == DOT) {
          val dotOffset = in.skipToken()
          if (in.token == THIS) {
            in.nextToken()
            t = atPos(start) { This(name.toTypeName) }
            if (!thisOK || in.token == DOT)
              t = selectors(t, typeOK, accept(DOT))
          } else if (in.token == SUPER) {
            in.nextToken()
            t = atPos(start) { Super(name.toTypeName, mixinQualifierOpt()) }
            accept(DOT)
            t = selector(t)
            if (in.token == DOT) t = selectors(t, typeOK, in.skipToken())
          } else {
            t = selectors(t, typeOK, dotOffset)
          }
        }
      }
      t
    }

    def selectors(t: Tree, typeOK: Boolean, dotOffset: Int): Tree =
      if (typeOK && in.token == TYPE) {
        in.nextToken()
        atPos(t.pos.start, dotOffset) { SingletonTypeTree(t) }
      } else {
        val t1 = selector(t)
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
      val start = in.offset
      val id = atPos(start) { Ident(ident()) }
      if (in.token == DOT) { selectors(id, false, in.skipToken()) }
      else id
    }

    /** SimpleExpr    ::= literal
     *                  | symbol
     *                  | null
     *  @note  The returned tree does not yet have a position
     */
    def literal(isNegated: Boolean): Tree = {
      val isSymLit = in.token == SYMBOLLIT
      val t = Literal {
        in.token match {
          case CHARLIT   => Constant(in.charVal)
          case INTLIT    => Constant(in.intVal(isNegated).toInt)
          case LONGLIT   => Constant(in.intVal(isNegated))
          case FLOATLIT  => Constant(in.floatVal(isNegated).toFloat)
          case DOUBLELIT => Constant(in.floatVal(isNegated))
          case STRINGLIT | SYMBOLLIT => Constant(in.strVal)
          case TRUE      => Constant(true)
          case FALSE     => Constant(false)
          case NULL      => Constant(null)
          case _ =>
            syntaxErrorOrIncomplete("illegal literal", true)
            Constant(null)
        }
      }
      in.nextToken()
      if (isSymLit) Apply(scalaDot(nme.Symbol), List(t)) else t
    }

/* ------------- NEW LINES ------------------------------------------------- */

    def newLineOpt() {
      if (in.token == NEWLINE) in.nextToken()
    }

    def newLinesOpt() {
      if (in.token == NEWLINE || in.token == NEWLINES)
        in.nextToken()
    }

    def newLineOptWhenFollowedBy(token: Int) {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && in.next.token == token) newLineOpt()
    }

    def newLineOptWhenFollowing(p: Int => Boolean) {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && p(in.next.token)) newLineOpt()
    }

/* ------------- TYPES ---------------------------------------------------- */

    /** TypedOpt ::= [`:' Type]
     */
    def typedOpt(): Tree =
      if (in.token == COLON) { in.nextToken(); typ() }
      else TypeTree()

    /** Types ::= Type {`,' Type}
     */
    def types(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean): List[Tree] = {
      val ts = new ListBuffer[Tree] += argType(isPattern, isTypeApply, isFuncArg)
      while (in.token == COMMA) {
        in.nextToken()
        ts += argType(isPattern, isTypeApply, isFuncArg)
      }
      ts.toList
    }

    /** Type ::= InfixType `=>' Type
     *         | `(' [`=>' Type] `)' `=>' Type
     *         | InfixType [ExistentialClause]
     *  ExistentialClause ::= forSome `{' ExistentialDcl {semi ExistentialDcl}} `}'
     *  ExistentialDcl    ::= type TypeDcl | val ValDcl
     */
    def typ(): Tree = typ(false)

    def typ(isPattern: Boolean): Tree = placeholderTypeBoundary {
      val start = in.offset
      val t =
        if (in.token == LPAREN) {
          in.nextToken()
          if (in.token == RPAREN) {
            in.nextToken()
            atPos(start, accept(ARROW)) {
              makeFunctionTypeTree(List(), typ(isPattern))
            }
          } else {
            val ts = types(isPattern, false, true)
            accept(RPAREN)
            if (in.token == ARROW)
              atPos(start, in.skipToken()) {
                makeFunctionTypeTree(ts, typ(isPattern))
              }
            else {
              ts foreach checkNotByName
              val tuple = atPos(start) { makeTupleType(ts, true) }
              infixTypeRest(
                compoundTypeRest(
                  annotTypeRest(simpleTypeRest(tuple, isPattern)),
                  isPattern),
                isPattern, InfixMode.FirstOp)
            }
          }
        } else {
          infixType(isPattern, InfixMode.FirstOp)
        }
      if (in.token == ARROW)
        atPos(start, in.skipToken()) {
          makeFunctionTypeTree(List(t), typ(isPattern))
        }
      else if (in.token == FORSOME)
        atPos(start, in.skipToken()) {
          val whereClauses = refinement()
          whereClauses foreach checkLegalExistential
          ExistentialTypeTree(t, whereClauses)
        }
      else t
    }

    /** InfixType ::= CompoundType {id [nl] CompoundType}
     */
    def infixType(isPattern: Boolean, mode: InfixMode.Value): Tree = placeholderTypeBoundary {
      infixTypeRest(compoundType(isPattern), isPattern, mode)
    }

    def infixTypeRest(t: Tree, isPattern: Boolean, mode: InfixMode.Value): Tree = {
      if (isIdent && in.name != nme.STAR) {
        val opOffset = in.offset
        val leftAssoc = treeInfo.isLeftAssoc(in.name)
        if (mode != InfixMode.FirstOp) checkAssoc(opOffset, in.name, mode == InfixMode.LeftOp)
        val op = ident()
        val tycon = atPos(opOffset) { Ident(op.toTypeName) }
        newLineOptWhenFollowing(isTypeIntroToken)
        def mkOp(t1: Tree) = atPos(t.pos.start, opOffset) { AppliedTypeTree(tycon, List(t, t1)) }
        if (leftAssoc)
          infixTypeRest(mkOp(compoundType(isPattern)), isPattern, InfixMode.LeftOp)
        else
          mkOp(infixType(isPattern, InfixMode.RightOp))
      } else t
    }

    /** CompoundType ::= AnnotType {with AnnotType} [Refinement]
     *                |  Refinement
     */
    def compoundType(isPattern: Boolean): Tree = {
      val t = if (in.token == LBRACE) atPos(o2p(in.offset)) (scalaAnyRefConstr)
              else annotType(isPattern)
      compoundTypeRest(t, isPattern)
    }

    def compoundTypeRest(t: Tree, isPattern: Boolean): Tree = {
      var ts = new ListBuffer[Tree] += t
      while (in.token == WITH) {
        in.nextToken(); ts += annotType(isPattern)
      }
      newLineOptWhenFollowedBy(LBRACE)
      atPos(t.pos.start) {
        if (in.token == LBRACE) {
          // Warn if they are attempting to refine Unit; we can't be certain it's
          // scala.Unit they're refining because at this point all we have is an
          // identifier, but at a later stage we lose the ability to tell an empty
          // refinement from no refinement at all.  See bug #284.
          for (Ident(name) <- ts) name.toString match {
            case "Unit" | "scala.Unit"  =>
              warning("Detected apparent refinement of Unit; are you missing an '=' sign?")
            case _ =>
          }
          CompoundTypeTree(Template(ts.toList, emptyValDef, refinement()))
        }
        else
          makeIntersectionTypeTree(ts.toList)
      }
    }

    /** AnnotType        ::=  SimpleType {Annotation}
     */
    def annotType(isPattern: Boolean): Tree = placeholderTypeBoundary {
      annotTypeRest(simpleType(isPattern))
    }

    def annotTypeRest(t: Tree): Tree =
      (t /: annotations(false, false)) (makeAnnotated)

    /** SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' Id
     *                     |  StableId
     *                     |  Path `.' type
     *                     |  `(' Types [`,'] `)'
     *                     |  WildcardType
     */
    def simpleType(isPattern: Boolean): Tree = {
      val start = in.offset
      val t =
        if (in.token == LPAREN) {
          in.nextToken()
          val ts = types(isPattern, false, false)
          accept(RPAREN)
          atPos(start) { makeTupleType(ts, true) }
        } else if (in.token == USCORE) {
          wildcardType(in.skipToken())
        } else path(false, true) match {
          case r @ SingletonTypeTree(_) => r
          case r => convertToTypeId(r)
        }
      simpleTypeRest(t, isPattern)
    }

    def simpleTypeRest(t: Tree, isPattern: Boolean): Tree =
      if (in.token == HASH) {
        val hashOffset = in.skipToken()
        val nameOffset = in.offset
        val name = ident()
        val sel = atPos(t.pos.start, if (name == nme.ERROR) hashOffset else nameOffset) {
          SelectFromTypeTree(t, name.toTypeName)
        }
        simpleTypeRest(sel, isPattern)
      } else if (in.token == LBRACKET) {
        simpleTypeRest(atPos(t.pos.start) { AppliedTypeTree(t, typeArgs(isPattern, false)) }, isPattern)
      } else {
        t
      }

    /** WildcardType ::= `_' TypeBounds
     */
    def wildcardType(start: Int) = {
      val pname = freshName(o2p(start), "_$").toTypeName
      val t = atPos(start) { Ident(pname) }
      val param = atPos(t2p(t)) { makeSyntheticTypeParam(pname, typeBounds()) }
      placeholderTypes = param :: placeholderTypes
      t
    }

    /** TypeArgs    ::= `[' ArgType {`,' ArgType} `]'
     */
    def typeArgs(isPattern: Boolean, isTypeApply: Boolean): List[Tree] = {
      accept(LBRACKET)
      val ts = types(isPattern, isTypeApply, false)
      accept(RBRACKET)
      ts
    }

    /** ArgType       ::=  Type
     */
    def argType(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean): Tree = {
      val start = in.offset
      if (isPattern) {
        if (in.token == USCORE) {
          in.nextToken()
          if (in.token == SUBTYPE || in.token == SUPERTYPE) wildcardType(start)
          else atPos(start) { Bind(nme.WILDCARD.toTypeName, EmptyTree) }
        } else if (in.token == IDENTIFIER && treeInfo.isVariableName(in.name.toTypeName)) {
          atPos(start) { Bind(ident().toTypeName, EmptyTree) }
        } else {
          typ(true)
        }
      } else if (isFuncArg) {
        // copy-paste (with change) from def paramType
        if (in.token == ARROW) {
          in.nextToken()
          val tycon = atPos(start) { rootScalaDot(nme.BYNAME_PARAM_CLASS_NAME.toTypeName) }
          atPos(start) { AppliedTypeTree(tycon, List(typ())) }
        } else {
          val t = typ()
          if (isIdent && in.name == STAR) {
            val tycon = atPos(in.skipToken()) { rootScalaDot(nme.REPEATED_PARAM_CLASS_NAME.toTypeName) }
            atPos(start) { AppliedTypeTree(tycon, List(t)) }
          } else t
        }
      } else {
        typ()
      }
    }

/* ----------- EXPRESSIONS ------------------------------------------------ */

    /** EqualsExpr ::= `=' Expr
     */
    def equalsExpr(): Tree = {
      accept(EQUALS)
      expr()
    }

    /** Exprs ::= Expr {`,' Expr}
     */
    def exprs(): List[Tree] = {
      val ts = new ListBuffer[Tree] += expr()
      while (in.token == COMMA) {
        in.nextToken()
        ts += expr()
      }
      ts.toList
    }

    def condExpr(): Tree = {
      if (in.token == LPAREN) {
        in.nextToken()
        val r = expr()
        accept(RPAREN)
        r
      } else {
        accept(LPAREN)
        Literal(true)
      }
    }

    /* hook for IDE, unlike expression can be stubbed
     * don't use for any tree that can be inspected in the parser!
     */
    def statement(location: Int): Tree = expr(location) // !!! still needed?

    /** Expr       ::= (Bindings | Id | `_')  `=>' Expr
     *               | Expr1
     *  ResultExpr ::= (Bindings | Id `:' CompoundType) `=>' Block
     *               | Expr1
     *  Expr1      ::= if `(' Expr `)' {nl} Expr [[semi] else Expr]
     *               | try (`{' Block `}' | Expr) [catch `{' CaseClauses `}'] [finally Expr]
     *               | while `(' Expr `)' {nl} Expr
     *               | do Expr [semi] while `(' Expr `)'
     *               | for (`(' Enumerators `)' | '{' Enumerators '}') {nl} [yield] Expr
     *               | throw Expr
     *               | return [Expr]
     *               | [SimpleExpr `.'] Id `=' Expr
     *               | SimpleExpr1 ArgumentExprs `=' Expr
     *               | PostfixExpr Ascription
     *               | PostfixExpr match `{' CaseClauses `}'
     *  Bindings   ::= `(' [Binding {`,' Binding}] `)'
     *  Binding    ::= (Id | `_') [`:' Type]
     *  Ascription ::= `:' CompoundType
     *               | `:' Annotation {Annotation}
     *               | `:' `_' `*'
     */
    def expr(): Tree = expr(Local)

    def expr(location: Int): Tree = {
      var savedPlaceholderParams = placeholderParams
      placeholderParams = List()
      var res = expr0(location)
      if (!placeholderParams.isEmpty && !isWildcard(res)) {
        res = atPos(res.pos){ Function(placeholderParams.reverse, res) }
        placeholderParams = List()
      }
      placeholderParams = placeholderParams ::: savedPlaceholderParams
      res
    }

    def expr0(location: Int): Tree = in.token match {
      case IF =>
        atPos(in.skipToken()) {
          val cond = condExpr()
          newLinesOpt()
          val thenp = expr()
          val elsep = if (in.token == ELSE) { in.nextToken(); expr() }
                      else Literal(())
          If(cond, thenp, elsep)
        }
      case TRY =>
        atPos(in.skipToken()) {
          val body =
            if (in.token == LBRACE) surround(LBRACE, RBRACE)(block(), Literal(()))
            else if (in.token == LPAREN) surround(LPAREN, RPAREN)(expr(), Literal(()))
            else expr()
          val catches =
            if (in.token == CATCH) {
              in.nextToken()
              surround(LBRACE, RBRACE)(caseClauses(), Nil)
            } else Nil
          val finalizer =
            if (in.token == FINALLY) { in.nextToken(); expr() }
            else EmptyTree
          Try(body, catches, finalizer)
        }
      case WHILE =>
        val start = in.offset
        atPos(in.skipToken()) {
          val lname: Name = freshName(o2p(start), "while$")
          val cond = condExpr()
          newLinesOpt()
          val body = expr()
          makeWhile(lname, cond, body)
        }
      case DO =>
        val start = in.offset
        atPos(in.skipToken()) {
          val lname: Name = freshName(o2p(start), "doWhile$")
          val body = expr()
          if (isStatSep) in.nextToken()
          accept(WHILE)
          val cond = condExpr()
          makeDoWhile(lname, body, cond)
        }
      case FOR =>
        atPos(in.skipToken()) {
          val (open, close) = if (in.token == LBRACE) (LBRACE, RBRACE) else (LPAREN, RPAREN)
          val enums = surround(open, close)(enumerators(), Nil)
          newLinesOpt()
          if (in.token == YIELD) {
            in.nextToken()
            makeForYield(enums, expr())
          } else {
            makeFor(enums, expr())
          }
        }
      case RETURN =>
        atPos(in.skipToken()) {
          Return(if (isExprIntro) expr() else Literal(()))
        }
      case THROW =>
        atPos(in.skipToken()) {
          Throw(expr())
        }
      case _ =>
        var t = postfixExpr()
        if (in.token == EQUALS) {
          t match {
            case Ident(_) | Select(_, _) | Apply(_, _) =>
              t = atPos(t.pos.start, in.skipToken()) { makeAssign(t, expr()) }
            case _ =>
          }
        } else if (in.token == COLON) {
          t = stripParens(t)
          val colonPos = in.skipToken()
          if (in.token == USCORE) {
            //todo: need to handle case where USCORE is a wildcard in a type
            val uscorePos = in.skipToken()
            if (isIdent && in.name == nme.STAR) {
              in.nextToken()
              t = atPos(t.pos.start, colonPos) {
                Typed(t, atPos(uscorePos) { Ident(nme.WILDCARD_STAR.toTypeName) })
              }
            } else {
              syntaxErrorOrIncomplete("`*' expected", true)
            }
          } else if (in.token == AT) {
            t = (t /: annotations(false, false)) (makeAnnotated)
          } else {
            t = atPos(t.pos.start, colonPos) {
              val tpt =
                if (location == Local) typ() else infixType(false, InfixMode.FirstOp)
              if (isWildcard(t))
                (placeholderParams: @unchecked) match {
                  case (vd @ ValDef(mods, name, _, _)) :: rest =>
                    placeholderParams = treeCopy.ValDef(vd, mods, name, tpt.syntheticDuplicate, EmptyTree) :: rest
                }
              // this does not correspond to syntax, but is necessary to
              // accept closures. We might restrict closures to be between {...} only.
              Typed(t, tpt)
            }
          }
        } else if (in.token == MATCH) {
          t = atPos(t.pos.start, in.skipToken()) {
            Match(stripParens(t), surround(LBRACE, RBRACE)(caseClauses(), Nil))
          }
        }
        // in order to allow anonymous functions as statements (as opposed to expressions) inside
        // templates, we have to disambiguate them from self type declarations - bug #1565
        // The case still missed is unparenthesized single argument, like "x: Int => x + 1", which
        // may be impossible to distinguish from a self-type and so remains an error.  (See #1564)
        def lhsIsTypedParamList() = t match {
          case Parens(xs) if xs forall (_.isInstanceOf[Typed]) => true
          case _ => false
        }
        if (in.token == ARROW && (location != InTemplate || lhsIsTypedParamList)) {
          t = atPos(t.pos.start, in.skipToken()) {
            Function(convertToParams(t), if (location != InBlock) expr() else block())
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
        top = reduceStack(true, base, top, precedence(in.name), treeInfo.isLeftAssoc(in.name))
        val op = in.name
        opstack = OpInfo(top, op, in.offset) :: opstack
        ident()
        newLineOptWhenFollowing(isExprIntroToken)
        if (isExprIntro) {
          top = prefixExpr()
        } else {
          val topinfo = opstack.head
          opstack = opstack.tail
          val od = stripParens(reduceStack(true, base, topinfo.operand, 0, true))
          return atPos(od.pos.start, topinfo.offset) {
            Select(od, topinfo.operator.encode)
          }
        }
      }
      reduceStack(true, base, top, 0, true)
    }

    /** PrefixExpr   ::= [`-' | `+' | `~' | `!' | `&'] SimpleExpr
    */
    def prefixExpr(): Tree = {
      def unaryOp(): Name = "unary_" + ident()
      if (isIdent && in.name == MINUS) {
        atPos(in.offset) {
          val name = unaryOp()
          in.token match {
            case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT => literal(true)
            case _ => Select(stripParens(simpleExpr()), name)
          }
        }
      } else if (isIdent && (in.name == PLUS || in.name == TILDE || in.name == BANG)) {
        atPos(in.offset) {
          val name = unaryOp()
          Select(stripParens(simpleExpr()), name)
        }
      } else {
        simpleExpr()
      }
    }
    def xmlLiteral(): Tree

    /* SimpleExpr    ::= new (ClassTemplate | TemplateBody)
     *                |  BlockExpr
     *                |  SimpleExpr1 [`_']
     * SimpleExpr1   ::= literal
     *                |  xLiteral
     *                |  Path
     *                |  `(' [Exprs [`,']] `)'
     *                |  SimpleExpr `.' Id
     *                |  SimpleExpr TypeArgs
     *                |  SimpleExpr1 ArgumentExprs
     */
    def simpleExpr(): Tree = {
      var canApply = true
      val t = in.token match {
        case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT |
             SYMBOLLIT | TRUE | FALSE | NULL =>
          atPos(in.offset) { literal(false) }
        case XMLSTART =>
          xmlLiteral()
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          path(true, false)
        case USCORE =>
          val start = in.offset
          val pname = freshName(o2p(start), "x$")
          val id = atPos(start) (Ident(pname))
          in.nextToken()
          val param = atPos(t2p(id)){ makeSyntheticParam(pname) }
          placeholderParams = param :: placeholderParams
          id
        case LPAREN =>
          atPos(in.skipToken()) {
            val ts = if (in.token == RPAREN) List() else exprs()
            accept(RPAREN)
            Parens(ts)
          }
        case LBRACE =>
          canApply = false
          blockExpr()
        case NEW =>
          canApply = false
          atPos(in.skipToken()) {
            val (parents, argss, self, stats) = template(false)
            makeNew(parents, self, stats, argss)
          }
        case _ =>
          syntaxErrorOrIncomplete("illegal start of simple expression", true)
          errorTermTree
      }
      simpleExprRest(t, canApply)
    }

    def simpleExprRest(t: Tree, canApply: Boolean): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case DOT =>
          in.nextToken()
          simpleExprRest(selector(stripParens(t)), true)
        case LBRACKET =>
          val t1 = stripParens(t)
          t1 match {
            case Ident(_) | Select(_, _) =>
              val tapp = atPos(t1.pos.start, in.offset) {
                TypeApply(t1, typeArgs(false, true))
              }
              simpleExprRest(tapp, true)
            case _ =>
              t1
          }
        case LPAREN | LBRACE if (canApply) =>
          val app = atPos(t.pos.start, in.offset) {
            // look for anonymous function application like (f _)(x) and
            // translate to (f _).apply(x), bug #460
            val sel = t match {
              case Parens(List(Typed(_, _: Function))) =>
                Select(stripParens(t), nme.apply)
              case _ =>
                stripParens(t)
            }
            Apply(sel, argumentExprs())
          }
          simpleExprRest(app, true)
        case USCORE =>
          atPos(t.pos.start, in.skipToken()) {
            Typed(stripParens(t), Function(List(), EmptyTree))
          }
        case _ =>
          t
      }
    }

    /** ArgumentExprs ::= `(' [Exprs [`,']] `)'
      *                 | [nl] BlockExpr
     */
    def argumentExprs(): List[Tree] = {
      // if arg has the form "x$1 => a = x$1" it's treated as "a = x$1" with x$1
      // in placeholderParams. This allows e.g. "val f: Int => Int = foo(a = 1, b = _)"
      def convertArg(arg: Tree): Tree = arg match {
        case Function(
          List(vd @ ValDef(mods, pname1, ptype1, EmptyTree)),
          Assign(Ident(aname), rhs)) if (mods hasFlag Flags.SYNTHETIC) =>
          rhs match {
            case Ident(`pname1`) | Typed(Ident(`pname1`), _) =>
              placeholderParams = vd :: placeholderParams
              atPos(arg.pos) { Assign(Ident(aname), Ident(pname1)) }
            case _ => arg
          }
        case _ => arg
      }

      if (in.token == LBRACE)
        List(blockExpr())
      else
        surround(LPAREN, RPAREN)(if (in.token == RPAREN) List() else (exprs() map convertArg), List())
    }

    /** BlockExpr ::= `{' (CaseClauses | Block) `}'
     */
    def blockExpr(): Tree = {
      val res = atPos(accept(LBRACE)) { // no need to surround
        if (in.token == CASE) Match(EmptyTree, caseClauses())
        else block()
      }
      accept(RBRACE)
      res
    }

    /** Block ::= BlockStatSeq
     *  @note  Return tree does not carry position.
     */
    def block(): Tree = {
      makeBlock(blockStatSeq(new ListBuffer[Tree]))
    }

   /** CaseClauses ::= CaseClause {CaseClause}
    */
    def caseClauses(): List[CaseDef] = {
      val ts = new ListBuffer[CaseDef]
      do { ts += caseClause()
      } while (in.token == CASE)
      ts.toList
    }

    /** CaseClause ::= case Pattern [Guard] `=>' Block
     */
    def caseClause(): CaseDef =
      atPos(accept(CASE)) {
        val pat = pattern()
        val gd = guard()
        makeCaseDef(pat, gd, caseBlock())
      }
    // IDE HOOK (so we can memoize case blocks) // needed?
    def caseBlock(): Tree =
      atPos(accept(ARROW))(block())

    /** Guard ::= if PostfixExpr
     */
    def guard(): Tree =
      if (in.token == IF) { in.nextToken(); stripParens(postfixExpr()) }
      else EmptyTree

    /** Enumerators ::= Generator {semi Enumerator}
     *  Enumerator  ::=  Generator
     *                |  Guard
     *                |  val Pattern1 `=' Expr
     */
    def enumerators(): List[Enumerator] = {
      val newStyle = in.token != VAL // todo: deprecate old style
      if (!newStyle)
        deprecationWarning(in.offset, "for (val x <- ... ) has been deprecated; use for (x <- ... ) instead")
      val enums = new ListBuffer[Enumerator]
      generator(enums, false)
      while (isStatSep) {
        in.nextToken()
        if (newStyle) {
          if (in.token == IF) enums += makeFilter(in.offset, guard())
          else generator(enums, true)
        } else {
          if (in.token == VAL) generator(enums, true)
          else enums += makeFilter(in.offset, expr())
        }
      }
      enums.toList
    }

    /** Generator ::= Pattern1 (`<-' | '=') Expr [Guard]
     */
    def generator(enums: ListBuffer[Enumerator], eqOK: Boolean) {
      val start = in.offset
      if (in.token == VAL) in.nextToken()
      val pat = pattern1(false)
      val point = in.offset
      val tok = in.token
      if (tok == EQUALS && eqOK) in.nextToken()
      else accept(LARROW)
      val rhs = expr()
      enums += makeGenerator(r2p(start, point, in.lastOffset), pat, tok == EQUALS, rhs)
      if (in.token == IF) enums += makeFilter(in.offset, guard())
    }

    def makeFilter(start: Int, tree: Tree) = Filter(r2p(start, tree.pos.point, tree.pos.end), tree)

/* -------- PATTERNS ------------------------------------------- */

    /**   Patterns ::= Pattern { `,' Pattern }
     *    SeqPatterns ::= SeqPattern { `,' SeqPattern }
     *
     *  (also eats trailing comma if it finds one)
     */
    def patterns(seqOK: Boolean): List[Tree] = {
      val ts = new ListBuffer[Tree] += pattern(seqOK)
      while (in.token == COMMA) {
        in.nextToken()
        ts += pattern(seqOK)
      }
      ts.toList
    }

    /**   Pattern  ::=  Pattern1 { `|' Pattern1 }
     *    SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
     */
    def pattern(seqOK: Boolean): Tree = {
      val start = in.offset
      val t = pattern1(seqOK)
      if (isIdent && in.name == BAR) {
        val ts = new ListBuffer[Tree] += t
        while (isIdent && in.name == BAR) {
          in.nextToken(); ts += pattern1(seqOK)
        }
        atPos(start) { makeAlternative(ts.toList) }
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
    def pattern1(seqOK: Boolean): Tree = {
      val p = pattern2(seqOK)
      p match {
        case Ident(name) if (treeInfo.isVarPattern(p) && in.token == COLON) =>
          atPos(p.pos.start, in.skipToken()) { Typed(p, compoundType(true)) }
        case _ =>
          p
      }
    }

    /*   Pattern2    ::=  varid [ @ Pattern3 ]
     *                |   Pattern3
     *   SeqPattern2 ::=  varid [ @ SeqPattern3 ]
     *                |   SeqPattern3
     */
    def pattern2(seqOK: Boolean): Tree = {
      val p = pattern3(seqOK)
      if (in.token == AT) {
        p match {
          case Ident(name) =>
            if (name == nme.WILDCARD) {
              in.nextToken()
              pattern3(seqOK)
            } else if (treeInfo.isVarPattern(p)) {
              in.nextToken()
              atPos(p.pos.start) { Bind(name, pattern3(seqOK)) }
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
    def pattern3(seqOK: Boolean): Tree = {
      val base = opstack
      var top = simplePattern(seqOK)
      if (seqOK && isIdent && in.name == STAR)
        return atPos(top.pos.start, in.skipToken())(Star(stripParens(top)))

      while (isIdent && in.name != BAR) {
        top = reduceStack(
          false, base, top, precedence(in.name), treeInfo.isLeftAssoc(in.name))
        val op = in.name
        opstack = OpInfo(top, op, in.offset) :: opstack
        ident()
        top = simplePattern(seqOK)
      }
      stripParens(reduceStack(false, base, top, 0, true))
    }

    def xmlLiteralPattern(): Tree

    /** SimplePattern    ::= varid
     *                    |  `_'
     *                    |  literal
     *                    |  XmlPattern
     *                    |  StableId  [TypeArgs] [`(' [SeqPatterns [`,']] `)']
     *                    |  `(' [Patterns [`,']] `)'
     *  SimpleSeqPattern ::= varid
     *                    |  `_'
     *                    |  literal
     *                    |  XmlPattern
     *                    |  `<' xLiteralPattern
     *                    |  StableId [TypeArgs] [`(' [SeqPatterns [`,']] `)']
     *                    |  `(' [SeqPatterns [`,']] `)'
     *
     * XXX: Hook for IDE
     */
    def simplePattern(seqOK: Boolean): Tree = {
      val start = in.offset
      in.token match {
        case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
          var t = stableId()
          in.token match {
            case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT =>
              t match {
                case Ident(name) if name == nme.MINUS =>
                  return atPos(start) { literal(true) }
                case _ =>
              }
            case _ =>
          }
          /* not yet
           if (in.token == LBRACKET)
           atPos(in.offset) {
           val ts = typeArgs(true, false)
           accept(LPAREN)
           val ps = if (in.token == RPAREN) List() else patterns(true, false)
           accept(RPAREN)
           Apply(TypeApply(convertToTypeId(t), ts), ps)
           }
           else */
          if (in.token == LPAREN)
            atPos(start, in.offset) { Apply(t, argumentPatterns()) }
          else t
        case USCORE =>
          in.nextToken()
          atPos(start) { Ident(nme.WILDCARD) }
        case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
             STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL =>
          atPos(start) { literal(false) }
        case LPAREN =>
          in.nextToken()
          atPos(start) {
            val ps = if (in.token == RPAREN) List() else patterns(false)
            accept(RPAREN)
            Parens(ps)
          }
        case XMLSTART =>
          xmlLiteralPattern()
        case _ =>
          syntaxErrorOrIncomplete("illegal start of simple pattern", true)
          errorPatternTree
      }
    }

    def argumentPatterns(): List[Tree] = {
      accept(LPAREN)
      val ps = if (in.token == RPAREN) List() else patterns(true)
      accept(RPAREN)
      ps
    }

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    /** Drop `private' modifier when follwed by a qualifier.
     *  Conract `abstract' and `override' to ABSOVERRIDE
     */
    private def normalize(mods: Modifiers): Modifiers =
      if ((mods hasFlag Flags.PRIVATE) && mods.privateWithin != nme.EMPTY.toTypeName)
        mods &~ Flags.PRIVATE
      else if ((mods hasFlag Flags.ABSTRACT) && (mods hasFlag Flags.OVERRIDE))
        mods &~ (Flags.ABSTRACT | Flags.OVERRIDE) | Flags.ABSOVERRIDE
      else
        mods

    private def addMod(mods: Modifiers, mod: Long): Modifiers = {
      if (mods hasFlag mod) syntaxError(in.offset, "repeated modifier", false)
      in.nextToken()
      mods | mod
    }

    /** AccessQualifier ::= "[" (Id | this) "]"
     */
    def accessQualifierOpt(mods: Modifiers): Modifiers = {
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
        case LAZY =>
          loop(addMod(mods, Flags.LAZY))
        case NEWLINE =>
          in.nextToken()
          loop(mods)
        case _ =>
          mods
      }
      loop(NoMods)
    }

    /** LocalModifiers ::= {LocalModifier}
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
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
        case LAZY =>
          loop(addMod(mods, Flags.LAZY))
        case _ =>
          mods
      }
      loop(NoMods)
    }

    /** Annotations      ::= {`@' SimpleType {ArgumentExprs}}
     *  ConsrAnnotations ::= {`@' SimpleType ArgumentExprs}
     */
    def annotations(skipNewLines: Boolean, requireOneArgList: Boolean): List[Tree] = {
      var annots = new ListBuffer[Tree]
      while (in.token == AT) {
        in.nextToken()
        annots += annotationExpr(requireOneArgList)
        if (skipNewLines) newLineOpt()
      }
      annots.toList
    }

    def annotationExpr(requireOneArgList: Boolean): Tree = {
      atPos(in.offset) {
        val t = simpleType(false)
        val argss = new ListBuffer[List[Tree]]
        if (requireOneArgList)
          argss += argumentExprs()
        else if (in.token == LPAREN)
          do { argss += argumentExprs() } while (in.token == LPAREN)
        else argss += List()
        New(t, argss.toList)
      }
    }

/* -------- PARAMETERS ------------------------------------------- */

    /** ParamClauses      ::= {ParamClause} [[nl] `(' implicit Params `)']
     *  ParamClause       ::= [nl] `(' [Params] ')'
     *  Params            ::= Param {`,' Param}
     *  Param             ::= {Annotation} Id [`:' ParamType] [`=' Expr]
     *  ClassParamClauses ::= {ClassParamClause} [[nl] `(' implicit ClassParams `)']
     *  ClassParamClause  ::= [nl] `(' [ClassParams] ')'
     *  ClassParams       ::= ClassParam {`,' ClassParam}
     *  ClassParam        ::= {Annotation}  [{Modifier} (`val' | `var')] Id [`:' ParamType] [`=' Expr]
     */
    def paramClauses(owner: Name, implicitViews: List[Tree], ofCaseClass: Boolean): List[List[ValDef]] = {
      var implicitmod = 0
      var caseParam = ofCaseClass
      def param(): ValDef = {
        val start = in.offset
        val annots = annotations(false, false)
        var mods = Modifiers(Flags.PARAM)
        if (owner.isTypeName) {
          mods = modifiers() | Flags.PARAMACCESSOR
          if (mods.hasFlag(Flags.LAZY)) syntaxError("lazy modifier not allowed here. Use call-by-name parameters instead", false)
          if (in.token == VAL) {
            in.nextToken()
          } else if (in.token == VAR) {
            mods |= Flags.MUTABLE
            in.nextToken()
          } else if (!caseParam) {
            mods |= Flags.PRIVATE | Flags.LOCAL
          }
          if (caseParam) {
            mods |= Flags.CASEACCESSOR
          }
        }
        val nameOffset = in.offset
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
                  in.offset,
                  (if (mods.hasFlag(Flags.MUTABLE)) "`var'" else "`val'") +
                  " parameters may not be call-by-name", false)
              else bynamemod = Flags.BYNAMEPARAM
            }
            paramType()
          }
        val default =
          if (in.token == EQUALS) {
            in.nextToken()
            mods |= Flags.DEFAULTPARAM
            expr()
          } else EmptyTree
        atPos(start, if (name == nme.ERROR) start else nameOffset) {
          ValDef((mods | implicitmod | bynamemod) withAnnotations annots, name, tpt, default)
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
      val start = in.offset
      newLineOptWhenFollowedBy(LPAREN)
      if (ofCaseClass && in.token != LPAREN)
        deprecationWarning(in.offset, "case classes without a parameter list have been deprecated;\n"+
                           "use either case objects or case classes with `()' as parameter list.")
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
          syntaxError(in.offset, "no type parameters allowed here", false)
        else if(in.token == EOF)
          incompleteInputError("auxiliary constructor needs non-implicit parameter list")
        else
          syntaxError(start, "auxiliary constructor needs non-implicit parameter list", false)
      addImplicitViews(owner, result, implicitViews)
    }

    /** ParamType ::= Type | `=>' Type | Type `*'
     */
    def paramType(): Tree =
      if (in.token == ARROW)
        atPos(in.skipToken()) {
          AppliedTypeTree(
            rootScalaDot(nme.BYNAME_PARAM_CLASS_NAME.toTypeName), List(typ()))
        }
      else {
        val t = typ()
        if (isIdent && in.name == STAR) {
          in.nextToken()
          atPos(t.pos.start, t.pos.point) {
            AppliedTypeTree(
              rootScalaDot(nme.REPEATED_PARAM_CLASS_NAME.toTypeName), List(t))
          }
        } else t
      }

    /** TypeParamClauseOpt    ::= [TypeParamClause]
     *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
     *  VariantTypeParam      ::= {Annotation} [`+' | `-'] TypeParam
     *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
     *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
     *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds [<% Type]
     */
    def typeParamClauseOpt(owner: Name, implicitViewBuf: ListBuffer[Tree]): List[TypeDef] = {
      def typeParam(ms: Modifiers): TypeDef = {
        var mods = ms | Flags.PARAM
        val start = in.offset
        if (owner.isTypeName && isIdent) {
          if (in.name == PLUS) {
            in.nextToken()
            mods |= Flags.COVARIANT
          } else if (in.name == MINUS) {
            in.nextToken()
            mods |= Flags.CONTRAVARIANT
          }
        }
        val nameOffset = in.offset
        val pname =
          (if (in.token == USCORE) { // @M! also allow underscore
            in.nextToken()
            nme.WILDCARD
          } else ident()).toTypeName
        val param = atPos(start, nameOffset) {
          val tparams = typeParamClauseOpt(pname, null) // @M TODO null --> no higher-order view bounds for now
          TypeDef(mods, pname, tparams, typeBounds())
        }
        if (in.token == VIEWBOUND && (implicitViewBuf ne null))
          implicitViewBuf += atPos(start, in.skipToken()) {
            makeFunctionTypeTree(List(Ident(pname)), typ())
          }
        param
      }
      val params = new ListBuffer[TypeDef]
      newLineOptWhenFollowedBy(LBRACKET)
      if (in.token == LBRACKET) {
        in.nextToken()
        params += typeParam(NoMods.withAnnotations(annotations(true, false)))
        while (in.token == COMMA) {
          in.nextToken()
          params += typeParam(NoMods.withAnnotations(annotations(true, false)))
        }
        accept(RBRACKET)
      }
      params.toList
    }

    /** TypeBounds ::= [`>:' Type] [`<:' Type]
     */
    def typeBounds(): TypeBoundsTree =
      TypeBoundsTree(
        bound(SUPERTYPE, nme.Nothing),
        bound(SUBTYPE, nme.Any))

    def bound(tok: Int, default: Name): Tree =
      if (in.token == tok) { in.nextToken(); typ() }
      else rootScalaDot(default.toTypeName)

/* -------- DEFS ------------------------------------------- */


    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    def importClause(): List[Tree] = {
      accept(IMPORT)
      val ts = new ListBuffer[Tree] += importExpr()
      while (in.token == COMMA) {
        in.nextToken(); ts += importExpr()
      }
      ts.toList
    }

    /**  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
     * XXX: Hook for IDE
     */
    def importExpr(): Tree = {
      val start = in.offset
      var t: Tree = null
      if (in.token == THIS) {
        in.nextToken()
        t = atPos(start) { This(nme.EMPTY.toTypeName) }
        accept(DOT)
        t = selector(t)
        accept(DOT)
      } else {
        val id = atPos(start) { Ident(ident()) }
        accept(DOT)
        if (in.token == THIS) {
          in.nextToken()
          t = atPos(start) { This(id.name.toTypeName) }
          accept(DOT)
          t = selector(t)
          accept(DOT)
        } else {
          t = id
        }
      }
      def loop(): Tree =
        if (in.token == USCORE) {
          in.nextToken()
          Import(t, List((nme.WILDCARD, null)))
        } else if (in.token == LBRACE) {
          Import(t, importSelectors())
        } else {
          val nameOffset = in.offset
          val name = ident()
          if (in.token == DOT) {
            t = atPos(start, if (name == nme.ERROR) in.offset else nameOffset) {
              Select(t, name)
            }
            in.nextToken()
            loop()
          } else {
            Import(t, List((name, name)))
          }
        }
      atPos(start) { loop() }
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
    def importSelector(names: ListBuffer[(Name, Name)]): Boolean =
      if (in.token == USCORE) {
        in.nextToken(); names += ((nme.WILDCARD, null)); true
      } else {
        val name = ident()
        names += ((
          name,
          if (in.token == ARROW) {
            in.nextToken()
            if (in.token == USCORE) { in.nextToken(); nme.WILDCARD } else ident()
          } else {
            name
          }))
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
    def defOrDcl(mods: Modifiers): List[Tree] = {
      if ((mods hasFlag Flags.LAZY) && in.token != VAL)
        syntaxError("lazy not allowed here. Only vals can be lazy", false)
      in.token match {
        case VAL =>
          patDefOrDcl(mods)
        case VAR =>
          patDefOrDcl(mods | Flags.MUTABLE)
        case DEF =>
          List(funDefOrDcl(mods))
        case TYPE =>
          List(typeDefOrDcl(mods))
        case _ =>
          List(tmplDef(mods))
      }
    }
    /** IDE hook: for non-local defs or dcls with modifiers and annotations */
    def nonLocalDefOrDcl : List[Tree] = {
      val annots = annotations(true, false)
      defOrDcl(modifiers() withAnnotations annots)
    }
    /** not hooked by the IDE, will not undergo stubbing. Used for early initialization blocks. */
    def preNonLocalDefOrDcl : List[Tree] = {
      val annots = annotations(true, false)
      defOrDcl(modifiers() withAnnotations annots)
    }


    /** PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  ValDcl ::= Id {`,' Id} `:' Type
     *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
     */
    def patDefOrDcl(mods: Modifiers): List[Tree] = {
      var newmods = mods
      val lhsBuf = new ListBuffer[Tree]
      do {
        in.nextToken()
        val p = pattern2(false)
        lhsBuf += stripParens(p)
      } while (in.token == COMMA)
      val lhs = lhsBuf.toList
      val tp = typedOpt()
      var rhs =
        if (tp.isEmpty || in.token == EQUALS) {
          accept(EQUALS)
          if (!tp.isEmpty && newmods.hasFlag(Flags.MUTABLE) &&
              (lhs.toList forall (_.isInstanceOf[Ident])) && in.token == USCORE) {
            in.nextToken()
            newmods = newmods | Flags.DEFAULTINIT
            EmptyTree
          } else {
            expr()
          }
        } else {
          newmods = newmods | Flags.DEFERRED
          EmptyTree
        }
      def mkDefs(p: Tree): List[Tree] = {
        //Console.println("DEBUG: p = "+p.toString()); // DEBUG
        val trees =
          makePatDef(newmods, if (tp.isEmpty) p else Typed(p, tp), rhs) map
            atPos(p.pos.start, p.pos.point)
        rhs = rhs.syntheticDuplicate
        if (newmods hasFlag Flags.DEFERRED) {
          trees match {
            case List(ValDef(_, _, _, EmptyTree)) =>
              if (mods.hasFlag(Flags.LAZY)) syntaxError(p.pos, "lazy values may not be abstract", false)
            case _ => syntaxError(p.pos, "pattern definition may not be abstract", false)
          }
        }
        trees
      }
      for (p <- lhs.toList; d <- mkDefs(p)) yield d
    }

    /** VarDef ::= PatDef
     *           | Id {`,' Id} `:' Type `=' `_'
     *  VarDcl ::= Id {`,' Id} `:' Type
    def varDefOrDcl(mods: Modifiers): List[Tree] = {
      var newmods = mods | Flags.MUTABLE
      val lhs = new ListBuffer[(Int, Name)]
      do {
        in.nextToken()
        lhs += (in.offset, ident())
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
    }
     */

    /** FunDef ::= FunSig `:' Type `=' Expr
     *           | FunSig [nl] `{' Block `}'
     *           | this ParamClause ParamClauses (`=' ConstrExpr | [nl] ConstrBlock)
     *  FunDcl ::= FunSig [`:' Type]
     *  FunSig ::= id [FunTypeParamClause] ParamClauses
     */
    def funDefOrDcl(mods: Modifiers): Tree = {
      val start = in.skipToken()
      if (in.token == THIS) {
        atPos(start, in.skipToken()) {
          val vparamss = paramClauses(nme.CONSTRUCTOR, implicitClassViews map (_.syntheticDuplicate), false)
          newLineOptWhenFollowedBy(LBRACE)
          val rhs = if (in.token == LBRACE) constrBlock(vparamss)
                    else { accept(EQUALS); constrExpr(vparamss) }
          DefDef(mods, nme.CONSTRUCTOR, List(), vparamss, TypeTree(), rhs)
        }
      } else {
        var newmods = mods
        val nameOffset = in.offset
        val name = ident()
        atPos(start, if (name == nme.ERROR) start else nameOffset) {
          // implicitViewBuf is for view bounded type parameters of the form
          // [T <% B]; it contains the equivalent implicit parameter, i.e. (implicit p: T => B)
          val implicitViewBuf = new ListBuffer[Tree]
          val tparams = typeParamClauseOpt(name, implicitViewBuf)
          val vparamss = paramClauses(name, implicitViewBuf.toList, false)
          newLineOptWhenFollowedBy(LBRACE)
          var restype = typedOpt()
          val rhs =
            if (isStatSep || in.token == RBRACE) {
              if (restype.isEmpty) restype = scalaUnitConstr
              newmods |= Flags.DEFERRED
              EmptyTree
            } else if (restype.isEmpty && in.token == LBRACE) {
              restype = scalaUnitConstr
              blockExpr()
            } else {
              equalsExpr()
            }
          DefDef(newmods, name, tparams, vparamss, restype, rhs)
        }
      }
    }

    /** ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     */
    def constrExpr(vparamss: List[List[ValDef]]): Tree =
      if (in.token == LBRACE) constrBlock(vparamss)
      else Block(List(selfInvocation(vparamss)), Literal(()))

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
        if (implicitClassViews.isEmpty) t
        else Apply(t, vparamss.last.map(vp => Ident(vp.name)))
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
        Block(stats, Literal(()))
      }

    /** TypeDef ::= type Id [TypeParamClause] `=' Type
     *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
     */
    def typeDefOrDcl(mods: Modifiers): Tree = {
      val start = in.skipToken()
      newLinesOpt()
      atPos(start, in.offset) {
        val name = ident().toTypeName
        // @M! a type alias as well as an abstract type may declare type parameters
        val tparams = typeParamClauseOpt(name, null)
        in.token match {
          case EQUALS =>
            in.nextToken()
            TypeDef(mods, name, tparams, typ())
          case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE =>
            TypeDef(mods | Flags.DEFERRED, name, tparams, typeBounds())
          case _ =>
            syntaxErrorOrIncomplete("`=', `>:', or `<:' expected", true)
            EmptyTree
        }
      }
    }

    /** Hook for IDE, for top-level classes/objects */
    def topLevelTmplDef: Tree = {
      val annots = annotations(true, false)
      val mods = modifiers() withAnnotations annots
      tmplDef(mods)
    }

    /**  TmplDef ::= [case] class ClassDef
     *            |  [case] object ObjectDef
     *            |  [override] trait TraitDef
     */
    def tmplDef(mods: Modifiers): Tree = {
      if (mods.hasFlag(Flags.LAZY)) syntaxError("classes cannot be lazy", false)
      in.token match {
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
    }

    /** ClassDef ::= Id [TypeParamClause] {Annotation}
                     [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
     *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
     */
    def classDef(mods: Modifiers): ClassDef = {
      val start = in.skipToken()
      val nameOffset = in.offset
      val name = ident().toTypeName
      atPos(start, if (name == nme.ERROR.toTypeName) start else nameOffset) {
        val savedViews = implicitClassViews
        val implicitViewBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, implicitViewBuf)
        implicitClassViews = implicitViewBuf.toList
        if (!implicitClassViews.isEmpty && mods.hasFlag(Flags.TRAIT)) {
          syntaxError("traits cannot have type parameters with <% bounds", false)
          implicitClassViews = List()
        }
        val constrAnnots = annotations(false, true)
        val (constrMods, vparamss) =
          if (mods.hasFlag(Flags.TRAIT)) (Modifiers(Flags.TRAIT), List())
          else (accessModifierOpt(), paramClauses(name, implicitClassViews, mods.hasFlag(Flags.CASE)))
        var mods1 = mods
        if (mods hasFlag Flags.TRAIT) {
          if (settings.Xexperimental.value && in.token == SUBTYPE) mods1 |= Flags.DEFERRED
        } else if (in.token == SUBTYPE) {
          syntaxError("classes are not allowed to be virtual", false)
        }
        var template = templateOpt(mods1, name, constrMods withAnnotations constrAnnots, vparamss)
        if (isInterface(mods1, template.body)) mods1 |= Flags.INTERFACE
        val result = ClassDef(mods1, name, tparams, template)
        implicitClassViews = savedViews
        result
      }
    }

    /** ObjectDef       ::= Id ClassTemplateOpt
     */
    def objectDef(mods: Modifiers): ModuleDef = {
      val start = in.skipToken()
      val nameOffset = in.offset
      val name = ident()
      atPos(start, if (name == nme.ERROR) start else nameOffset) {
        val mods1 = if (in.token == SUBTYPE) mods | Flags.DEFERRED else mods
        val template = templateOpt(mods1, name, NoMods, List())
        ModuleDef(mods1, name, template)
      }
    }

    /** ClassParents       ::= AnnotType {`(' [Exprs [`,']] `)'} {with AnnotType}
     *  TraitParents       ::= AnnotType {with AnnotType}
     */
    def templateParents(isTrait: Boolean): (List[Tree], List[List[Tree]]) = {
      val parents = new ListBuffer[Tree] += annotType(false)
      val argss = new ListBuffer[List[Tree]]
      if (in.token == LPAREN && !isTrait)
        do { argss += argumentExprs() } while (in.token == LPAREN)
      else argss += List()
      while (in.token == WITH) {
        in.nextToken()
        parents += annotType(false)
      }
      (parents.toList, argss.toList)
    }

    /** ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
     *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
     *  EarlyDefs     ::= `{' [EarlyDef {semi EarlyDef}] `}'
     *  EarlyDef      ::= Annotations Modifiers PatDef
     */
    def template(isTrait: Boolean): (List[Tree], List[List[Tree]], ValDef, List[Tree]) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        // @S: pre template body cannot stub like post body can!
        val (self, body) = templateBody(true)
        if (in.token == WITH && self.isEmpty) {
          val earlyDefs: List[Tree] = body flatMap {
            case vdef @ ValDef(mods, name, tpt, rhs) if !(mods hasFlag Flags.DEFERRED) =>
              List(treeCopy.ValDef(vdef, mods | Flags.PRESUPER, name, tpt, rhs))
            case tdef @ TypeDef(mods, name, tparams, rhs) =>
              List(treeCopy.TypeDef(tdef, mods | Flags.PRESUPER, name, tparams, rhs))
            case stat if !stat.isEmpty =>
              syntaxError(stat.pos, "only type definitions and concrete field definitions allowed in early object initialization section", false)
              List()
            case _ => List()
          }
          in.nextToken()
          val (parents, argss) = templateParents(isTrait)
          val (self1, body1) = templateBodyOpt(isTrait)
          (parents, argss, self1, earlyDefs ::: body1)
        } else {
          (List(), List(List()), self, body)
        }
      } else {
        val (parents, argss) = templateParents(isTrait)
        val (self, body) = templateBodyOpt(isTrait)
        (parents, argss, self, body)
      }
    }

    def isInterface(mods: Modifiers, body: List[Tree]): Boolean =
      (mods hasFlag Flags.TRAIT) && (body forall treeInfo.isInterfaceMember)

    /** ClassTemplateOpt ::= 'extends' ClassTemplate | [['extends'] TemplateBody]
     *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [['extends'] TemplateBody] | '<:' TemplateBody
     *  TraitExtends     ::= 'extends' | `<:'
     *  @note leaves result unpositioned.
     */
    def templateOpt(mods: Modifiers, name: Name, constrMods: Modifiers,
                    vparamss: List[List[ValDef]]): Template = {
      val (parents0, argss, self, body) =
        if (in.token == EXTENDS || settings.Xexperimental.value && (mods hasFlag Flags.TRAIT) && in.token == SUBTYPE) {
          in.nextToken()
          template(mods hasFlag Flags.TRAIT)
        } else if ((in.token == SUBTYPE) && (mods hasFlag Flags.TRAIT)) {
	  in.nextToken()
          template(true)
	} else {
          newLineOptWhenFollowedBy(LBRACE)
          val (self, body) = templateBodyOpt(false)
          (List(), List(List()), self, body)
        }
      var parents = parents0
      if (name != nme.ScalaObject.toTypeName && !isInterface(mods, body))
        parents = parents ::: List(scalaScalaObjectConstr)
      if (parents.isEmpty)
        parents = List(scalaAnyRefConstr)
      if (mods.hasFlag(Flags.CASE)) parents = parents ::: List(productConstr)
      Template(parents, self, constrMods, vparamss, argss, body)
    }

/* -------- TEMPLATES ------------------------------------------- */

    /** TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateBody(isPre: Boolean) = {
      accept(LBRACE)
      val result @ (self, stats) = templateStatSeq(isPre)
      accept(RBRACE)
      if (stats.isEmpty) (self, List(EmptyTree)) else result
    }
    def templateBodyOpt(traitParentSeen: Boolean): (ValDef, List[Tree]) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        templateBody(false)
      } else {
        if (in.token == LPAREN)
          syntaxError((if (traitParentSeen) "parents of traits" else "traits or objects")+
                      " may not have parameters", true)
        (emptyValDef, List())
      }
    }

    /** Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
     */
    def refinement(): List[Tree] = {
      accept(LBRACE)
      val body = refineStatSeq()
      accept(RBRACE)
      body
    }

/* -------- STATSEQS ------------------------------------------- */

  /** Create a tree representing a packaging */
    def makePackaging(start: Int, pkg: Tree, stats: List[Tree]): PackageDef =
      atPos(start, pkg.pos.point) {
        pkg match {
          case Ident(name) =>
            PackageDef(name, stats)
          case Select(qual, name) =>
            makePackaging(start, qual, List(PackageDef(name, stats)))
        }
      }

    /** Create a tree representing a package object, converting
     *    package object foo { ... }
     *  to
     *    package foo {
     *      object `package` { ... }
     *    }
     */
    def makePackageObject(start: Int, objDef: ModuleDef): PackageDef = objDef match {
      case ModuleDef(mods, name, impl) =>
        makePackaging(
          start, atPos(o2p(objDef.pos.start)){ Ident(name) }, List(ModuleDef(mods, nme.PACKAGEkw, impl)))
    }

    /** Packaging ::= package QualId [nl] `{' TopStatSeq `}'
     */
    def packaging(start: Int): Tree = {
      val pkg = qualId()
      newLineOptWhenFollowedBy(LBRACE)
      accept(LBRACE)
      val stats = topStatSeq()
      accept(RBRACE)
      makePackaging(start, pkg, stats)
    }

    /** TopStatSeq ::= TopStat {semi TopStat}
     *  TopStat ::= Annotations Modifiers TmplDef
     *            | Packaging
     *            | package object objectDef
     *            | Import
     *            |
     */
    def topStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (in.token != RBRACE && in.token != EOF) {
        if (in.token == PACKAGE) {
          val start = in.skipToken()
	  stats += {
	    if (in.token == OBJECT) makePackageObject(start, objectDef(NoMods))
	    else packaging(start)
          }
        } else if (in.token == IMPORT) {
          stats ++= importClause()
          // XXX: IDE hook this all.
        } else if (in.token == CLASS ||
                   in.token == CASECLASS ||
                   in.token == TRAIT ||
                   in.token == OBJECT ||
                   in.token == CASEOBJECT ||
                   in.token == LBRACKET || //todo: remove
                   in.token == AT ||
                   isModifier) {
          stats ++ joinComment(List(topLevelTmplDef))
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
     *                     | super ArgumentExprs {ArgumentExprs}
     *                     |
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateStatSeq(isPre : Boolean) = checkNoEscapingPlaceholders {
      var self: ValDef = emptyValDef
      val stats = new ListBuffer[Tree]
      if (isExprIntro) {
        val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
        if (in.token == ARROW) {
          first match {
            case Typed(tree @ This(name), tpt) if (name == nme.EMPTY.toTypeName) =>
              self = atPos(tree.pos) { makeSelfDef(nme.WILDCARD, tpt) }
            case _ =>
              convertToParam(first) match {
                case tree @ ValDef(_, name, tpt, EmptyTree) if (name != nme.ERROR) =>
                  self = atPos(tree.pos) { makeSelfDef(name, tpt) }
                case _ =>
              }
          }
          in.nextToken()
        } else {
          stats += first
          if (in.token != RBRACE && in.token != EOF/* !isStatSep(in.token)*/) acceptStatSep()
        }
      }
      while (in.token != RBRACE && in.token != EOF) {
        if (in.token == IMPORT) {
          stats ++= importClause()
        } else if (isExprIntro) {
          stats += statement(InTemplate)
        } else if (isDefIntro || isModifier || in.token == LBRACKET /*todo: remove */ || in.token == AT) {
          if (isPre) // @S: avoid caching by calling a different method that does the same thing (except in the IDE)
            stats ++= joinComment(preNonLocalDefOrDcl)
          else stats ++= joinComment(nonLocalDefOrDcl)
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
    def refineStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      while (in.token != RBRACE && in.token != EOF) {
        if (isDclIntro) { // don't IDE hook
          stats ++= joinComment(defOrDcl(NoMods))
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("illegal start of declaration", true)
        }
        if (in.token != RBRACE) acceptStatSep()
      }
      stats.toList
    }

    /** overridable IDE hook for local definitions of blockStatSeq
     *  Here's an idea how to fill in start and end positions.
    def localDef : List[Tree] = {
      atEndPos {
        atStartPos(in.offset) {
          val annots = annotations(true)
          val mods = localModifiers() withAnnotations annots
          if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(mods)
          else List(tmplDef(mods))
        }
      } (in.offset)
    }
    */

    def localDef : List[Tree] = {
      val annots = annotations(true, false)
      val mods = localModifiers() withAnnotations annots
      if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(mods)
      else List(tmplDef(mods))
    }

    /** BlockStatSeq ::= { BlockStat semi } [ResultExpr]
     *  BlockStat    ::= Import
     *                 | Annotations [implicit] [lazy] Def
     *                 | Annotations LocalModifiers TmplDef
     *                 | Expr1
     *                 |
     */
    def blockStatSeq(stats: ListBuffer[Tree]): List[Tree] = checkNoEscapingPlaceholders {
      while ((in.token != RBRACE) && (in.token != EOF) && (in.token != CASE)) {
        if (in.token == IMPORT) {
          stats ++= importClause()
          acceptStatSep()
        } else if (isExprIntro) {
          stats += statement(InBlock)
          if (in.token != RBRACE && in.token != CASE) acceptStatSep()
        } else if (isDefIntro || isLocalModifier || in.token == AT) {
          stats ++= localDef
          if (in.token == RBRACE || in.token == CASE) {
            syntaxError("block must end in result expression, not in definition", false)
            stats += Literal(()).setPos(o2p(in.offset))
          } else acceptStatSep()
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
    def compilationUnit(): Tree = checkNoEscapingPlaceholders {
      val ts = new ListBuffer[Tree]

      // @S: the IDE can insert phantom semi-colons before package during editing
      // @S: just eat them (doesn't really change the grammar)
      while (in.token == SEMI) in.nextToken()
      val start = in.offset
      if (in.token == PACKAGE) {
        in.nextToken()
	if (in.token == OBJECT) {
	  ts += makePackageObject(start, objectDef(NoMods))
	  if (in.token != EOF) {
	    acceptStatSep()
	    ts ++= topStatSeq()
	  }
	} else {
          val pkg = qualId()
          newLineOptWhenFollowedBy(LBRACE)
          if (in.token == EOF) {
            ts += makePackaging(start, pkg, List())
          } else if (isStatSep) {
            in.nextToken()
            ts += makePackaging(start, pkg, topStatSeq())
          } else {
            accept(LBRACE)
            ts += makePackaging(start, pkg, topStatSeq())
              accept(RBRACE)
              ts ++= topStatSeq()
            }
	  }
        } else {
          ts ++= topStatSeq()
        }
        val stats = ts.toList
        atPos(start) {
          stats match {
            case List(stat @ PackageDef(_, _)) => stat
            case _ => makePackaging(start, atPos(o2p(start)) { Ident(nme.EMPTY_PACKAGE_NAME) }, stats)
          }
        }
    }
  }
}
