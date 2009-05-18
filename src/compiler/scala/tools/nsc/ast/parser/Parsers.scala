/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: Parsers.scala 17415 2009-03-31 13:38:18Z imaier $
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
  val global : Global
  import global._

  private val glob: global.type = global
  import global.posAssigner.atPos

  case class OpInfo(operand: Tree, operator: Name, pos: Int)

  class UnitParser(val unit: global.CompilationUnit) extends Parser {
    val in = new UnitScanner(unit)
    in.init()

    def freshName(pos: Position, prefix: String): Name =
      unit.fresh.newName(pos, prefix)

    implicit def i2p(offset: Int): Position = new OffsetPosition(unit.source,offset)

    def warning(pos: Int, msg: String) { unit.warning(pos, msg) }

    def incompleteInputError(msg: String) {
      unit.incompleteInputError(unit.source.asInstanceOf[BatchSourceFile].content.length - 1, msg)
    }
    def deprecationWarning(pos: Int, msg: String) {
      unit.deprecationWarning(pos, msg)
    }
    def syntaxError(pos: Int, msg: String) { unit.error(pos, msg) }

    /** the markup parser */
    lazy val xmlp = new MarkupParser(this, true)

    object symbXMLBuilder extends SymbolicXMLBuilder(treeBuilder, this, true) { // DEBUG choices
      val global: self.global.type = self.global
      def freshName(prefix: String): Name = UnitParser.this.freshName(NoPosition, prefix)
    }
    def xmlLiteral : Tree = xmlp.xLiteral
    def xmlLiteralPattern : Tree = xmlp.xLiteralPattern
  }

  class ScanOnly(unit: global.CompilationUnit) extends UnitParser(unit) {
    override def parse(): Tree = {
      while (in.token != EOF) in.nextToken
      null
    }
  }

  // parser constants, here so they don't pollute parser debug listing
  private object ParserConfiguration {
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
  }

  abstract class Parser {
    ParserConfiguration.hashCode
    import ParserConfiguration._
    val in: Scanner
    //val unit : CompilationUnit
    //import in.ScanPosition
    protected def freshName(pos: Position, prefix: String): Name
    protected def posToReport: Int = in.offset

    protected implicit def i2p(offset: Int): Position
    //private implicit def p2i(pos: Position) = pos.offset.get

    private def inToken = in.token
    private def inSkipToken = in.skipToken()
    private def inNextToken = in.nextToken()
    private def inCurrentPos = in.offset
    private def inNextTokenCode : Int = in.next.token
    private def inName = in.name
    private def charVal = in.charVal
    private def intVal(isNegated: Boolean) = in.intVal(isNegated).asInstanceOf[Int]
    private def longVal(isNegated: Boolean) = in.intVal(isNegated)
    private def floatVal(isNegated: Boolean) = in.floatVal(isNegated).asInstanceOf[Float]
    private def doubleVal(isNegated: Boolean) = in.floatVal(isNegated)
    private def stringVal = in.strVal

    /** whether a non-continuable syntax error has been seen */
    //private var syntaxErrorSeen = false
    private var lastErrorPos : Int = -1

    object treeBuilder extends TreeBuilder {
      val global: self.global.type = self.global
      def freshName(pos : Position, prefix: String): Name =
        Parser.this.freshName(pos, prefix)
    }
    import treeBuilder._

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
        t = ExistentialTypeTree(t, placeholderTypes.reverse)
        placeholderTypes = List()
      }
      placeholderTypes = placeholderTypes ::: savedPlaceholderTypes
      t
    }

/* ------------- ERROR HANDLING ------------------------------------------- */

    protected def skip() {
      var nparens = 0
      var nbraces = 0
      while (true) {
        inToken match {
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
        inNextToken
      }
    }
    def warning(pos: Int, msg: String): Unit
    def incompleteInputError(msg: String): Unit
    def deprecationWarning(pos: Int, msg: String): Unit
    private def syntaxError(pos: Position, msg: String, skipIt: Boolean) {
      pos.offset match {
        case None => syntaxError(msg,skipIt)
        case Some(offset) => syntaxError(offset, msg, skipIt)
      }
    }
    def syntaxError(pos: Int, msg: String): Unit
    def syntaxError(msg: String, skipIt: Boolean) {
      syntaxError(inCurrentPos, msg, skipIt)
    }

    def syntaxError(pos: Int, msg: String, skipIt: Boolean) {
      if (pos > lastErrorPos) {
        syntaxError(pos, msg)
        // no more errors on this token.
        lastErrorPos = inCurrentPos
      }
      if (skipIt)
        skip()
    }

    def warning(msg: String) { warning(inCurrentPos, msg) }

    def syntaxErrorOrIncomplete(msg: String, skipIt: Boolean) {
      val inToken = this.inToken
      if (inToken == EOF)
        incompleteInputError(msg)
      else
        syntaxError(inCurrentPos, msg, skipIt)
    }
    // unused.
    /* Commented out because the comment says it is unused.
       Probably eliminate eventually.  GAW 2008.05.01
    def mismatch(expected: Int, found: Int) {
      val posToReport = this.posToReport
      val msg =
        ScannerConfiguration.token2string(expected) + " expected but " +
        ScannerConfiguration.token2string(found) + " found."

      if (found == EOF)
        incompleteInputError(msg)
      else
        syntaxError(posToReport, msg, true)
    }
    */

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      */
    def accept(token: Int): Int = {
      val pos = inCurrentPos
      if (inToken != token) {
        val posToReport =
          //if (inCurrentPos.line(unit.source).get(0) > in.lastPos.line(unit.source).get(0))
          //  in.lastPos
          //else
            inCurrentPos
        val msg =
          token2string(token) + " expected but " +token2string(inToken) + " found."

        if (inToken == EOF)
          incompleteInputError(msg)
        else
          syntaxError(posToReport, msg, true)
      }
      if (inToken == token) inNextToken
      pos
    }
    def surround[T](open: Int, close: Int)(f: => T, orElse: T): T = {
      val wasOpened = inToken == open
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
      if (inToken == NEWLINE || inToken == NEWLINES) { inNextToken; true }
      else {
        val ret = inToken == SEMI
        accept(SEMI)
        ret
      }

    def errorTypeTree = TypeTree().setType(ErrorType).setPos((inCurrentPos))
    def errorTermTree = Literal(Constant(null)).setPos((inCurrentPos))
    def errorPatternTree = Ident(nme.WILDCARD).setPos((inCurrentPos))

/* -------------- TOKEN CLASSES ------------------------------------------- */

    def isModifier: Boolean = inToken match {
      case ABSTRACT | FINAL | SEALED | PRIVATE |
           PROTECTED | OVERRIDE | IMPLICIT | LAZY => true
      case _ => false
    }

    def isLocalModifier: Boolean = inToken match {
      case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY => true
      case _ => false
    }

    def isDefIntro: Boolean = inToken match {
      case VAL | VAR | DEF | TYPE | OBJECT |
           CASEOBJECT | CLASS | CASECLASS | TRAIT => true
      case _ => false
    }

    def isDclIntro: Boolean = inToken match {
      case VAL | VAR | DEF | TYPE => true
      case _ => false
    }

    def isIdent = inToken == IDENTIFIER || inToken == BACKQUOTED_IDENT

    def isExprIntroToken(token: Int): Boolean = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
           STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL | IDENTIFIER | BACKQUOTED_IDENT |
           THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
           DO | RETURN | THROW | LPAREN | LBRACE | XMLSTART => true
      case _ => false
    }

    def isExprIntro: Boolean = isExprIntroToken(inToken)

    def isTypeIntroToken(token: Int): Boolean = token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS |
           SUPER | USCORE | LPAREN | AT => true
      case _ => false
    }

    def isTypeIntro: Boolean = isTypeIntroToken(inToken)

    def isStatSep(token: Int): Boolean =
      token == NEWLINE || token == NEWLINES || token == SEMI

    def isStatSep: Boolean = isStatSep(inToken)


/* --------- COMMENT AND ATTRIBUTE COLLECTION ----------------------------- */

    /** Join the comment associated with a definition
    */
    def joinComment(trees: => List[Tree]): List[Tree] = {
      val buf = in.flushDoc
      if ((buf ne null) && buf.length > 0) trees map (t => DocDef(buf, t) setPos t.pos)
      else trees
    }

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

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
        def removeAsPlaceholder(name: Name) {
          placeholderParams = placeholderParams filter (_.name != name)
        }
        tree match {
          case Ident(name) =>
            removeAsPlaceholder(name)
            ValDef(Modifiers(Flags.PARAM), name, TypeTree(), EmptyTree)
          case Typed(tree @ Ident(name), tpe) if (tpe.isType) => // get the ident!
            removeAsPlaceholder(name)
            ValDef(Modifiers(Flags.PARAM), name, tpe, EmptyTree).setPos(tree.pos)
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
      val pname = freshName(tree.pos, "x$")
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

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */

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

    def checkAssoc(pos: Int, op: Name, leftAssoc: Boolean) =
      if (treeInfo.isLeftAssoc(op) != leftAssoc)
        syntaxError(
          pos, "left- and right-associative operators with same precedence may not be mixed", false)

    def reduceStack(isExpr: Boolean, base: List[OpInfo], top0: Tree,
                    prec: Int, leftAssoc: Boolean): Tree = {
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

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

    def ident(): Name =
      if (inToken == IDENTIFIER || inToken == BACKQUOTED_IDENT) {
        val name = inName.encode
        inNextToken
        name
      } else {
        accept(IDENTIFIER)
        nme.ERROR
      }

    def selector(t: Tree): Tree =
      atPos(inCurrentPos)(Select(t, ident()))

    /** Path       ::= StableId
     *              |  [Ident `.'] this
     *  AnnotType ::= Path [`.' type]
     */
    def path(thisOK: Boolean, typeOK: Boolean): Tree = {
      var t: Tree = null
      if (inToken == THIS) {
        t = atPos(inSkipToken) { This(nme.EMPTY.toTypeName) }
        if (!thisOK || inToken == DOT) {
          t =  selectors(t, typeOK, accept(DOT))
        }
      } else if (inToken == SUPER) {
	      // val pos = inCurrentPos
	val pos = inSkipToken
        val (mix,usePos) = mixinQualifierOpt(pos)
        t = atPos(usePos) {
          Super(nme.EMPTY.toTypeName, mix)
        }
        t = atPos(accept(DOT)) { selector(t) }
        if (inToken == DOT)
          t = selectors(t, typeOK, inSkipToken)
      } else {
        val i = atPos(inCurrentPos) {
          if (inToken == BACKQUOTED_IDENT) new BackQuotedIdent(ident())
          else Ident(ident())
        }
        t = i
        if (inToken == DOT) {
          val pos = inSkipToken
          if (inToken == THIS) {
            inNextToken
            t = atPos(i.pos) { This(i.name.toTypeName) }
            if (!thisOK || inToken == DOT)
              t = selectors(t, typeOK, accept(DOT))
          } else if (inToken == SUPER) {
            inNextToken
            val (mix,pos) = mixinQualifierOpt(i.pos)
            t = atPos(pos) { Super(i.name.toTypeName, mix) }
            t = atPos(accept(DOT)) {selector(t)}
            if (inToken == DOT)
              t = selectors(t, typeOK, inSkipToken)
          } else {
            t = selectors(t, typeOK, pos)
          }
        }
      }
      t
    }

    def selectors(t: Tree, typeOK: Boolean, pos : Int): Tree =
      if (typeOK && inToken == TYPE) {
        inNextToken
        atPos(pos) { SingletonTypeTree(t) }
      } else {
        val t1 = atPos(pos) { selector(t); }
        if (inToken == DOT) { selectors(t1, typeOK, inSkipToken) }
        else t1
      }

    /** MixinQualifier ::= `[' Id `]'
    */
    def mixinQualifierOpt(pos: Position): (Name, Position) =
      if (inToken == LBRACKET) {
        inNextToken
        val pos = inCurrentPos
        val name = ident().toTypeName
        accept(RBRACKET)
        (name, pos)
      } else {
        (nme.EMPTY.toTypeName, pos)
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
      val id = atPos(inCurrentPos) { Ident(ident()) }
      if (inToken == DOT) { selectors(id, false, inSkipToken) }
      else id
    }

    /** SimpleExpr    ::= literal
    *                  | symbol
    *                  | null
    */

    def literal(isPattern: Boolean, isNegated: Boolean): Tree = {
      def litToTree() = atPos(inCurrentPos) {
        Literal(
          inToken match {
            case CHARLIT   => Constant(charVal)
            case INTLIT    => Constant(intVal(isNegated))
            case LONGLIT   => Constant(longVal(isNegated))
            case FLOATLIT  => Constant(floatVal(isNegated))
            case DOUBLELIT => Constant(doubleVal(isNegated))
            case STRINGLIT | SYMBOLLIT => Constant(stringVal)
            case TRUE      => Constant(true)
            case FALSE     => Constant(false)
            case NULL      => Constant(null)
            case _ =>
              syntaxErrorOrIncomplete("illegal literal", true)
              null
          })
      }

      val isSymLit = inToken == SYMBOLLIT
      val t = litToTree()
      val pos = inSkipToken
      if (isSymLit) {
        atPos(pos) {
          var symid = scalaDot(nme.Symbol)
          Apply(symid, List(t))
        }
      } else {
        t
      }
    }

    def newLineOpt() {
      if (inToken == NEWLINE) inNextToken
    }

    def newLinesOpt() {
      if (inToken == NEWLINE || inToken == NEWLINES)
        inNextToken
    }

    def newLineOptWhenFollowedBy(token: Int) {
      // note: next is defined here because current == NEWLINE
      if (inToken == NEWLINE && inNextTokenCode == token) newLineOpt()
    }

    def newLineOptWhenFollowing(p: Int => Boolean) {
      // note: next is defined here because current == NEWLINE
      if (inToken == NEWLINE && p(inNextTokenCode)) newLineOpt()
    }

/* ------------- TYPES ---------------------------------------------------- */

    /** TypedOpt ::= [`:' Type]
    */
    def typedOpt(): Tree =
      if (inToken == COLON) { inNextToken; typ() }
      else TypeTree()

    /** RequiresTypedOpt ::= [requires AnnotType]
    */
    def requiresTypeOpt(): Tree =
      if (inToken == REQUIRES) {
        deprecationWarning(inCurrentPos, "`requires T' has been deprecated; use `{ self: T => ...'  instead")
        inNextToken; annotType(false)
      } else TypeTree()

    /** Types ::= Type {`,' Type}
     *  (also eats trailing comma if it finds one)
     */
    def types(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean): List[Tree] = {
      val ts = new ListBuffer[Tree] + argType(isPattern, isTypeApply, isFuncArg)
      while (inToken == COMMA) {
        val pos = inCurrentPos
        inNextToken
        if (inToken == RPAREN) {
          deprecationWarning(pos, "Trailing commas have been deprecated")
          return ts.toList
        } else {
          ts += argType(isPattern, isTypeApply, isFuncArg)
        }
      }
      ts.toList
    }

    /** modes for infix types */
    object InfixMode extends Enumeration {
      val FirstOp, LeftOp, RightOp = Value
    }

    /** Type ::= InfixType `=>' Type
     *         | `(' [`=>' Type] `)' `=>' Type
     *         | InfixType [ExistentialClause]
     *  ExistentialClause ::= forSome `{' ExistentialDcl {semi ExistentialDcl}} `}'
     *  ExistentialDcl    ::= type TypeDcl | val ValDcl
     */
    def typ(): Tree = typ(false)

    def typ(isPattern: Boolean): Tree = placeholderTypeBoundary {
      val t =
        if (inToken == LPAREN) {
          val pos = inSkipToken
          if (inToken == RPAREN) {
            inNextToken
            atPos(accept(ARROW)) { makeFunctionTypeTree(List(), typ(isPattern)) }
          } else {
            val ts = types(isPattern, false, true)
            accept(RPAREN)
            if (inToken == ARROW) atPos(inSkipToken) {
              makeFunctionTypeTree(ts, typ(isPattern))
            }
            else {
              for (t <- ts) t match {
                case AppliedTypeTree(Select(_, n), _)
                if (n == nme.BYNAME_PARAM_CLASS_NAME.toTypeName) =>
                  syntaxError(t.pos, "no by-name parameter type allowed here", false)
                case _ =>
              }
              infixTypeRest(pos, annotTypeRest(pos, isPattern, makeTupleType(ts, true)), false, InfixMode.FirstOp)
            }
          }
        } else {
          infixType(isPattern, InfixMode.FirstOp)
        }
      if (inToken == ARROW)
        atPos(inSkipToken) {
          makeFunctionTypeTree(List(t), typ(isPattern))
        }
      else if (inToken == FORSOME)
        atPos(inSkipToken) {
          val whereClauses = refinement()
          for (wc <- whereClauses) {
            wc match {
              case TypeDef(_, _, _, TypeBoundsTree(_, _)) |
                   ValDef(_, _, _, EmptyTree) | EmptyTree =>
                ;
              case _ =>
                syntaxError(wc.pos, "not a legal existential clause", false)
            }
          }
          ExistentialTypeTree(t, whereClauses)
        }
      else t
    }

    /** InfixType ::= CompoundType {id [nl] CompoundType}
     */
    def infixType(isPattern: Boolean, mode: InfixMode.Value): Tree = placeholderTypeBoundary {
      infixTypeRest(inCurrentPos, infixTypeFirst(isPattern), isPattern, mode)
    }

    def infixTypeFirst(isPattern: Boolean): Tree =
      if (inToken == LBRACE) scalaAnyRefConstr else annotType(isPattern)

    def infixTypeRest(pos: Int, t0: Tree, isPattern: Boolean, mode: InfixMode.Value): Tree = {
      val t = compoundTypeRest(pos, t0, isPattern)
      if (isIdent && inName != nme.STAR) {
        val opPos = inCurrentPos
        val leftAssoc = treeInfo.isLeftAssoc(inName)
        if (mode == InfixMode.LeftOp) checkAssoc(opPos, inName, true)
        else if (mode == InfixMode.RightOp) checkAssoc(opPos, inName, false)
        val op = ident()
        newLineOptWhenFollowing(isTypeIntroToken)
        def mkOp(t1: Tree) = atPos(opPos) { AppliedTypeTree(Ident(op.toTypeName), List(t, t1)) }
        if (leftAssoc)
          infixTypeRest(inCurrentPos, mkOp(compoundType(isPattern)), isPattern, InfixMode.LeftOp)
        else
          mkOp(infixType(isPattern, InfixMode.RightOp))
      } else t
    }

    /** CompoundType ::= AnnotType {with AnnotType} [Refinement]
     *                |  Refinement
     */
    def compoundType(isPattern: Boolean): Tree =
      compoundTypeRest(inCurrentPos, infixTypeFirst(isPattern), isPattern)

    def compoundTypeRest(pos: Int, t: Tree, isPattern: Boolean): Tree = {
      var ts = new ListBuffer[Tree] + t
      while (inToken == WITH) {
        inNextToken; ts += annotType(isPattern)
      }
      newLineOptWhenFollowedBy(LBRACE)
      atPos(pos) {
        if (inToken == LBRACE) {
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
     *  SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' Id
     *                     |  StableId
     *                     |  Path `.' type
     *                     |  `(' Types [`,'] `)'
     *                     |  WildcardType
     */
    def annotType(isPattern: Boolean): Tree = placeholderTypeBoundary {
      val pos = inCurrentPos

      val t: Tree = annotTypeRest(pos, isPattern,
        if (inToken == LPAREN) {
          inNextToken
          val ts = types(isPattern, false, false)
          accept(RPAREN)
          atPos(pos) { makeTupleType(ts, true) }
        } else if (inToken == USCORE) {
          wildcardType(inSkipToken)
        } else {
          val r = path(false, true)
          r match {
            case SingletonTypeTree(_) => r
            case _ => convertToTypeId(r)
          }
        })
      (t /: annotations(false)) (makeAnnotated)
    }

    def annotTypeRest(pos: Int, isPattern: Boolean, t: Tree): Tree =
      if (inToken == HASH) {
        inSkipToken
        val posId = inCurrentPos
        val id = ident
        annotTypeRest(pos, isPattern, atPos(posId) { SelectFromTypeTree(t, id.toTypeName) })
      } else if (inToken == LBRACKET) {
        val usePos = if (t.pos != NoPosition) t.pos else i2p(pos)
        annotTypeRest(pos, isPattern, atPos(usePos) { AppliedTypeTree(t, typeArgs(isPattern, false)) })
      }
      else
        t

    /** WildcardType ::= `_' TypeBounds
     */
    def wildcardType(pos: Int) = {
      val pname = freshName(pos, "_$").toTypeName
      val param = atPos(pos) { makeSyntheticTypeParam(pname, typeBounds()) }
      placeholderTypes = param :: placeholderTypes
      Ident(pname) setPos pos
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
    def argType(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean): Tree =
      if (isPattern) {
        if (inToken == USCORE)
          if (inToken == SUBTYPE || inToken == SUPERTYPE) wildcardType(inSkipToken)
          else atPos(inSkipToken) { Bind(nme.WILDCARD.toTypeName, EmptyTree) }
        else if (inToken == IDENTIFIER && treeInfo.isVariableName(inName.toTypeName))
          atPos(inCurrentPos) {
            Bind(ident().toTypeName, EmptyTree)
          }
        else {
          typ(true)
        }
      } else if (isFuncArg) {
        // copy-paste (with change) from def paramType
        if (inToken == ARROW)
          atPos(inSkipToken) {
            AppliedTypeTree(
                rootScalaDot(nme.BYNAME_PARAM_CLASS_NAME.toTypeName), List(typ()))
          }
        else {
          val t = typ()
          if (isIdent && inName == STAR) {
            inNextToken
            atPos(t.pos) {
              AppliedTypeTree(
                rootScalaDot(nme.REPEATED_PARAM_CLASS_NAME.toTypeName), List(t))
            }
          } else t
        }
      } else if (isTypeApply) {
        typ()
      } else {
        typ()
      }

/* ----------- EXPRESSIONS ------------------------------------------------ */

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
      val ts = new ListBuffer[Tree] + expr()
      while (inToken == COMMA) {
        val pos = inCurrentPos
        inNextToken
        if (inToken == RPAREN) {
          deprecationWarning(pos, "Trailing commas have been deprecated")
          return ts.toList
        } else {
          ts += expr()
        }
      }
      ts.toList
    }


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
    /* hook for IDE, unlike expression can be stubbed
     * don't use for any tree that can be inspected in the parser!
     */
    def statement(location: Int): Tree = expr(location)
    def expr(location: Int): Tree = {
      def isWildcard(t: Tree): Boolean = t match {
        case Ident(name1) if !placeholderParams.isEmpty && name1 == placeholderParams.head.name => true
        case Typed(t1, _) => isWildcard(t1)
        case Annotated(t1, _) => isWildcard(t1)
        case _ => false
      }
      var savedPlaceholderParams = placeholderParams
      placeholderParams = List()
      var res = inToken match {
        case IF =>
          val pos = inSkipToken
          val cond = surround(LPAREN,RPAREN)(expr(),Literal(true))
          newLinesOpt()
          val thenp = expr()
          val elsep = if (inToken == ELSE) { inNextToken; expr() }
		      else Literal(())
          atPos(pos) { If(cond, thenp, elsep) }
        case TRY =>
          atPos(inSkipToken) {
            val body =
              if (inToken == LBRACE) surround(LBRACE, RBRACE)(block(), Literal(()))
              else if (inToken == LPAREN) surround(LPAREN, RPAREN)(expr(), Literal(()))
              else expr()
            val catches =
              if (inToken == CATCH) {
                inNextToken
                val cases = surround(LBRACE,RBRACE)(caseClauses(), Nil)
                cases
              } else Nil
            val finalizer =
              if (inToken == FINALLY) { inNextToken; expr() }
              else EmptyTree
            Try(body, catches, finalizer)
          }
        case WHILE =>
          val pos = inSkipToken
          val lname: Name = freshName(pos, "while$")
          val cond = surround(LPAREN,RPAREN)(expr(),Literal(true))
          newLinesOpt()
          val body = expr()
          atPos(pos) { makeWhile(lname, cond, body) }
        case DO =>
          val pos = inSkipToken
          val lname: Name = freshName(pos, "doWhile$")
          val body = expr()
          if (isStatSep) inNextToken
          accept(WHILE)
          val cond = surround(LPAREN,RPAREN)(expr(), Literal(true))
          atPos(pos) { makeDoWhile(lname, body, cond) }
        case FOR =>
          atPos(inSkipToken) {
            val startToken = inToken
            val (open,close) = if (startToken == LBRACE) (LBRACE,RBRACE) else (LPAREN,RPAREN)
            val enums = surround(open,close)(enumerators(), Nil)
            newLinesOpt()
            if (inToken == YIELD) {
              inNextToken; makeForYield(enums, expr())
            } else makeFor(enums, expr())
          }
        case RETURN =>
          atPos(inSkipToken) {
            Return(if (isExprIntro) expr() else Literal(()))
          }
        case THROW =>
          atPos(inSkipToken) {
            Throw(expr())
          }
        case DOT =>
          deprecationWarning(inCurrentPos, "`.f' has been deprecated; use `_.f'  instead")
          atPos(inSkipToken) {
            if (isIdent) {
              makeDotClosure(stripParens(simpleExpr()))
            } else {
              syntaxErrorOrIncomplete("identifier expected", true)
              errorTermTree
            }
          }
        case _ =>
          var t = postfixExpr()
          if (inToken == EQUALS) {
            t match {
              case Ident(_) | Select(_, _) | Apply(_, _) =>
                t = atPos(inSkipToken) { makeAssign(t, expr()) }
              case _ =>
            }
          } else if (inToken == COLON) {
            t = stripParens(t)
            val pos = inSkipToken
            if (inToken == USCORE) {
              //todo: need to handle case where USCORE is a wildcard in a type
              val pos1 = inSkipToken
              if (isIdent && inName == nme.STAR) {
                inNextToken
                t = atPos(pos) {
                  Typed(t, atPos(pos1) { Ident(nme.WILDCARD_STAR.toTypeName) })
                }
              } else {
                syntaxErrorOrIncomplete("`*' expected", true)
              }
            } else if (in.token == AT) {
              t = (t /: annotations(false)) (makeAnnotated)
            } else {
              t = atPos(pos) {
                val tpt =
                  if (location != Local) infixType(false, InfixMode.FirstOp)
                  else typ()
                if (isWildcard(t))
                  (placeholderParams: @unchecked) match {
                    case (vd @ ValDef(mods, name, _, _)) :: rest =>
                      placeholderParams = copy.ValDef(vd, mods, name, tpt.duplicate, EmptyTree) :: rest
                  }
                // this does not correspond to syntax, but is necessary to
                // accept closures. We might restrict closures to be between {...} only!
                Typed(t, tpt)
              }
            }
          } else if (inToken == MATCH) {
            t = atPos(inSkipToken) {
              val cases = surround(LBRACE,RBRACE)(caseClauses(), Nil)
              Match(stripParens(t), cases)
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
          if (inToken == ARROW && (location != InTemplate || lhsIsTypedParamList)) {
            t = atPos(inSkipToken) {
              Function(convertToParams(t), if (location != InBlock) expr() else block())
            }
          }
          stripParens(t)
      }
      if (!placeholderParams.isEmpty && !isWildcard(res)) {
        res = atPos(res.pos){ Function(placeholderParams.reverse, res) }
        placeholderParams = List()
      }
      placeholderParams = placeholderParams ::: savedPlaceholderParams
      res
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
          true, base, top, precedence(inName), treeInfo.isLeftAssoc(inName))
        val op = inName
        opstack = OpInfo(top, op, inCurrentPos) :: opstack
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
      if (isIdent && inName == MINUS) {
        val name = unaryOp()
        inToken match {
          case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT => literal(false, true)
          case _ => atPos(inCurrentPos) { Select(stripParens(simpleExpr()), name) }
        }
      } else if (isIdent && (inName == PLUS || inName == TILDE || inName == BANG)) {
        val pos = inCurrentPos
        val name = unaryOp()
        atPos(pos) { Select(stripParens(simpleExpr()), name) }
      } else if (isIdent && inName == AMP) {
        deprecationWarning(inCurrentPos, "`&f' has been deprecated; use `f _' instead")
        val pos = inCurrentPos
        val name = ident()
        atPos(pos) { Typed(stripParens(simpleExpr()), Function(List(), EmptyTree)) }
/* XX-LIFTING
      } else if (settings.Xexperimental.value && isIdent && inName == SLASH) {
        val pos = inSkipToken
        val name = freshName()
        liftedGenerators += ValFrom(pos, Bind(name, Ident(nme.WILDCARD)), simpleExpr())
        Ident(name) setPos pos
*/
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
      var t: Tree = null
      var canApply = true
      inToken match {
        case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT |
             SYMBOLLIT | TRUE | FALSE | NULL =>
          t = literal(false, false)
        case XMLSTART => t = xmlLiteral()
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          t = path(true, false)
        case USCORE =>
          val pos = inSkipToken
          val pname = freshName(pos, "x$")
          val param = atPos(pos){ makeSyntheticParam(pname) }
          placeholderParams = param :: placeholderParams
          t = atPos(pos) { Ident(pname) }
        case LPAREN =>
          val pos = inSkipToken
          val ts = if (inToken == RPAREN) List() else exprs()
          accept(RPAREN)
          t = Parens(ts) setPos (pos)
        case LBRACE =>
          t = blockExpr()
          canApply = false
        case NEW =>
          t = atPos(inSkipToken) {
            val (parents, argss, self, stats) = template(false)
            makeNew(parents, self, stats, argss)
          }
          canApply = false
        case _ =>
          syntaxErrorOrIncomplete("illegal start of simple expression", true)
          t = errorTermTree
      }
      simpleExprRest(t, canApply)
    }

    def simpleExprRest(t: Tree, canApply: Boolean): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      inToken match {
        case DOT =>
          simpleExprRest(atPos(inSkipToken) { selector(stripParens(t)) }, true)
        case LBRACKET =>
          val t1 = stripParens(t)
          t1 match {
            case Ident(_) | Select(_, _) =>
              val pos = if (t1.pos == NoPosition) i2p(inCurrentPos) else t1.pos
              simpleExprRest(atPos(pos) { TypeApply(t1, typeArgs(false, true)) }, true)
            case _ =>
              t1
          }
        case LPAREN | LBRACE if (canApply) =>
          // again, position should be on idetifier, not (
          var pos = if (t.pos == NoPosition) i2p(inCurrentPos) else t.pos
          simpleExprRest(atPos(pos) {
            // look for anonymous function application like (f _)(x) and
            // translate to (f _).apply(x), bug #460
            val sel = t match {
              case Parens(List(Typed(_, _: Function))) =>
                Select(stripParens(t), nme.apply)
              case _ =>
                stripParens(t)
            }
            Apply(sel, argumentExprs())
          }, true)
        case USCORE =>
          atPos(inSkipToken) { Typed(stripParens(t), Function(List(), EmptyTree)) }
        case _ =>
          t
      }
    }

    /** ArgumentExprs ::= `(' [Exprs [`,']] `)'
      *                 | [nl] BlockExpr
     */
    def argumentExprs(): List[Tree] = {
      if (inToken == LBRACE) {
        List(blockExpr())
      } else {
        val ts = surround(LPAREN,RPAREN)(if (inToken == RPAREN) List() else exprs(), List())
        ts
      }
    }

    /** BlockExpr ::= `{' (CaseClauses | Block) `}'
     */
    def blockExpr(): Tree = {
      assert(inToken == LBRACE)
      val res = atPos(accept(LBRACE)) { // no need to surround
        if (inToken == CASE) Match(EmptyTree, caseClauses())
        else block()
      }
      accept(RBRACE)
      res
    }

    /** Block ::= BlockStatSeq
     */
    def block(): Tree = {
      makeBlock(blockStatSeq(new ListBuffer[Tree]))
    }

   /** CaseClauses ::= CaseClause {CaseClause}
    */
    def caseClauses(): List[CaseDef] = {
      val ts = new ListBuffer[CaseDef]
      do { ts += caseClause()
      } while (inToken == CASE)
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
    // IDE HOOK (so we can memoize case blocks)
    def caseBlock(): Tree =
      atPos(accept(ARROW))(block())

    /** Guard ::= if PostfixExpr
     */
    def guard(): Tree =
      if (inToken == IF) { inNextToken; stripParens(postfixExpr()) }
      else EmptyTree

    /** Enumerators ::= Generator {semi Enumerator}
     *  Enumerator  ::=  Generator
     *                |  Guard
     *                |  val Pattern1 `=' Expr
     */
    def enumerators(): List[Enumerator] = {
      val newStyle = inToken != VAL // todo: deprecate old style
      //if (!newStyle)
      //  deprecationWarning(inCurrentPos, "for (val x <- ... ) has been deprecated; use for (x <- ... ) instead")
      val enums = new ListBuffer[Enumerator]
      generator(enums, false)
      while (isStatSep) {
        inNextToken
        if (newStyle) {
          if (inToken == IF) enums += Filter(guard())
          else generator(enums, true)
        } else {
          if (inToken == VAL) generator(enums, true)
          else enums += Filter(expr())
        }
      }
      enums.toList
    }

    /** Generator ::= Pattern1 (`<-' | '=') Expr [Guard]
     */
    def generator(enums: ListBuffer[Enumerator], eqOK: Boolean) {
      if (inToken == VAL) inNextToken
      val pos = inCurrentPos;
      val pat = pattern1(false)
      val tok = inToken
      if (tok == EQUALS && eqOK) inNextToken
      else accept(LARROW)
      enums += makeGenerator(pos, pat, tok == EQUALS, expr)
      if (inToken == IF) enums += Filter(guard())
    }
    //def p2i(pos : ScanPosition) : Int;

/* -------- PATTERNS ------------------------------------------- */

    /**   Patterns ::= Pattern { `,' Pattern }
     *    SeqPatterns ::= SeqPattern { `,' SeqPattern }
     *
     *  (also eats trailing comma if it finds one)
     */
    def patterns(seqOK: Boolean): List[Tree] = {
      val ts = new ListBuffer[Tree] + pattern(seqOK)
      while (inToken == COMMA) {
        val pos = inCurrentPos
        inNextToken
        if (inToken == RPAREN) {
          deprecationWarning(pos, "Trailing commas have been deprecated")
          return ts.toList
        } else {
          ts += pattern(seqOK)
        }
      }
      ts.toList
    }

    /**   Pattern  ::=  Pattern1 { `|' Pattern1 }
     *    SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
     */
    def pattern(seqOK: Boolean): Tree = {
      val pos = inCurrentPos
      val t = pattern1(seqOK)
      if (isIdent && inName == BAR) {
        val ts = new ListBuffer[Tree] + t
        while (isIdent && inName == BAR) {
          inNextToken; ts += pattern1(seqOK)
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
    def pattern1(seqOK: Boolean): Tree = {
      //if (false && /*disabled, no regexp matching*/ seqOK && !isExprIntro) {
        //atPos(inCurrentPos) { Sequence(List()) }
      //} else {
        val p = pattern2(seqOK)
        p match {
          case Ident(name) if (treeInfo.isVarPattern(p) && inToken == COLON) =>
            atPos(inSkipToken) { Typed(p, compoundType(true)) }
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
    def pattern2(seqOK: Boolean): Tree = {
      val p = pattern3(seqOK)
      if (inToken == AT) {
        p match {
          case Ident(name) =>
            if (name == nme.WILDCARD) {
              inNextToken; pattern3(seqOK)
            } else if (treeInfo.isVarPattern(p)) {
              inNextToken
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
    def pattern3(seqOK: Boolean): Tree = {
      val base = opstack
      var top = simplePattern(seqOK)
      if (seqOK && isIdent && inName == STAR)
          return atPos(inSkipToken)(Star(stripParens(top)))

      while (isIdent && inName != BAR) {
        top = reduceStack(
          false, base, top, precedence(inName), treeInfo.isLeftAssoc(inName))
        val op = inName
        opstack = OpInfo(top, op, inCurrentPos) :: opstack
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
    def simplePattern(seqOK: Boolean): Tree = inToken match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
        var t = stableId()
        inToken match {
          case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT =>
            t match {
              case Ident(name) if name == nme.MINUS =>
                return literal(true, true)
              case _ =>
            }
          case _ =>
        }
/* not yet
        if (inToken == LBRACKET)
          atPos(inCurrentPos) {
            val ts = typeArgs(true, false)
            accept(LPAREN)
            val ps = if (inToken == RPAREN) List() else patterns(true, false)
            accept(RPAREN)
            Apply(TypeApply(convertToTypeId(t), ts), ps)
          }
        else */
        if (inToken == LPAREN) {
          atPos(t.pos) { Apply(t, argumentPatterns()) }
        } else t
      case USCORE =>
        atPos(inSkipToken) { Ident(nme.WILDCARD) }
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
           STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL =>
        literal(true, false)
      case LPAREN =>
        val pos = inSkipToken
        val ps = if (inToken == RPAREN) List() else patterns(false)
        accept(RPAREN)
        Parens(ps) setPos (pos)
      case XMLSTART =>
        xmlLiteralPattern()
      case _ =>
        syntaxErrorOrIncomplete("illegal start of simple pattern", true)
        errorPatternTree
    }

    def argumentPatterns(): List[Tree] = {
      accept(LPAREN)
      val ps = if (inToken == RPAREN) List() else patterns(true)
      accept(RPAREN)
      ps
    }

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    private def normalize(mods: Modifiers): Modifiers =
      if ((mods hasFlag Flags.PRIVATE) && mods.privateWithin != nme.EMPTY.toTypeName)
        mods &~ Flags.PRIVATE
      else if ((mods hasFlag Flags.ABSTRACT) && (mods hasFlag Flags.OVERRIDE))
        mods &~ (Flags.ABSTRACT | Flags.OVERRIDE) | Flags.ABSOVERRIDE
      else
        mods

    private def addMod(mods: Modifiers, mod: Long): Modifiers = {
      if (mods hasFlag mod) syntaxError(inCurrentPos, "repeated modifier", false)
      inNextToken
      mods | mod
    }

    /** AccessQualifier ::= "[" (Id | this) "]"
     */
    def accessQualifierOpt(mods: Modifiers): Modifiers = {
      var result = mods
      if (inToken == LBRACKET) {
        inNextToken
        if (mods.privateWithin != nme.EMPTY.toTypeName)
          syntaxError("duplicate private/protected qualifier", false)
        result = if (inToken == THIS) { inNextToken; mods | Flags.LOCAL }
                 else Modifiers(mods.flags, ident().toTypeName)
        accept(RBRACKET)
      }
      result
    }

    /** AccessModifier ::= (private | protected) [AccessQualifier]
     */
    def accessModifierOpt(): Modifiers = normalize {
      inToken match {
        case PRIVATE => inNextToken; accessQualifierOpt(Modifiers(Flags.PRIVATE))
        case PROTECTED => inNextToken; accessQualifierOpt(Modifiers(Flags.PROTECTED))
        case _ => NoMods
      }
    }

    /** Modifiers ::= {Modifier}
     *  Modifier  ::= LocalModifier
     *             |  AccessModifier
     *             |  override
     */
    def modifiers(): Modifiers = normalize {
      def loop(mods: Modifiers): Modifiers = inToken match {
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
          inNextToken
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
      def loop(mods: Modifiers): Modifiers = inToken match {
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

    /** Annotations   ::= {Annotation [nl]}
     *  Annotation    ::= `@' AnnotationExpr
     */
    def annotations(skipNewLines: Boolean): List[Annotation] = {
      var annots = new ListBuffer[Annotation]
      while (inToken == AT) {
        inNextToken
        annots += annotationExpr()
        if (skipNewLines) newLineOpt()
      }
      annots.toList
    }

    /** AnnotationExpr ::= StableId [TypeArgs] [`(' [Exprs] `)'] [[nl] `{' {NameValuePair} `}']
     *  NameValuePair ::= val id `=' PrefixExpr
     */
    def annotationExpr(): Annotation = {
      def nameValuePair(): Tree = {
        var pos = inCurrentPos
        accept(VAL)
        val aname = ident()
        accept(EQUALS)
        val rhs = stripParens(prefixExpr())
        atPos(pos) { ValDef(NoMods, aname, TypeTree(), rhs) }
      }
      val pos = inCurrentPos
      var t: Tree = convertToTypeId(stableId())
      if (inToken == LBRACKET)
        t = atPos(inCurrentPos)(AppliedTypeTree(t, typeArgs(false, false)))
      val args = if (inToken == LPAREN) argumentExprs() else List()
      newLineOptWhenFollowedBy(LBRACE)
      val nameValuePairs: List[Tree] = if (inToken == LBRACE) {
        inNextToken
        val nvps = new ListBuffer[Tree] + nameValuePair()
        while (inToken == COMMA) {
          inNextToken
          nvps += nameValuePair()
        }
        accept(RBRACE)
        nvps.toList
      } else List()
      val constr = atPos(pos) { New(t, List(args)) }
      Annotation(constr, nameValuePairs) setPos pos
    }

/* -------- PARAMETERS ------------------------------------------- */

    /** ParamClauses      ::= {ParamClause} [[nl] `(' implicit Params `)']
     *  ParamClause       ::= [nl] `(' [Params] ')'
     *  Params            ::= Param {`,' Param}
     *  Param             ::= {Annotation} Id [`:' ParamType]
     *  ClassParamClauses ::= {ClassParamClause} [[nl] `(' implicit ClassParams `)']
     *  ClassParamClause  ::= [nl] `(' [ClassParams] ')'
     *  ClassParams       ::= ClassParam {`,' ClassParam}
     *  ClassParam        ::= {Annotation}  [{Modifier} (`val' | `var')] Id [`:' ParamType]
     */
    def paramClauses(owner: Name, implicitViews: List[Tree], ofCaseClass: Boolean): List[List[ValDef]] = {
      var implicitmod = 0
      var caseParam = ofCaseClass
      def param(): ValDef = {
        var pos = inCurrentPos

        {
          val annots = annotations(false)
          var mods = Modifiers(Flags.PARAM)
          if (owner.isTypeName) {
            mods = modifiers() | Flags.PARAMACCESSOR
            if (mods.hasFlag(Flags.LAZY)) syntaxError("lazy modifier not allowed here. Use call-by-name parameters instead", false)
            if (inToken == VAL) {
              inNextToken
            } else if (inToken == VAR) {
              mods = mods | Flags.MUTABLE
              inNextToken
            } else {
              if (mods.flags != Flags.PARAMACCESSOR) accept(VAL)
              if (!(caseParam)) mods = mods | Flags.PRIVATE | Flags.LOCAL
            }
            if (caseParam) mods = mods | Flags.CASEACCESSOR
          }
          val namePos = inCurrentPos
          val name = ident()
          if (name != nme.ERROR) pos = namePos
          var bynamemod = 0
          val tpt =
            if (settings.Xexperimental.value && !owner.isTypeName && inToken != COLON) {
              TypeTree()
            } else { // XX-METHOD-INFER
              accept(COLON)
              if (inToken == ARROW) {
                if (owner.isTypeName && !mods.hasFlag(Flags.LOCAL))
                  syntaxError(
                    inCurrentPos,
                    (if (mods.hasFlag(Flags.MUTABLE)) "`var'" else "`val'") +
                    " parameters may not be call-by-name", false)
                else bynamemod = Flags.BYNAMEPARAM
              }
              paramType()
            }
          atPos(pos){
            ValDef((mods | implicitmod | bynamemod) withAnnotations annots, name, tpt, EmptyTree)
          }
        }
      }
      def paramClause(): List[ValDef] = {
        val params = new ListBuffer[ValDef]
        if (inToken != RPAREN) {
          if (inToken == IMPLICIT) {
            if (!implicitViews.isEmpty)
              syntaxError("cannot have both view bounds `<%' and implicit parameters", false)
            inNextToken
            implicitmod = Flags.IMPLICIT
          }
          params += param()
          while (inToken == COMMA) {
            inNextToken; params += param()
          }
        }
        params.toList
      }
      val vds = new ListBuffer[List[ValDef]]
      val pos = inCurrentPos
      newLineOptWhenFollowedBy(LPAREN)
      if (ofCaseClass && inToken != LPAREN)
        deprecationWarning(inCurrentPos, "case classes without a parameter list have been deprecated;\n"+
                           "use either case objects or case classes with `()' as parameter list.")
      while (implicitmod == 0 && inToken == LPAREN) {
        inNextToken
        vds += paramClause()
        accept(RPAREN)
        caseParam = false
        newLineOptWhenFollowedBy(LPAREN)
      }
      val result = vds.toList
      if (owner == nme.CONSTRUCTOR &&
          (result.isEmpty ||
           (!result.head.isEmpty && result.head.head.mods.hasFlag(Flags.IMPLICIT))))
        if (inToken == LBRACKET)
          syntaxError(pos, "no type parameters allowed here", false)
        else if(inToken == EOF)
          incompleteInputError("auxiliary constructor needs non-implicit parameter list")
        else
          syntaxError(pos, "auxiliary constructor needs non-implicit parameter list", false)
      addImplicitViews(owner, result, implicitViews)
    }

    /** ParamType ::= Type | `=>' Type | Type `*'
     */
    def paramType(): Tree =
      if (inToken == ARROW)
        atPos(inSkipToken) {
          AppliedTypeTree(
              rootScalaDot(nme.BYNAME_PARAM_CLASS_NAME.toTypeName), List(typ()))
        }
      else {
        val t = typ()
        if (isIdent && inName == STAR) {
          inNextToken
          atPos(t.pos) {
            AppliedTypeTree(
              rootScalaDot(nme.REPEATED_PARAM_CLASS_NAME.toTypeName), List(t))
          }
        } else t
      }

    /** TypeParamClauseOpt    ::= [TypeParamClause]
     *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
     *  VariantTypeParam      ::= [`+' | `-'] TypeParam
     *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
     *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
     *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds [<% Type]
     */
    def typeParamClauseOpt(owner: Name, implicitViewBuf: ListBuffer[Tree]): List[TypeDef] = {
      def typeParam(): TypeDef = {
        var mods = Modifiers(Flags.PARAM)
        if (owner.isTypeName && isIdent) {
          if (inName == PLUS) {
            inNextToken
            mods = mods | Flags.COVARIANT
          } else if (inName == MINUS) {
            inNextToken
            mods = mods | Flags.CONTRAVARIANT
          }
        }
        val pos = inCurrentPos
        val pname =
          (if (inToken == USCORE) { // @M! also allow underscore
            inNextToken
            nme.WILDCARD
          } else ident()).toTypeName

        val tparams = typeParamClauseOpt(pname, null) // @M TODO null --> no higher-order view bounds for now
        val param = atPos(pos) { TypeDef(mods, pname, tparams, typeBounds()) }
        if (inToken == VIEWBOUND && (implicitViewBuf ne null))
          implicitViewBuf += atPos(inSkipToken) {
            makeFunctionTypeTree(List(Ident(pname)), typ())
          }
        param
      }
      val params = new ListBuffer[TypeDef]
      newLineOptWhenFollowedBy(LBRACKET)
      if (inToken == LBRACKET) {
        inNextToken
        params += typeParam()
        while (inToken == COMMA) {
          inNextToken
          params += typeParam()
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
      if (inToken == tok) { inNextToken; typ() }
      else rootScalaDot(default.toTypeName)

/* -------- DEFS ------------------------------------------- */


    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    def importClause(): List[Tree] = {
      accept(IMPORT)
      val ts = new ListBuffer[Tree] + importExpr()
      while (inToken == COMMA) {
        inNextToken; ts += importExpr()
      }
      ts.toList
    }

    /**  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
     * XXX: Hook for IDE
     */
    def importExpr(): Tree =
      atPos(inCurrentPos) {
        var t: Tree = null
        //var pos : ScanPosition = null.asInstanceOf[ScanPosition]
        var pos : Int = -1
        if (inToken == THIS) {
          t = atPos(inCurrentPos) { This(nme.EMPTY.toTypeName) }
          t = atPos(accept(DOT)) { selector(t) }
          pos = accept(DOT)
        } else {
          val i = atPos(inCurrentPos) { Ident(ident()) }
          pos = accept(DOT)
          if (inToken == THIS) {
            inNextToken
            t = atPos(i.pos) { This(i.name.toTypeName) }
            t = atPos(accept(DOT)) { selector(t) }
            pos = accept(DOT)
          } else {
            t = i
          }
        }
        def loop: Tree =
          if (inToken == USCORE) {
            inNextToken
            Import(t, List((nme.WILDCARD, null)))
          } else if (inToken == LBRACE) {
            Import(t, importSelectors())
          } else {
            val identPos = inCurrentPos
            val name = ident() // @S: use position of identifier, not dot!
            pos = if (name == nme.ERROR) pos else identPos
            if (inToken == DOT) {
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
      while (!isLast && inToken == COMMA) {
        inNextToken
        isLast = importSelector(names)
      }
      accept(RBRACE)
      names.toList
    }

    /** ImportSelector ::= Id [`=>' Id | `=>' `_']
     */
    def importSelector(names: ListBuffer[(Name, Name)]): Boolean =
      if (inToken == USCORE) {
        inNextToken; names += ((nme.WILDCARD, null)); true
      } else {
        val name = ident()
        names += ((
          name,
          if (inToken == ARROW) {
            inNextToken
            if (inToken == USCORE) { inNextToken; nme.WILDCARD } else ident()
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
      inToken match {
        case VAL =>
          patDefOrDcl(mods)
        case VAR =>
          patDefOrDcl(mods | Flags.MUTABLE)
        case DEF =>
          List(funDefOrDcl(mods))
        case TYPE =>
          inNextToken
          newLinesOpt()
          List(typeDefOrDcl(mods))
        case _ =>
          List(tmplDef(mods))
      }
    }
    /** IDE hook: for non-local defs or dcls with modifiers and annotations */
    def nonLocalDefOrDcl : List[Tree] = {
      val annots = annotations(true)
      defOrDcl(modifiers() withAnnotations annots)
    }
    /** not hooked by the IDE, will not undergo stubbing. Used for early initialization blocks. */
    def preNonLocalDefOrDcl : List[Tree] = {
      val annots = annotations(true)
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
        inNextToken
        val p = pattern2(false)
        lhsBuf += stripParens(p)
      } while (inToken == COMMA)
      val lhs = lhsBuf.toList
      val tp = typedOpt()
      val rhs =
        if (tp.isEmpty || inToken == EQUALS) {
          accept(EQUALS)
          if (!tp.isEmpty && newmods.hasFlag(Flags.MUTABLE) &&
              (lhs.toList forall (_.isInstanceOf[Ident])) && inToken == USCORE) {
            inNextToken
            newmods = newmods | Flags.DEFAULTINIT
            EmptyTree
          } else {
            expr()
          }
        } else {
          newmods = newmods | Flags.DEFERRED
          EmptyTree
        }
      var originalUsed = false
      def mkDefs(p: Tree): List[Tree] = {
        //Console.println("DEBUG: p = "+p.toString()); // DEBUG
        val trees =
          makePatDef(newmods,
                     if (tp.isEmpty)
                       p
                     else
                       Typed(p, tp),
                     if (inIDE && !originalUsed) {
                       // because duplicates have weaker status than originals
                       // need an original.
                       originalUsed = true
                       rhs
                     } else rhs.duplicate) map atPos(p.pos)
        if (newmods.hasFlag(Flags.DEFERRED)) {
          trees match {
            case List(ValDef(_, _, _, EmptyTree)) =>
              if (mods.hasFlag(Flags.LAZY))
                syntaxError(p.pos, "lazy values may not be abstract", false)
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
        inNextToken
        lhs += (inCurrentPos, ident())
      } while (inToken == COMMA)
      val tp = typedOpt()
      val rhs = if (tp.isEmpty || inToken == EQUALS) {
        accept(EQUALS)
        if (!tp.isEmpty && inToken == USCORE) {
          inNextToken
          EmptyTree
        } else {
          expr()
        }
      } else {
        newmods = newmods | Flags.DEFERRED
        EmptyTree
      }
      var originalUsed = false
      for ((pos, name) <- lhs.toList) yield atPos(pos) {
        if (inIDE && !originalUsed) {
          originalUsed = true
          ValDef(newmods, name, tp, rhs)
        } else ValDef(newmods, name, tp.duplicate, rhs.duplicate)
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
      var pos = inSkipToken // position of `def'
      if (inToken == THIS) {
        atPos(inCurrentPos) {
          inNextToken
          val vparamss = paramClauses(nme.CONSTRUCTOR, implicitClassViews map (_.duplicate), false)
          newLineOptWhenFollowedBy(LBRACE)
          val rhs = if (inToken == LBRACE) constrBlock(vparamss)
                    else { accept(EQUALS); constrExpr(vparamss) }
          DefDef(mods, nme.CONSTRUCTOR, List(), vparamss, TypeTree(), rhs)
        }
      } else {
        var newmods = mods
        val namePos = inCurrentPos
        val name = ident()
        if (name != nme.ERROR) pos = namePos
        atPos(pos) {
          // implicitViewBuf is for view bounded type parameters of the form
          // [T <% B]; it contains the equivalent implicit parameter, i.e. (implicit p: T => B)
          val implicitViewBuf = new ListBuffer[Tree]
          val tparams = typeParamClauseOpt(name, implicitViewBuf)
          val vparamss = paramClauses(name, implicitViewBuf.toList, false)
          newLineOptWhenFollowedBy(LBRACE)
          var restype = typedOpt()
          val rhs =
            if (isStatSep || inToken == RBRACE) {
              if (restype.isEmpty) restype = scalaUnitConstr
              newmods = newmods | Flags.DEFERRED
              EmptyTree
            } else if (restype.isEmpty && inToken == LBRACE) {
              restype = scalaUnitConstr
              blockExpr()
            } else equalsExpr()
          DefDef(newmods, name, tparams, vparamss, restype, rhs)
        }
      }
    }


    /** ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     */
    def constrExpr(vparamss: List[List[ValDef]]): Tree =
      if (inToken == LBRACE) constrBlock(vparamss)
      else Block(List(selfInvocation(vparamss)), Literal(()))

    /** SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
     */
    def selfInvocation(vparamss: List[List[ValDef]]): Tree =
      atPos(accept(THIS)) {
        newLineOptWhenFollowedBy(LBRACE)
        var t = Apply(Ident(nme.CONSTRUCTOR), argumentExprs())
        while (inToken == LPAREN || inToken == LBRACE) {
          t = Apply(t, argumentExprs())
          newLineOptWhenFollowedBy(LBRACE)
        }
        if (implicitClassViews.isEmpty) t
        else Apply(t, vparamss.last.map(vp => Ident(vp.name)))
      }

    /** ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     */
    def constrBlock(vparamss: List[List[ValDef]]): Tree =
      atPos(inSkipToken) {
        val statlist = new ListBuffer[Tree]
        statlist += selfInvocation(vparamss)
        val stats = if (isStatSep) { inNextToken; blockStatSeq(statlist) }
                    else statlist.toList
        accept(RBRACE)
        Block(stats, Literal(()))
      }

    /** TypeDef ::= Id [TypeParamClause] `=' Type
     *  TypeDcl ::= Id [TypeParamClause] TypeBounds
     */
    def typeDefOrDcl(mods: Modifiers): Tree =
      atPos(inCurrentPos) {
        val name = ident().toTypeName

        // @M! a type alias as well as an abstract type may declare type parameters
        val tparams = inToken match {
          case LBRACKET =>
            typeParamClauseOpt(name, null)
          case _ =>
            Nil
        }

        inToken match {
          case EQUALS =>
            inNextToken
            TypeDef(mods, name, tparams, typ())
          case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE =>
            TypeDef(mods | Flags.DEFERRED, name, tparams, typeBounds())
          case _ =>
            syntaxErrorOrIncomplete("`=', `>:', or `<:' expected", true)
            EmptyTree
        }
      }

    /** Hook for IDE, for top-level classes/objects */
    def topLevelTmplDef: Tree = {
      val annots = annotations(true)
      val mods = modifiers() withAnnotations annots
      tmplDef(mods)
    }

    /**  TmplDef ::= [case] class ClassDef
     *            |  [case] object ObjectDef
     *            |  [override] trait TraitDef
     */
    def tmplDef(mods: Modifiers): Tree = {
      if (mods.hasFlag(Flags.LAZY)) syntaxError("classes cannot be lazy", false)
      inToken match {
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
      var pos = inSkipToken
      var namePos = inCurrentPos
      val name = ident().toTypeName
      if (name != nme.ERROR) pos = namePos
      atPos(pos) {
        val savedViews = implicitClassViews
        val implicitViewBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, implicitViewBuf)
        implicitClassViews = implicitViewBuf.toList
        if (!implicitClassViews.isEmpty && mods.hasFlag(Flags.TRAIT)) {
          syntaxError("traits cannot have type parameters with <% bounds", false)
          implicitClassViews = List()
        }
        val constrAnnots = annotations(false)
        val (constrMods, vparamss) =
          if (mods.hasFlag(Flags.TRAIT)) (Modifiers(Flags.TRAIT), List())
          else (accessModifierOpt(), paramClauses(name, implicitClassViews, mods.hasFlag(Flags.CASE)))
        val thistpe = requiresTypeOpt()
        var mods1 =
          if (mods hasFlag Flags.TRAIT)
            if (inToken == SUBTYPE) mods | Flags.DEFERRED
            else mods
          else if (inToken == SUBTYPE) {
            syntaxError("classes are not allowed to be virtual", false)
            mods
          }
          else
            mods
        var template = templateOpt(mods1, name, constrMods withAnnotations constrAnnots, vparamss)
        if (!thistpe.isEmpty) {
          if (template.self.isEmpty) {
            template = copy.Template(
              template, template.parents, makeSelfDef(nme.WILDCARD, thistpe), template.body)
          } else syntaxError("`requires' cannot be combined with explicit self type", false)
        }
        if (isInterface(mods1, template.body)) mods1 |= Flags.INTERFACE
        val result = ClassDef(mods1, name, tparams, template)
        implicitClassViews = savedViews
        result
      }
    }

    /** ObjectDef       ::= Id ClassTemplateOpt
     */
    def objectDef(mods: Modifiers): ModuleDef = {
      var pos = inSkipToken
      var namePos = inCurrentPos
      val name = ident().toTermName
      if (name != nme.ERROR) pos = namePos
      atPos(pos) {
        val mods1 = if (inToken == SUBTYPE) mods | Flags.DEFERRED else mods
        val template = templateOpt(mods1, name, NoMods, List())
        ModuleDef(mods1, name, template)
      }
    }


    /** ClassParents       ::= AnnotType {`(' [Exprs [`,']] `)'} {with AnnotType}
     *  TraitParents       ::= AnnotType {with AnnotType}
     */
    def templateParents(isTrait: Boolean): (List[Tree], List[List[Tree]]) = {
      val parents = new ListBuffer[Tree] + annotType(false)
      val argss = new ListBuffer[List[Tree]]
      if (inToken == LPAREN && !isTrait)
        do { argss += argumentExprs() } while (inToken == LPAREN)
      else argss += List()
      while (inToken == WITH) {
        inNextToken
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
      if (inToken == LBRACE) {
        // @S: pre template body cannot stub like post body can!
        val (self, body) = templateBody(true)
        if (inToken == WITH && self.isEmpty) {
          val earlyDefs: List[Tree] = body flatMap {
            case vdef @ ValDef(mods, name, tpt, rhs) if !(mods hasFlag Flags.DEFERRED) =>
              List(copy.ValDef(vdef, mods | Flags.PRESUPER, name, tpt, rhs))
            case tdef @ TypeDef(mods, name, tparams, rhs) =>
              List(copy.TypeDef(tdef, mods | Flags.PRESUPER, name, tparams, rhs))
            case stat if !stat.isEmpty =>
                syntaxError(stat.pos, "only type definitions and concrete field definitions allowed in early object initialization section", false)
              List()
            case _ => List()
          }
          inNextToken
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
     */
    def templateOpt(mods: Modifiers, name: Name, constrMods: Modifiers,
                    vparamss: List[List[ValDef]]): Template = {
      val pos = inCurrentPos;
      val (parents0, argss, self, body) =
        if (inToken == EXTENDS || settings.Xexperimental.value && (mods hasFlag Flags.TRAIT) && inToken == SUBTYPE) {
          inNextToken
          template(mods hasFlag Flags.TRAIT)
        } else if ((inToken == SUBTYPE) && (mods hasFlag Flags.TRAIT)) {
	  inNextToken
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
      val tree = Template(parents, self, constrMods, vparamss, argss, body)
      // @S: if nothing parsed, don't use next position!
      // @S: if primary constructor does not always have the same position, then the IDE gets confused.
      // @S: since build compiler is used to generate IDE files, don't set position here!
      tree
      // if (pos == inCurrentPos || inIDE) tree else atPos(pos) {tree}
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
      if (inToken == LBRACE) {
        templateBody(false)
      } else {
        if (inToken == LPAREN)
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

    /** Packaging ::= package QualId [nl] `{' TopStatSeq `}'
     */
    def packaging(pkgPos: Int): Tree = {
      val pkg = qualId()
      val pos = if (pkg.pos != NoPosition) pkg.pos else i2p(pkgPos)
      atPos(pos) {
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
     *            | package object objectDef
     *            | Import
     *            |
     */
    def topStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (inToken != RBRACE && inToken != EOF) {
        if (inToken == PACKAGE) {
	  val pkgPos = accept(PACKAGE)
	  stats += {
	    if (inToken == OBJECT)
	      atPos(pkgPos) { makePackageObject(objectDef(NoMods)) }
	    else packaging(pkgPos)
	  }
        } else if (inToken == IMPORT) {
          stats ++= importClause()
          // XXX: IDE hook this all.
        } else if (inToken == CLASS ||
                   inToken == CASECLASS ||
                   inToken == TRAIT ||
                   inToken == OBJECT ||
                   inToken == CASEOBJECT ||
                   inToken == LBRACKET || //todo: remove
                   inToken == AT ||
                   isModifier) {
          stats ++ joinComment(List(topLevelTmplDef))
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("expected class or object definition", true)
        }
        if (inToken != RBRACE && inToken != EOF) acceptStatSep()
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
        if (inToken == ARROW) {
          first match {
            case Typed(tree @ This(name), tpt) if (name == nme.EMPTY.toTypeName) =>
              self = makeSelfDef(nme.WILDCARD, tpt).setPos(tree.pos)
            case _ =>
              convertToParam(first) match {
                case tree @ ValDef(_, name, tpt, EmptyTree) if (name != nme.ERROR) =>
                  self = makeSelfDef(name, tpt).setPos(tree.pos)
                case _ =>
              }
          }
          inNextToken
        } else {
          stats += first
          if (in.token != RBRACE && in.token != EOF/* !isStatSep(in.token)*/) acceptStatSep()
        }
      }
      while (inToken != RBRACE && inToken != EOF) {
        if (inToken == IMPORT) {
          stats ++= importClause()
        } else if (isExprIntro) {
          stats += statement(InTemplate)
        } else if (isDefIntro || isModifier || inToken == LBRACKET /*todo: remove */ || inToken == AT) {
          if (isPre) // @S: avoid caching by calling a different method that does the same thing (except in the IDE)
            stats ++= joinComment(preNonLocalDefOrDcl)
          else stats ++= joinComment(nonLocalDefOrDcl)
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("illegal start of definition", true)
        }
        if (inToken != RBRACE && inToken != EOF) acceptStatSep()
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
      while (inToken != RBRACE && inToken != EOF) {
        if (isDclIntro) { // don't IDE hook
          stats ++= joinComment(defOrDcl(NoMods))
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("illegal start of declaration", true)
        }
        if (inToken != RBRACE) acceptStatSep()
      }
      stats.toList
    }

    /** overridable IDE hook for local definitions of blockStatSeq
     *  Here's an idea how to fill in start and end positions.
    def localDef : List[Tree] = {
      atEndPos {
        atStartPos(inCurrentPos) {
          val annots = annotations(true)
          val mods = localModifiers() withAnnotations annots
          if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(mods)
          else List(tmplDef(mods))
        }
      } (inCurrentPos)
    }
    */

    def localDef : List[Tree] = {
      val annots = annotations(true)
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
      var keepGoing = true
      var hasError = false
      while ((inToken != RBRACE) && (inToken != EOF) && (inToken != CASE) && keepGoing) {
        var hasError0 = hasError
        hasError = false
        if (inToken == IMPORT) {
          stats ++= importClause()
          acceptStatSep()
        } else if (isExprIntro) {
          stats += statement(InBlock)
          if (inToken != RBRACE && inToken != CASE) acceptStatSep()
        } else if (isDefIntro || isLocalModifier || in.token == AT) {
          stats ++= localDef
          if (inToken == RBRACE || inToken == CASE) {
            syntaxError("block must end in result expression, not in definition", false)
            stats += Literal(()).setPos(inCurrentPos)
          } else acceptStatSep()
        } else if (isStatSep) {
          inNextToken
        } else {
          syntaxErrorOrIncomplete("illegal start of statement", true)
          if (hasError0) keepGoing = false else hasError = true
        }
      }
      stats.toList
    }

    /** CompilationUnit ::= [package QualId semi] TopStatSeq
     */
    def compilationUnit(): Tree = checkNoEscapingPlaceholders {
      var pos = inCurrentPos;
      {
        val ts = new ListBuffer[Tree]
        // @S: the IDE can insert phantom semi-colons before package during editing
        // @S: just eat them (doesn't really change the grammar)
        while (inToken == SEMI) inNextToken
        if (inToken == PACKAGE) {
          pos = inSkipToken
	  if (in.token == OBJECT) {
	    ts += makePackageObject(objectDef(NoMods))
	    if (inToken != EOF) {
	      acceptStatSep()
	      ts ++= topStatSeq()
	    }
	  } else {
            val pkg = qualId()
            newLineOptWhenFollowedBy(LBRACE)
            if (inToken == EOF) {
              ts += makePackaging(pkg, List())
            } else if (isStatSep) {
              inNextToken
              ts += makePackaging(pkg, topStatSeq())
            } else {
              accept(LBRACE)
              ts += makePackaging(pkg, topStatSeq())
              accept(RBRACE)
              ts ++= topStatSeq()
            }
	  }
        } else {
          ts ++= topStatSeq()
        }
        val stats = ts.toList
        val usePos =
          if (stats.isEmpty || stats.head.pos == NoPosition) i2p(pos)
          else stats.head.pos
        atPos(usePos) { stats match {
          case List(stat @ PackageDef(_, _)) => stat
          case _ => makePackaging(Ident(nme.EMPTY_PACKAGE_NAME), stats)
        }}
      }
    }
  }
}
