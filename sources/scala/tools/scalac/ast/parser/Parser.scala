/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

import scalac.ast.parser.PatternNormalizer;
import scalac.symtab.Modifiers;
import scalac.ast._;
import scalac.atree.AConstant;
import scalac._;
import scalac.util._;
import ch.epfl.lamp.util.Position;
import java.util.{Map, Stack, ArrayList, LinkedList};
import java.lang.{Integer, Long, Float, Double};
import scala.Iterator;
import scala.tools.scalac.util.NewArray;
import scala.collection.immutable.ListMap ;
import scala.collection.mutable.Buffer;

package scala.tools.scalac.ast.parser {

/** A recursive descent parser for the programming language Scala.
 *
 *  @author     Martin Odersky, Matthias Zenger, Burak Emir
 *  @version    1.3
 */
class Parser(unit: Unit) {

  import Tokens._;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  /** the lexical analyzer
  */
  val s = new Scanner(unit);

  /** the tree factory
  */
  val make: TreeFactory = unit.global.make;

  /** the tree generator
  */
  val gen: TreeGen = unit.global.treeGen;

  /** pattern checker and normalizer
  */
  val pN = new PatternNormalizer(unit);

  /** The current nesting depths of while and do loops.
  */
  var loopNestingDepth = 0;

  /** this is the general parse method
  */
  def parse(): Array[Tree] = {
    val ts = if (s.unit.console) templateStatSeq() else compilationUnit();
    accept(EOF);
    ts
  }

/////// ERROR HANDLING //////////////////////////////////////////////////////

  private def skip(): unit = {
    //System.out.println("<skipping> " + s.token2string(s.token));//DEBUG
    var nparens = 0;
    var nbraces = 0;
    while (true) {
      s.token match {
        case EOF =>
          return;
        case SEMI =>
          if (nparens == 0 && nbraces == 0)
            return;
        case RPAREN =>
          nparens = nparens - 1;
        case RBRACE =>
          if (nbraces == 0) return;
          nbraces = nbraces - 1;
        case LPAREN =>
          nparens = nparens + 1;
        case LBRACE =>
          nbraces = nbraces + 1;
        case _ =>
      }
      s.nextToken();
    }
  }

  def syntaxError(msg: String, skipIt: boolean): Tree =
    syntaxError(s.pos, msg, skipIt);

  def syntaxError(pos: int, msg: String, skipIt: boolean): Tree = {
    if (pos != s.errpos) {
      s.unit.error(pos, msg);
      s.errpos = pos;
    }
    if (skipIt) skip();
    make.Bad(pos)
  }

  def accept(token: int): int = {
    val pos = s.pos;
    if (s.token != token) {
      val errpos = if ((s.pos >>> Position.COLUMN_BITS) >
                    (s.lastpos >>> Position.COLUMN_BITS)) s.lastpos
                   else s.pos;
      syntaxError(errpos, Tokens.token2string(token) + " expected but " +
                  Tokens.token2string(s.token) + " found.", true);
    }
    if (s.token == token) s.nextToken();
    pos
  }

/////// TOKEN CLASSES //////////////////////////////////////////////////////

  def isModifier(): boolean =
    (s.token == ABSTRACT)
    || (s.token == FINAL)
    || (s.token == SEALED)
    || (s.token == PRIVATE)
    || (s.token == PROTECTED)
    || (s.token == OVERRIDE);

  def isLocalModifier(): boolean =
    (s.token == ABSTRACT)
    || (s.token == FINAL)
    || (s.token == SEALED);

  def isDefIntro(): boolean = s.token match {
    case VAL | VAR | DEF | TYPE | OBJECT | CASEOBJECT | CLASS | CASECLASS | TRAIT =>
      true
    case _ =>
      false
  }

  def isDclIntro(): boolean = s.token match {
    case VAL | VAR | DEF | TYPE =>
      true;
    case _ =>
      false
  }

  def isExprIntro(): boolean = s.token match {
    case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
         STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL | IDENTIFIER |
         THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
         DO | RETURN | THROW | LPAREN | LBRACE =>
      true;
    case _ =>
      false;
  }

/////// COMMENT COLLECTION ///////////////////////////////////////////////////

  /** keep the comments associated with a given tree
  */
  protected val mapTreeComment: Map = unit.global.mapTreeComment;

  /** stack of comments
  */
  protected val commentStack = new Stack();

  /** positive if we are inside a block
  */
  protected var local = 0;

  /** push last encountered comment and reset the buffer
  */
  protected def pushComment(): unit = {
    if (local == 0) {
      commentStack.push(if (s.docBuffer == null) null else s.docBuffer.toString());
      s.docBuffer = null;
    }
  }

  /** pop a comment from the stack and associate it with the given tree
  */
  protected def popComment(tree: Tree): Tree = {
    if (local == 0)
      if (!commentStack.empty())
        mapTreeComment.put(tree, commentStack.pop().asInstanceOf[String]);
    tree
  }

/////// TREE CONSTRUCTION ////////////////////////////////////////////////////

  /** Name supply
  */
  var freshCnt = 0;

  def fresh(): Name = {
    val n = Name.fromString("x$" + freshCnt);
    freshCnt = freshCnt + 1;
    n
  }

  /** Create a tree representing a packaging
  */
  def makePackaging(pos: int, pkg0: Tree, stats0: Array[Tree]): Tree = {
    var pkg = pkg0;
    var stats = stats0;
    while (true) {
      val templ = make.Template(pos, Tree.EMPTY_ARRAY, stats);
      pkg match {
        case Tree$Select(qual, name) =>
          stats = NewArray.Tree(
                    make.PackageDef(pos, make.Ident(pkg.pos, name), templ));
          pkg = qual;
        case _ =>
          return make.PackageDef(pos, pkg, templ);
      }
    }
    null//dummy
  }

  /** Create tree representing binary operation expression or pattern.
  */
  def makeBinop(isExpr: boolean, pos: int, left: Tree, op: Name, right: Tree): Tree =
    if (isExpr) {
      if (op.isLeftAssoc()) {
        make.Apply(
          pos,
          make.Select(pos, left, NameTransformer.encode(op)),
          NewArray.Tree(right));
      } else {
        val x: Name = fresh();
        make.Block(
          pos,
          NewArray.Tree(
            make.ValDef(pos, 0, x, Tree.Empty, left)),
            make.Apply(
              pos,
              make.Select(pos, right, NameTransformer.encode(op)),
              NewArray.Tree(make.Ident(left.pos, x))));
      }
    } else {
      make.Apply(
        pos,
        make.Ident(pos, NameTransformer.encode(op).toTypeName()),
        NewArray.Tree(left, right));
    }

  def scalaDot(pos: int, name: Name): Tree =
    make.Select(pos, make.Ident(pos, Names.scala), name);

  def scalaRuntimeDot(pos: int, name: Name): Tree =
    make.Select(pos, scalaDot(pos, Names.runtime), name);

  def ScalaRunTimeDot(pos: int, name: Name): Tree =
    make.Select(pos, scalaRuntimeDot(pos, Names.ScalaRunTime), name);

  def scalaBooleanDot(pos: int, name: Name): Tree =
    make.Select(pos, scalaDot(pos, Names.Boolean), name);

  def scalaAnyRefConstr(pos: int): Tree =
    make.Apply(
      pos, scalaDot(pos, Names.AnyRef.toTypeName()), Tree.EMPTY_ARRAY);

  def scalaObjectConstr(pos: int): Tree =
    make.Apply(
      pos, scalaDot(pos, Names.ScalaObject.toTypeName()), Tree.EMPTY_ARRAY);

  /** Create tree for for-comprehension <for (enums) do body> or
  *   <for (enums) yield body> where mapName and flatmapName are chosen
  *  corresponding to whether this is a for-do or a for-yield.
  */
  def makeFor(pos: int, enums: Array[Tree], mapName: Name, flatmapName: Name, body: Tree): Tree = {

    def makeFor1(pos: int, name: Name, pat: Tree, rhs: Tree, body: Tree): Tree =
      make.Apply(
        pos, make.Select(pos, rhs, name),
        NewArray.Tree(makeForCont(pos, pat, body)));

    def makeForCont(pos: int, pat: Tree, body: Tree): Tree = {
      pat match {
        case Tree$Ident(name1) if (name1.isVariable()) =>
          make.Function(
              pos,
              NewArray.ValDef(
                make.ValDef(
                  pat.pos, Modifiers.PARAM, name1, Tree.Empty, Tree.Empty)),
              body);
        case _ =>
          make.Visitor(pos, NewArray.CaseDef(
            make.CaseDef(pos, pat, Tree.Empty, body)));
      }
    }

    enums(0) match {
      case Tree$PatDef(mods, pat, rhs) =>
        if (enums.length == 1) {
          makeFor1(pos, mapName, pat, rhs, body);
        } else {
          val newenums = new Array[Tree](enums.length - 1);
          enums(1) match {
            case Tree$PatDef(mods2, pat2, rhs2) =>
              System.arraycopy(enums, 1, newenums, 0, newenums.length);
              makeFor1(
                pos, flatmapName, pat, rhs,
                makeFor(enums(1).pos, newenums, mapName, flatmapName, body));
            case _ =>
              System.arraycopy(enums, 2, newenums, 1, newenums.length - 1);
              newenums(0) = make.PatDef(
                enums(0).pos, mods, pat,
                makeFor1(enums(1).pos, Names.filter, pat.duplicate(), rhs, enums(1)));
              makeFor(pos, newenums, mapName, flatmapName, body);
          }
        }
    }
  }

  def makeTry(pos: int, body: Tree, catcher: Tree, finalizer: Tree): Tree = {
    var t = body;
    if (catcher != Tree.Empty)
      t = make.Apply(
        pos,
        make.Select(
          pos,
          make.Apply(
            pos, ScalaRunTimeDot(pos, Names.Try), NewArray.Tree(t)),
          Names.Catch),
        NewArray.Tree(catcher));
    if (finalizer != Tree.Empty)
      t = make.Apply(
        pos,
        make.Select(
          pos,
          make.Apply(
            pos, ScalaRunTimeDot(pos, Names.Try), NewArray.Tree(t)),
          Names.Finally),
        NewArray.Tree(finalizer));
    t
  }

  def makeWhile(pos: int, lname: Name, cond: Tree, body: Tree): Tree = {
    val continu = make.Apply(
      pos, make.Ident(pos, lname), Tree.EMPTY_ARRAY);
    val rhs = make.If(
      pos,
      cond,
      make.Block(body.pos, NewArray.Tree(body), continu),
      gen.mkUnitLit(pos));
    make.LabelDef(pos, lname, new Array[Tree$Ident](0), rhs);
  }

  def makeDoWhile(pos: int, lname: Name, body: Tree, cond: Tree): Tree = {
    val continu = make.Apply(
      pos, make.Ident(pos, lname), Tree.EMPTY_ARRAY);
    val rhs = make.Block(
      body.pos,
      NewArray.Tree(
        body),
        make.If(
          cond.pos,
          cond,
          continu,
          gen.mkUnitLit(pos)));
    make.LabelDef(pos, lname, new Array[Tree$Ident](0), rhs)
  }

  def mkXML(pos:int, isPattern:boolean, t:Tree, args:Array[Tree]):Tree = {
    var symt = scalaDot(s.pos, Names.Symbol);
    if( isPattern ) symt = convertToTypeId(symt);
    val ts = new myTreeList();
    ts.append(t);
    ts.append(args);
    make.Apply(pos, symt, ts.toArray());
  }

  def makeXMLpat(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, true, gen.mkStringLit( pos, n.toString() ), args);

  def makeXML(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, false, gen.mkStringLit( pos, n.toString() ), args);

  def makeXMLseq( pos:int, args:Array[Tree] ) = {
    var symt = scalaDot(s.pos, Names.List);
    make.Apply(pos, symt, args);
  }

  def makeXML(pos:int, n:Name, args:Array[Tree], attrMap:ListMap[Name,String]):Tree = {
    val t = makeXML( pos, n, args );
    val attrs = new Array[Tree]( attrMap.size );
    var i = 0;
    for( val Pair( key, value ) <- attrMap.toList ) {
      attrs.update( i, make.Apply(pos,
                                  scalaDot(s.pos, Names.Tuple2),
                                  { val x = new Array[Tree](2);
                                   x(0) = gen.mkStringLit( pos, key.toString() );
                                   x(1) = gen.mkStringLit( pos, value.toString() );
                                   x }
                                ));
      i = i + 1;
    };
    make.Apply(pos,
               make.Select( pos, t, Names.PERCENT ),
               { val x = new Array[Tree](1);
                x( 0 ) = make.Apply(pos,
                                    scalaDot(s.pos, Names.List),
                                    attrs);
		x })

  }

  /** Convert tree to formal parameter list
  */
  def convertToParams(t: Tree): Array[Tree$ValDef] = t match {
    case Tree$Function(params, Tree.Empty) =>
      params
    case Tree$Ident(_) | Tree$Typed(Tree$Ident(_), _) =>
      NewArray.ValDef(convertToParam(t));
    case Tree$Literal(AConstant.UNIT) => // !!!
      Tree.ValDef_EMPTY_ARRAY;
    case _ =>
      syntaxError(t.pos, "malformed formal parameter list", false);
      Tree.ValDef_EMPTY_ARRAY;
  }

  /** Convert list of trees to formal parameter list
  */
  def convertToParams(ts: Array[Tree]): Array[Tree$ValDef] = {
    val res = new Array[Tree$ValDef](ts.length);
    for (val i <- Iterator.range(0, res.length))
      res(i) = convertToParam(ts(i));
    res;
  }

  /** Convert tree to formal parameter
  */
  def convertToParam(tree: Tree): Tree$ValDef = tree match {
    case Tree$Ident(name) =>
      make.ValDef(
        tree.pos, Modifiers.PARAM, name, Tree.Empty, Tree.Empty)
    case Tree$Typed(Tree$Ident(name), tpe) =>
      make.ValDef(
        tree.pos, Modifiers.PARAM, name, tpe, Tree.Empty)
    case _ =>
      val tpe = syntaxError(tree.pos, "not a legal formal parameter", false);
      make.ValDef(
        tree.pos, Modifiers.PARAM, Names.ERROR, tpe, Tree.Empty)
  }

  /** Convert (qual)ident to type identifier
  */
  def convertToTypeId(t: Tree): Tree = t match {
    case Tree$Ident(name) =>
      make.Ident(t.pos, name.toTypeName())
    case Tree$Select(qual, name) =>
      make.Select(t.pos, qual, name.toTypeName())
    case _ =>
      t
  }

  /** Convert (qual)ident to constructor identifier
  */
  def convertToConstr(t: Tree): Tree = t match {
    case Tree$Ident(name) =>
      make.Ident(t.pos, name.toTypeName())
    case Tree$Select(qual, name) =>
      make.Select(t.pos, qual, name.toTypeName())
    case _ =>
      syntaxError(t.pos, "class constructor expected", false)
  }

  /** Complete unapplied constructor with `()' arguments
  */
  def applyConstr(t: Tree): Tree = t match {
    case Tree$Apply(_, _) =>
      t
    case _ =>
      make.Apply(t.pos, t, Tree.EMPTY_ARRAY)
  }

/////// OPERAND/OPERATOR STACK /////////////////////////////////////////////////

  var operands = new Array[Tree](8);
  var positions = new Array[int](8);
  var operators = new Array[Name](8);
  var sp = 0;

  def push(od: Tree, pos: int, op: Name): unit = {
    if (sp == operands.length) {
      val operands1 = new Array[Tree](sp * 2);
      System.arraycopy(operands, 0, operands1, 0, sp);
      operands = operands1;
      val positions1 = new Array[int](sp * 2);
      System.arraycopy(positions, 0, positions1, 0, sp);
      positions = positions1;
      val operators1 = new Array[Name](sp * 2);
      System.arraycopy(operators, 0, operators1, 0, sp);
      operators = operators1;
    }
    operands(sp) = od;
    positions(sp) = pos;
    operators(sp) = op;
    sp = sp + 1;
  }

  def reduceStack(isExpr: boolean, base: int, _top: Tree, prec: int, leftAssoc: boolean): Tree = {
    var top = _top;
    if (sp != base &&
        operators(sp-1).precedence() == prec &&
        operators(sp-1).isLeftAssoc() != leftAssoc) {
      syntaxError(
        positions(sp-1),
        "left- and right-associative operators with same precedence may not be mixed",
        false);
    }
    while (sp != base &&
           (prec < operators(sp-1).precedence() ||
            (leftAssoc && prec == operators(sp-1).precedence()))) {
      sp = sp - 1;
      top = makeBinop(isExpr, positions(sp), operands(sp), operators(sp), top);
    }
    top
  }

/////// IDENTIFIERS AND LITERALS ////////////////////////////////////////////////////////////

  final val MINUS = Name.fromString("-");
  final val PLUS = Name.fromString("+");
  final val BANG = Name.fromString("!");
  final val TILDE = Name.fromString("~");
  final val STAR = Name.fromString("*");
  final val BAR  = Name.fromString("|");
  final val OPT  = Name.fromString("?");
  final val LT   = Name.fromString("<");

  def ident(): Name =
    if (s.token == IDENTIFIER) {
      val name = NameTransformer.encode(s.name);
      s.nextToken();
      name
    } else {
      accept(IDENTIFIER);
      Names.ERROR
    }

  /** StableRef  ::= StableId
  *              |  [Ident `.'] this
  *  SimpleType ::=  StableRef [`.' type]
  */
  def stableRef(thisOK: boolean, typeOK: boolean): Tree = {
    var t: Tree = null;
    if (s.token == THIS) {
      t = make.This(s.skipToken(), TypeNames.EMPTY);
      if (!thisOK || s.token == DOT)
        t = selectors(accept(DOT), t, typeOK);
    } else if (s.token == SUPER) {
      t = make.Super(
        s.skipToken(), TypeNames.EMPTY, mixinQualifierOpt());
      t = make.Select(accept(DOT), t, ident());
      if (s.token == DOT)
        t = selectors(s.skipToken(), t, typeOK);
    } else {
      val i: Tree$Ident = make.Ident(s.pos, ident());
      t = i;
      if (s.token == DOT) {
        val pos = s.skipToken();
        if (s.token == THIS) {
          s.nextToken();
          t = make.This(i.pos, i.name.toTypeName());
          if (!thisOK || s.token == DOT)
            t = selectors(accept(DOT), t, typeOK);
        } else if (s.token == SUPER) {
          s.nextToken();
          t = make.Super(
            i.pos, i.name.toTypeName(), mixinQualifierOpt());
          t = make.Select(accept(DOT), t, ident());
          if (s.token == DOT)
            t = selectors(s.skipToken(), t, typeOK);
        } else {
          t = selectors(pos, t, typeOK);
        }
      }
    }
    t
  }

  def selectors(pos: int, t: Tree, typeOK: boolean): Tree =
    if (typeOK && s.token == TYPE) {
      s.nextToken();
      make.SingletonType(pos, t);
    } else {
      val t1 = make.Select(pos, t, ident());
      if (s.token == DOT) selectors(s.skipToken(), t1, typeOK);
      else t1
    }

  /** MixinQualifier ::= `[' Id `]'
  */
  def mixinQualifierOpt(): Name =
    if (s.token == LBRACKET) {
      s.nextToken();
      val name = ident().toTypeName();
      accept(RBRACKET);
      name
    } else {
      TypeNames.EMPTY
    }

  /** StableId ::= Id
  *            |  StableRef `.' Id
  *            |  [Id '.'] super [MixinQualifier] ` `.' Id
  */
  def stableId(): Tree =
    stableRef(false, false);

  /** QualId ::= Id {`.' Id}
  */
  def qualId(): Tree = {
    val id = make.Ident(s.pos, ident());
    if (s.token == DOT) selectors(s.skipToken(), id, false)
    else id
  }

  /** SimpleExpr    ::= literal
  *                  | symbol [ArgumentExprs]
  *                  | null
  */
  def literal(isPattern: boolean): Tree = {
    def litToTree() = s.token match {
      case CHARLIT =>
        gen.mkCharLit(s.pos, s.intVal.asInstanceOf[char])
      case INTLIT =>
        gen.mkIntLit(s.pos, s.intVal.asInstanceOf[int])
      case LONGLIT =>
        gen.mkLongLit(s.pos, s.intVal)
      case FLOATLIT =>
        gen.mkFloatLit(s.pos, s.floatVal.asInstanceOf[float])
      case DOUBLELIT =>
        gen.mkDoubleLit(s.pos, s.floatVal)
      case STRINGLIT | SYMBOLLIT =>
        gen.mkStringLit(s.pos, s.name.toString())
      case TRUE =>
        gen.mkBooleanLit(s.pos, true)
      case FALSE =>
        gen.mkBooleanLit(s.pos, false)
      case NULL =>
        gen.mkNullLit(s.pos)
      case _ =>
        syntaxError("illegal literal", true)
    }

    val isSymLit = s.token == SYMBOLLIT;
    val t = litToTree();
    s.nextToken();
    if (isSymLit) {
      val pos = s.pos;
      if (s.token == LPAREN || s.token == LBRACE)
        mkXML( pos, isPattern, t, argumentExprs() );
      else
        mkXML( pos, isPattern, t, Tree.EMPTY_ARRAY );
    } else {
      t
    }
  }

//////// TYPES ///////////////////////////////////////////////////////////////

  /** TypedOpt ::= [`:' Type]
  */
  def typedOpt(): Tree =
    if (s.token == COLON) {
      s.nextToken();
      typ()
    } else {
      Tree.Empty
    }

  /** SimpleTypedOpt ::= [`:' SimpleType]
  */
  def simpleTypedOpt(): Tree =
    if (s.token == COLON) {
      s.nextToken();
      simpleType()
    } else {
      Tree.Empty
    }

  /** Types ::= Type {`,' Type}
  */
  def types(): Array[Tree] = {
    val ts = new myTreeList();
    ts.append(typ());
    while (s.token == COMMA) {
      s.nextToken();
      ts.append(typ());
    }
    ts.toArray()
  }

  /** Type ::= Type1 `=>' Type
  *         | `(' [Types] `)' `=>' Type
  *         | Type1
  */
  def typ(): Tree = {
    var t: Tree = _;
    if (s.token == LPAREN) {
      s.nextToken();
      if (s.token == RPAREN) {
        s.nextToken();
        val pos = accept(ARROW);
        return make.FunType(pos, Tree.EMPTY_ARRAY, typ());
      } else {
        t = typ();
        if (s.token == COMMA) {
          s.nextToken();
          val ts = new myTreeList();
          ts.append(t);
          ts.append(types());
          accept(RPAREN);
          val pos = accept(ARROW);
          return make.FunType(pos, ts.toArray(), typ());
        } else {
          accept(RPAREN);
        }
      }
    } else {
      t = type1()
    }
    if (s.token == ARROW)
      make.FunType(s.skipToken(), NewArray.Tree(t), typ())
    else
      t
  }

  /** Type1 ::= SimpleType {with SimpleType} [Refinement]
  */
  def type1(): Tree = {
    val pos = s.pos;
    val t = simpleType();
    if (s.token == WITH || s.token == LBRACE) {
      val ts = new myTreeList();
      ts.append(t);
      while (s.token == WITH) {
        s.nextToken();
        ts.append(simpleType());
      }
      val rs = if (s.token == LBRACE) refinement() else Tree.EMPTY_ARRAY;
      make.CompoundType(pos, ts.toArray(), rs)
    } else {
      t
    }
  }

  /** SimpleType ::= SimpleType TypeArgs
  *               | SimpleType `#' Id
  *               | StableId
  *               | StableRef `.' type
  *               | `(' Type `)'
  */
  def simpleType(): Tree = {
    val pos = s.pos;
    var t: Tree =
      if (s.token == LPAREN) {
        s.nextToken();
        val t = typ();
        accept(RPAREN);
        t
      } else {
        convertToTypeId(stableRef(false, true));
      }
    while (true) {
      if (s.token == HASH)
        t = make.SelectFromType(s.skipToken(), t, ident().toTypeName());
      else if (s.token == LBRACKET)
        t = make.AppliedType(pos, t, typeArgs());
      else
        return t;
    }
    null; //dummy
  }

  /** TypeArgs ::= `[' Types `]'
  */
  def typeArgs(): Array[Tree] = {
    accept(LBRACKET);
    val ts = types();
    accept(RBRACKET);
    ts
  }

//////// EXPRESSIONS ////////////////////////////////////////////////////////

  /** EqualsExpr ::= `=' Expr
  */
  def equalsExpr(): Tree = {
    accept(EQUALS);
    expr()
  }

  /** Exprs ::= Expr {`,' Expr}
  *          | Expr `:' `_' `*'
  def exprs(): Array[Tree] = {    val ts = new myTreeList();
    ts.append(expr(true, false));
    while (s.token == COMMA) {
      s.nextToken();
      ts.append(expr());
    }
    ts.toArray()
  }
  */

  /** Exprs ::= Expr {`,' Expr} [ `:' `_' `*' ]
  */
  def exprs(): Array[Tree] = {
    val ts = new myTreeList();
    ts.append(expr(true, false));
    while (s.token == COMMA) {
      s.nextToken();
      ts.append(expr(true, false));
    }
    ts.toArray()
  }

  /** Expr       ::= Bindings `=>' Expr
   *               | Expr1
   *  ResultExpr ::= Bindings `=>' Block
   *               | Expr1
   *  Expr1      ::= (' Expr `)' Expr [[`;'] else Expr]
   *               | try `{' block `}' [catch Expr] [finally Expr]
   *               | while `(' Expr `)' Expr
   *               | do Expr [`;'] while `(' Expr `)'
   *               | for `(' Enumerators `)' (do | yield) Expr
   *               | throw Expr
   *               | return [Expr]
   *               | [SimpleExpr `.'] Id `=' Expr
   *               | SimpleExpr ArgumentExprs `=' Expr
   *               | PostfixExpr [`:' Type1]
   *  Bindings   ::= Id [`:' Type1]
   *               | `(' [Binding {`,' Binding}] `)'
   *  Binding    ::= Id [`:' Type]
   */

  def expr(): Tree =
    expr(false, false);

  def expr(isArgument: boolean, isInBlock: boolean): Tree = {
    if (s.token == IF) {
      val pos = s.skipToken();
      accept(LPAREN);
      val cond = expr();
      accept(RPAREN);
      val thenp = expr();
      val elsep =
        if (s.token == ELSE) { s.nextToken(); expr() }
        else Tree.Empty;
      make.If(pos, cond, thenp, elsep)
    } else if (s.token == TRY) {
      val pos = s.skipToken();
      accept(LBRACE);
      val body = block(pos);
      accept(RBRACE);
      val catcher =
        if (s.token == CATCH) { s.nextToken(); expr() }
        else Tree.Empty;
      val finalizer =
        if (s.token == FINALLY) { s.nextToken(); expr() }
        else Tree.Empty;
      makeTry(pos, body, catcher, finalizer)
    } else if (s.token == WHILE) {
      val lname = Name.fromString("label$" + loopNestingDepth);
      loopNestingDepth = loopNestingDepth + 1;
      val pos = s.skipToken();
      accept(LPAREN);
      val cond = expr();
      accept(RPAREN);
      val body = expr();
      loopNestingDepth = loopNestingDepth - 1;
      makeWhile(pos, lname, cond, body)
    } else if (s.token == DO) {
      val lname = Name.fromString("label$" + loopNestingDepth);
      loopNestingDepth = loopNestingDepth + 1;
      val pos = s.skipToken();
      val body = expr();
      if (s.token == SEMI) s.nextToken();
      accept(WHILE);
      accept(LPAREN);
      val cond = expr();
      accept(RPAREN);
      loopNestingDepth = loopNestingDepth - 1;
      makeDoWhile(pos, lname, body, cond)
    } else if (s.token == FOR) {
      s.nextToken();
      accept(LPAREN);
      val enums = enumerators();
      accept(RPAREN);
      if (s.token == YIELD)
        makeFor(s.skipToken(), enums, Names.map, Names.flatmap, expr())
      else
        makeFor(s.pos, enums, Names.foreach, Names.foreach, expr())
    } else if (s.token == RETURN) {
      val pos = s.skipToken();
      val e =
        if (isExprIntro()) expr()
        else gen.mkUnitLit(pos);
      make.Return(pos, e)
    } else if (s.token == THROW) {
      val pos = s.skipToken();
      make.Throw(pos, expr())
    } else {
      var t = postfixExpr();
      if (s.token == EQUALS) {
        t match {
          case Tree$Ident(_) | Tree$Select(_, _) | Tree$Apply(_, _) =>
            t = make.Assign(s.skipToken(), t, expr());
          case _ =>
        }
      } else if (s.token == COLON) {
        val pos = s.skipToken();
        if (isArgument && s.token == USCORE) {
          val pos1 = s.skipToken();
          if (s.token == IDENTIFIER && s.name == Names.STAR) {
            s.nextToken();
            /*
            if( s.token != RPAREN )
              syntaxError(s.pos, " escaping sequences only allowed for last argument", true);
            */
            t = make.Typed(
              pos, t, make.Ident(pos1, TypeNames.WILDCARD_STAR));
          } else {
            syntaxError(s.pos, "`*' expected", true);
          }
        } else {
          val tp = type1();
          t = make.Typed(pos, t, tp);
        }
      }
      if (s.token == ARROW) {
        val arrowpos = s.skipToken();
        t = make.Function(arrowpos, convertToParams(t),
                          if (isInBlock) block(arrowpos) else expr());
      }
      t
    }
  }

  /** PostfixExpr   ::= InfixExpr [Id]
   *  InfixExpr     ::= PrefixExpr
   *                  | InfixExpr Id InfixExpr
   */
  def postfixExpr(): Tree = {
    val base = sp;
    var top = prefixExpr();
    while (s.token == IDENTIFIER) {
      top = reduceStack(
        true, base, top, s.name.precedence(), s.name.isLeftAssoc());
      push(top, s.pos, s.name);
      ident();
      if (isExprIntro()) {
        top = prefixExpr();
      } else {
        sp = sp - 1;
        val pos = positions(sp);
        val postOp = operators(sp);
        top = reduceStack(true, base, operands(sp), 0, true);
        return make.Select(pos, top, NameTransformer.encode(postOp));
      }
    }
    reduceStack(true, base, top, 0, true)
  }

  /** PrefixExpr   ::= [`-' | `+' | `~' | `!'] SimpleExpr
  */
  def prefixExpr(): Tree =
    if (s.token == IDENTIFIER &&
        (s.name == MINUS ||
         s.name == PLUS ||
         s.name == TILDE ||
         s.name == BANG)) {
      val name = ident();
      make.Select(s.pos, simpleExpr(), name);
    } else {
      simpleExpr()
    }

  /* SimpleExpr    ::= literal
  *                 | xmlExpr
  *                 | StableRef
  *                 | `(' [Expr] `)'
  *                 | BlockExpr
  *                 | new Template
  *                 | SimpleExpr `.' Id
  *                 | SimpleExpr TypeArgs
  *                 | SimpleExpr ArgumentExprs
  */
  def simpleExpr(): Tree = {
    var t: Tree = null;
    s.token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT |
           SYMBOLLIT | TRUE | FALSE | NULL =>
        t = literal(false);
      case IDENTIFIER | THIS | SUPER =>
        t = if(( s.name == LT )&&( unit.global.xmlMarkup )) {
          xmlExprTop();  /* top-level xml expression */
        } else {
          stableRef(true, false);
        }
      case LPAREN =>
        val pos = s.skipToken();
        if (s.token == RPAREN) {
          s.nextToken();
          t = gen.mkUnitLit(pos);
        } else {
          t = expr();
          if (s.token == COMMA) {
            val commapos = s.skipToken();
            val ts = new myTreeList();
            ts.append(t);
            ts.append(exprs());
            accept(RPAREN);
            if (s.token == ARROW) {
              t = make.Function(
                pos, convertToParams(ts.toArray()), Tree.Empty);
            } else {
              t = syntaxError(commapos, "`)' expected", false);
            }
          } else {
            accept(RPAREN);
          }
        }
      case LBRACE =>
        t = blockExpr();
      case NEW =>
        t = make.New(s.skipToken(), template());
      case _ =>
        return syntaxError("illegal start of expression", true);
    }
    while (true) {
      s.token match {
        case DOT =>
          t = make.Select(s.skipToken(), t, ident());
        case LBRACKET =>
          t match {
            case Tree$Ident(_) | Tree$Select(_, _) =>
              t = make.TypeApply(s.pos, t, typeArgs());
            case _ =>
              return t;
          }
        case LPAREN | LBRACE =>
          t = make.Apply(s.pos, t, argumentExprs());
        case _ =>
          return t;
      }
    }
    null;//dummy
  }

  /** top level xml expression, resynchronizes after succesful parse
   *  see xmlExpr
  */
  def xmlExprTop():Tree = {
    val pos = s.pos;
    var t = xmlExpr();
    //Console.println("old:"+token2string( s.token ) );
    s.token = EMPTY;
    s.nextToken();
    //Console.println("new:"+token2string( s.token ) );
    //Console.println("line:"+s.cline);
    if(( s.token == IDENTIFIER ) && ( s.name == LT ))  {
      val ts = new myTreeList();
      ts.append( t );
      while(( s.token == IDENTIFIER ) && ( s.name == LT )) {
	ts.append( xmlExpr() );
	s.nextToken();
      }
      t = makeXMLseq( pos, ts.toArray() );
    }
    t
  }

  /** ArgumentExprs ::= `(' [Exprs] `)'
   *                  | BlockExpr
   */
  def argumentExprs(): Array[Tree] = {
    var ts = Tree.EMPTY_ARRAY;
    if (s.token == LBRACE) {
      ts = NewArray.Tree(blockExpr());
    } else {
      accept(LPAREN);
      if (s.token != RPAREN)
        ts = exprs();
      accept(RPAREN);
    }
    ts
  }

  /** BlockExpr ::= `{' CaseClause {CaseClause} `}'
   *              | `{' Block `}'
   */
  def blockExpr(): Tree = {
    local = local + 1;
    val pos = accept(LBRACE);
    val res =
      if (s.token == CASE) {
        val stats = new myTreeList();
        do {
          stats.append(caseClause());
        } while (s.token == CASE);
        make.Visitor(pos,
          stats.copyTo(new Array[Tree$CaseDef](stats.length()))
          .asInstanceOf[Array[Tree$CaseDef]])
      } else {
        block(pos);
      }
    accept(RBRACE);
    local = local - 1;
    res
  }

  /** Block ::= BlockStatSeq
  */
  def block(pos: int): Tree = {
    block(pos, blockStatSeq(new myTreeList()));
  }
  private def block(pos: int, stats: Array[Tree]): Tree = {
    if (stats.length == 0)
      gen.mkUnitLit(pos);
    else if (!stats(stats.length - 1).isTerm())
      make.Block(pos, stats, gen.mkUnitLit(pos));
    else if (stats.length == 1)
      return stats(0);
    else {
      val trees: Array[Tree] = new Array[Tree](stats.length - 1);
      System.arraycopy(stats, 0, trees, 0, trees.length);
      return make.Block(pos, trees, stats(stats.length - 1));
    }
  }

  /** caseClause : =>= case Pattern [if PostfixExpr] `=>' Block
   */
  def caseClause(): Tree = {
    val pos = accept(CASE);
    val pat = validPattern();
    val guard =
      if (s.token == IF) { s.nextToken(); postfixExpr() }
      else Tree.Empty;
    make.CaseDef(pos, pat, guard, block(accept(ARROW)))
  }

  /** Enumerators ::= Generator {`;' Enumerator}
  *  Enumerator  ::= Generator
  *                | Expr
  */
  def enumerators(): Array[Tree] = {
    val enums = new myTreeList();
    enums.append(generator());
    while (s.token == SEMI) {
      s.nextToken();
      if (s.token == VAL) enums.append(generator());
      else enums.append(expr());
    }
    enums.toArray()
  }

  /** Generator ::= val Pattern1 `<-' Expr
  */
  def generator(): Tree = {
    val pos = accept(VAL);
    val pat = validPattern1();
    accept(LARROW);
    var rhs = expr();
    if (!TreeInfo.isVarPattern(pat))
      rhs = make.Apply(
        rhs.pos,
        make.Select(rhs.pos, rhs, Names.filter),
        NewArray.Tree(
          make.Visitor(
            rhs.pos,
            NewArray.CaseDef(
              make.CaseDef(
                rhs.pos, pat.duplicate(),
                Tree.Empty, gen.mkBooleanLit(s.pos, true)),
              make.CaseDef(
                rhs.pos, make.Ident(rhs.pos, Names.PATTERN_WILDCARD),
                Tree.Empty, gen.mkBooleanLit(s.pos, false))))));
    make.PatDef(pos, 0, pat, rhs)
  }

//////// PATTERNS ////////////////////////////////////////////////////////////

  /**  Pattern ( see pattern() ) which is checked for validity
  */
  def validPattern(): Tree = {
    val pos = s.pos;
    val pat = pattern();
    if (this.pN.check(pat)) { // reports syntax errors as side effect
      // normalize
      pN.wrapAlternative(pN.elimSequence(pN.flattenSequence(pat)))
    } else {
      make.Bad(pos)
    }
  }

  /**  Pattern1 ( see pattern1() ) which is checked for validity
   */
  def validPattern1(): Tree = {
    val pos = s.pos;
    val pat = pattern1();
    if(this.pN.check(pat)) { // reports syntax errors as side effect
      pN.wrapAlternative(pN.elimSequence(pN.flattenSequence(pat)))
    } else {
      make.Bad(pos)
    }
  }

  /** Patterns ::= Pattern {`,' Pattern}
  */
  def patterns(): Array[Tree] = {
    val ts = new myTreeList();
    ts.append(pattern());
    while (s.token == COMMA) {
      s.nextToken();
      ts.append(pattern());
    }
    ts.toArray()
  }

  /**   Pattern  ::=  Pattern1 { `|' Pattern1 }
  */
  def pattern(): Tree = {
    val pos = s.pos;
    val first = pattern1();
    if (s.token == IDENTIFIER && s.name == BAR) {
      val choices = new myTreeList();
      choices.append( first );
      while (s.token == IDENTIFIER && s.name == BAR) {
        s.nextToken();
        choices.append(pattern1());
      }
      val tarr = choices.toArray();
      val ts = pN.flattenAlternativeChildren(tarr);
      return pN.flattenAlternative(make.Alternative(pos, ts.toArray()));
    }
    first
  }

  /**   Pattern1  ::=  varid `:' Type1
  *                |  `_' `:' Type1
  *                |  Pattern2
  */
  def pattern1(): Tree = {
    val p = pattern2();
    if (s.token == COLON && TreeInfo.isVarPattern(p))
      make.Typed(s.skipToken(), p, type1())
    else p
  }

  /*   Pattern2  ::=  varid [ @ Pattern3 ]
  *                |  Pattern3
  */
  def pattern2(): Tree = {
    val p = pattern3();
    if (s.token == AT && TreeInfo.isVarPattern(p)) {
      p match {
        case Tree$Ident(name) =>
          if (name == Names.PATTERN_WILDCARD) return pattern3()
        case _ =>
      }
      make.Bind(s.skipToken(), p.asInstanceOf[Tree$Ident].name, pattern3());
    } else {
      p
    }
  }

  /*   Pattern3  ::=  SimplePattern [ '*' | '?' | '+' ]
  *               |  SimplePattern {Id SimplePattern}    // op2 must not be empty
  */
  def pattern3(): Tree = {
    val base = sp;
    var top = simplePattern();
    if (s.token == IDENTIFIER) {
      if (s.name == STAR) {    /*         p*  becomes  z@( |(p,z))       */
        s.nextToken();
        val zname = fresh();
        val zvar = make.Ident(s.pos, zname);
        return make.Bind(
          s.pos, zname,
          pN.flattenAlternative(
            make.Alternative(s.pos, NewArray.Tree(
              make.Sequence(s.pos, Tree.EMPTY_ARRAY),
              pN.flattenSequence(make.Sequence(s.pos, NewArray.Tree(
                top, zvar)))))));
      } else if (s.name == PLUS) {    /*    p+   becomes   z@(p,(z| ))    */
        s.nextToken();
        val zname = fresh();
        val zvar = make.Ident(s.pos, zname);
        return make.Bind(
          s.pos, zname,
          pN.flattenSequence(make.Sequence(s.pos, NewArray.Tree(
            top,
            pN.flattenAlternative(make.Alternative(s.pos, NewArray.Tree(
              zvar, make.Sequence(s.pos, Tree.EMPTY_ARRAY))))))));
      } else if (s.name == OPT) { /*    p?   becomes   (p| )            */
        s.nextToken();
        return pN.flattenAlternative(make.Alternative(s.pos, NewArray.Tree(
          top,
          make.Sequence(s.pos, Tree.EMPTY_ARRAY))));
      }
    }
    while ((s.token == IDENTIFIER) && (s.name != BAR)) {
      val tokn = s.name; // for error message
      top = reduceStack(
        false, base, top, s.name.precedence(), s.name.isLeftAssoc());
      push(top, s.pos, s.name);
      ident();
      top = simplePattern();
      if (TreeInfo.isEmptySequence(top)) {
        syntaxError(top.pos, "2nd argument to binary op "+ s.name + " may not be empty sequence pattern", false);
      }
    }
    reduceStack(false, base, top, 0, true)
  }

  /** simplePattern ::= varid
  *                 | `_'
  *                 | literal
  *                 | `<' xmlPatternTop
  *                 | StableId [ `(' [Patterns] `)' ]
  *                 | `(' [Patterns] `)'
  *                 |                     (word: empty - nothing)
  */
  def simplePattern(): Tree = s.token match {
    case RPAREN | COMMA =>
      make.Sequence(s.pos, Tree.EMPTY_ARRAY) // ((nothing))
    case IDENTIFIER | THIS =>
      if (s.name == BAR) {
        make.Sequence(s.pos, Tree.EMPTY_ARRAY); // ((nothing))
      } else if(( s.name == LT )&&( unit.global.xmlMarkup )) {
        xmlPatternTop()
      } else {
        var t = stableId();
        while (s.token == LPAREN) {
          var ts = Tree.EMPTY_ARRAY;
          accept(LPAREN);
          if (s.token != RPAREN)
            ts = patterns();
          accept(RPAREN);
          t = make.Apply(s.pos, convertToTypeId(t), ts);
        }
        t
      }
    case USCORE =>
      make.Ident(s.skipToken(), Names.PATTERN_WILDCARD)
    case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL =>
      literal(true)
    case LPAREN =>
      val p = s.pos;
      s.nextToken();
      val ts = if (s.token == RPAREN) Tree.EMPTY_ARRAY else patterns();
      var t: Tree = null;
      if (ts.length == 1 && !ts(0).isInstanceOf[Tree$Alternative])  {
        t = ts(0);
      } else {
        t = pN.flattenSequence(make.Sequence(s.pos, ts));
        t = pN.elimSequence(t);
      }
      accept(RPAREN);
      t
    case _ =>
      syntaxError("illegal start of pattern", true)
  }

  def xmlTag():Tuple2[Name, ListMap[Name,String]] = {
    var empty = false;

    /* [40] STag         ::= '<' Name (S Attribute)* S?
     * [41] Attribute    ::= Name Eq AttValue
    *  [44] EmptyElemTag ::= '<' Name (S Attribute)* S?
    */
    val elemName = s.xmlName();
    s.xmlSpaceOpt();
    var attrMap = ListMap.Empty[Name,String];
    while( s.xml_isNameStart() ) {
      val attrName:Name = s.xmlName();
      s.xmlSpaceOpt();
      s.xmlToken('=');
      s.xmlSpaceOpt();
      var attrValue:String = "";
      val endch:char = s.ch.asInstanceOf[char];
      endch match {
        case '"' | '\'' => {
          s.nextch();
          attrValue = s.xmlAttribValue( endch );
          s.xmlToken( endch.asInstanceOf[char] );
          s.xmlSpaceOpt();
        }
        case _ => s.xml_syntaxError("' or \" delimited value expected here");
      }
      if( attrMap.contains( attrName )) {
        s.xml_syntaxError("attribute "+attrName+" may only be defined once");
      }
      attrMap = attrMap.update( attrName, attrValue );
    }
    Tuple2( elemName, attrMap );
  }

  /* [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'                 */
  def xmlEndTag(n:Name) = {
    s.xmlToken('/');
    if( n != s.xmlName() ) s.xml_syntaxError("expected closing tag of "+n);
    s.xmlSpaceOpt();
    s.xmlToken('>')
  }

  /** '<' xmlExpr ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   **               | xmlTag1 '\' '>'
   */
  def xmlExpr():Tree = {
    val pos = s.pos;
    val Tuple2(elemName, attrMap) = xmlTag();
    if( s.ch == '/' ) {
      s.xmlToken('/');
      s.xmlToken('>');
      makeXML( pos, elemName, Tree.EMPTY_ARRAY );
    } else {
      s.xmlToken('>');                      /* handle XML content: */
      s.xmlSpaceOpt();
      val ts = new myTreeList();
      var exit = false;
      while( !exit ) {
        /* Console.println("read '"+s.ch.asInstanceOf[char]+"'"); */
        s.ch match {
          case '<' => {                                  /* tag */
            s.nextch();
            s.ch match {
              case '/' => exit = true;
              case '!' =>
                s.xmlComment();
              case _ =>
                /* search end tag */
                ts.append( xmlExpr() );
	        s.xmlSpaceOpt();
            }
          }

          case '{' => {                                 /* Scala block */
            while( s.ch == '{' ) {
              s.nextToken();
              s.nextToken();
              val bs = new myTreeList();
              val b = expr(true,false); //block( s.pos );
              if( s.token != RBRACE ) {
                s.xml_syntaxError(" expected end of Scala block");
              }
	      s.xmlSpaceOpt();
              ts.append( b );
            }
          }
          case _ => {                                   /* text ? */
            val pos = s.pos;
            ts.append( gen.mkStringLit( pos, s.xmlText() ));
          }
        }
      }
      xmlEndTag( elemName );
      if( attrMap.isEmpty )
	makeXML( pos, elemName, ts.toArray() );
      else
	makeXML( pos, elemName, ts.toArray(), attrMap );
    }
  }

  /** top level xml pattern, resynchronizes after succesful parse
   *  see xmlPattern
  */
  def xmlPatternTop():Tree = {
    val t = xmlPattern();
    s.nextToken();
    t
  }

  /** '<' xmlPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
   *                    | Name [S] '\' '>'
   */
  def xmlPattern():Tree = {
    val pos = s.pos;
    val elemName = s.xmlName();
    s.xmlSpaceOpt();
    if( s.ch == '/' ) {
      s.xmlToken('/');
      s.xmlToken('>');
      makeXMLpat( pos, elemName, Tree.EMPTY_ARRAY );
    } else { /* handle XML content: */
      s.xmlToken('>');
      val ts = new myTreeList();
      var exit = false;
      while( !exit ) {
        //Console.print("["+s.ch.asInstanceOf[char]+"]");
        s.ch match {
          case '<' => {                                  /* tag */
            s.nextch();
            if( s.ch != '/' ) { /* search end tag */
              ts.append( xmlPattern() );
            } else {
              exit = true
            }
          }
          case '{' => {                            /* Scala patterns */
            while( s.ch == '{' ) {
              s.nextToken();
              s.nextToken();
              val ps = patterns();
              if( s.token != RBRACE ) {
                s.xml_syntaxError(" expected end of Scala block");
              }
              ts.append( ps );
            }
          }
          case _ => {                                   /* text  */
            val pos = s.pos;
            ts.append( gen.mkStringLit( pos, s.xmlText() ));
          }
	}
      }
      xmlEndTag( elemName );
      makeXMLpat( pos, elemName, ts.toArray() );
    }
  }
////////// MODIFIERS ////////////////////////////////////////////////////////////

  /** Modifiers ::= {Modifier}
  *  Modifier  ::= final
  *              | private
  *              | protected
  *              | override
  *              | abstract
  */
  def modifiers(): int = {
    pushComment();
    var mods = 0;
    while (true) {
      var mod = 0;
      s.token match {
        case ABSTRACT =>
          mod = Modifiers.ABSTRACT;
        case FINAL =>
          mod = Modifiers.FINAL;
        case SEALED =>
          mod = Modifiers.SEALED;
        case PRIVATE =>
          mod = Modifiers.PRIVATE;
        case PROTECTED =>
          mod = Modifiers.PROTECTED;
        case OVERRIDE =>
          mod = Modifiers.OVERRIDE;
        case _ =>
          return mods;
      }
      if ((mods & mod) != 0)
        syntaxError(s.pos, "repeated modifier", false);
      mods = mods | mod;
      s.nextToken();
    }
    0 //dummy
  }

  /** LocalModifiers ::= {LocalModifier}
  *  LocalModifier  ::= final
  *                   | private
  */
  def localClassModifiers(): int = {
    var mods = 0;
    while (true) {
      var mod = 0;
      s.token match {
        case ABSTRACT =>
          mod = Modifiers.ABSTRACT;
        case FINAL =>
          mod = Modifiers.FINAL;
        case SEALED =>
          mod = Modifiers.SEALED;
        case _ =>
          return mods;
      }
      if ((mods & mod) != 0)
        syntaxError(s.pos, "repeated modifier", false);
      mods = mods | mod;
      s.nextToken();
    }
    0 //dummy
  }

//////// PARAMETERS //////////////////////////////////////////////////////////

  /** ParamClauses ::= {ParamClause}
  */
  def paramClauses(): Array[Array[Tree$ValDef]] = {
    val ts = new ArrayList();
    while (s.token == LPAREN)
      ts.add(paramClause());
    ts.toArray(new Array[Array[Tree$ValDef]](ts.size()).asInstanceOf[Array[java.lang.Object]])
      .asInstanceOf[Array[Array[Tree$ValDef]]]
  }

  /** ParamClauseOpt ::= [ParamClause]
  */
  def paramClauseOpt(): Array[Array[Tree$ValDef]] =
    if (s.token == LPAREN) NewArray.ValDefArray(paramClause())
    else Tree.ValDef_EMPTY_ARRAY_ARRAY;

  /** ParamClause ::= `(' [Param {`,' Param}] `)'
  */
  def paramClause(): Array[Tree$ValDef] = {
    val pos = accept(LPAREN);
    val params = new myTreeList();
    if (s.token != RPAREN) {
      params.append(param());
      while (s.token == COMMA) {
        s.nextToken();
        params.append(param());
      }
    }
    accept(RPAREN);
    params.copyTo(new Array[Tree$ValDef](params.length()))
      .asInstanceOf[Array[Tree$ValDef]]
  }

  /** Param ::= [def] Id `:' Type [`*']
  */
  def param(): Tree$ValDef = {
    val pos = s.pos;
    var mods = Modifiers.PARAM;
    if (s.token == DEF) {
      mods = mods | Modifiers.DEF;
      s.nextToken();
    }
    val name = ident();
    accept(COLON);
    var tp = typ();
    if (s.token == IDENTIFIER && s.name == STAR) {
      s.nextToken();
      mods = mods | Modifiers.REPEATED;
      tp = make.AppliedType(
        tp.pos,
        scalaDot(tp.pos, Names.Seq.toTypeName()),
        NewArray.Tree(tp));
    }
    make.ValDef(pos, mods, name, tp, Tree.Empty)
  }

  /** TypeParamClauseOpt ::= [`[' TypeParam {`,' TypeParam} `]']
  *  FunTypeParamClauseOpt ::= [`[' FunTypeParam {`,' FunTypeParam} `]']
  */
  def typeParamClauseOpt(variant: boolean): Array[Tree$AbsTypeDef] = {
    val params = new myTreeList();
    if (s.token == LBRACKET) {
      s.nextToken();
      params.append(typeParam(variant));
      while (s.token == COMMA) {
        s.nextToken();
        params.append(typeParam(variant));
      }
      accept(RBRACKET);
    }
    params.copyTo(new Array[Tree$AbsTypeDef](params.length()))
      .asInstanceOf[Array[Tree$AbsTypeDef]];
  }

  /** TypeParam   ::= [`+' | `-'] FunTypeParam
  *  FunTypeParam ::= Id TypeBounds
  */
  def typeParam(variant: boolean): Tree = {
    var mods = Modifiers.PARAM;
    if (variant && s.token == IDENTIFIER) {
      if (s.name == PLUS) {
        s.nextToken();
        mods = mods | Modifiers.COVARIANT;
      } else if (s.name == MINUS) {
        s.nextToken();
        mods = mods | Modifiers.CONTRAVARIANT;
      }
    }
    typeBounds(s.pos, mods, ident())
  }

  /** TypeBounds ::= [`>:' Type] [`<:' Type]
  */
  def typeBounds(pos: int, mods: int, name: Name): Tree = {
    val lobound =
      if (s.token == SUPERTYPE) { s.nextToken(); typ() }
      else scalaDot(pos, Names.All.toTypeName());
    val hibound =
      if (s.token == SUBTYPE) { s.nextToken(); typ() }
      else scalaDot(pos, Names.Any.toTypeName());
    make.AbsTypeDef(pos, mods, name.toTypeName(), hibound, lobound)
  }

//////// DEFS ////////////////////////////////////////////////////////////////

    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    def importClause(): Array[Tree] = {
      accept(IMPORT);
      val ts = new myTreeList();
      ts.append(importExpr());
      while (s.token == COMMA) {
        s.nextToken();
        ts.append(importExpr());
      }
      ts.toArray()
    }

  /**  ImportRef ::= StableId `.' (Id | `_' | ImportSelectors)
  */
  def importExpr(): Tree = {
    var t: Tree = null;
    val startpos = s.pos;
    var pos = 0;
    if (s.token == THIS) {
      t = make.This(s.skipToken(), TypeNames.EMPTY);
      t = make.Select(accept(DOT), t, ident());
      pos = accept(DOT);
    } else {
      val i: Tree$Ident = make.Ident(s.pos, ident());
      pos = accept(DOT);
      if (s.token == THIS) {
        s.nextToken();
        t = make.This(i.pos, i.name.toTypeName());
        t = make.Select(accept(DOT), t, ident());
        pos = accept(DOT);
      } else {
        t = i;
      }
    }
    while (true) {
      if (s.token == USCORE) {
        s.nextToken();
        return make.Import(startpos, t, NewArray.Name(Names.IMPORT_WILDCARD));
      } else if (s.token == LBRACE) {
        return make.Import(startpos, t, importSelectors());
      } else {
        val name = ident();
        if (s.token == DOT) {
          t = make.Select(pos, t, name);
          pos = accept(DOT);
        } else {
          return make.Import(startpos, t, NewArray.Name(name, name));
        }
      }
    }
    null //dummy
  }

  /** ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
  */
  def importSelectors(): Array[Name] = {
    val names = new LinkedList();
    accept(LBRACE);
    var isLast = importSelector(names);
    while (!isLast && s.token == COMMA) {
      s.nextToken();
      isLast = importSelector(names);
    }
    accept(RBRACE);
    names.toArray(new Array[Name](0).asInstanceOf[Array[java.lang.Object]]).asInstanceOf[Array[Name]]
  }

  /** ImportSelector ::= Id [`=>' Id | `=>' `_']
  */
  def importSelector(names: LinkedList/*<Name>*/): boolean =
    if (s.token == USCORE) {
      s.nextToken();
      names.add(Names.IMPORT_WILDCARD);
      true
    } else {
      val name = ident();
      names.add(name);
      if (s.token == ARROW) {
        s.nextToken();
        if (s.token == USCORE) {
          s.nextToken();
          names.add(Names.IMPORT_WILDCARD);
        } else {
          names.add(ident());
        }
      } else {
        names.add(name);
      }
      false
    }

  /** Def    ::= val PatDef
   *           | var VarDef
   *           | def FunDef
   *           | type TypeDef
   *           | ClsDef
   *  Dcl    ::= val ValDcl
   *           | var ValDcl
   *           | def FunDcl
   *           | type TypeDcl
   */
  def defOrDcl(mods: int): Array[Tree] = {
    s.token match {
      case VAL =>
		patDefOrDcl(mods);
      case VAR =>
        varDefOrDcl(mods);
      case DEF =>
        funDefOrDcl(mods);
      case TYPE =>
        s.nextToken();
        val ts = new myTreeList();
        ts.append(popComment(typeDefOrDcl(mods)));
        ts.toArray()
      case _ =>
        clsDef(mods)
    }
  }

  /**  ClsDef ::= ([case] class | trait) ClassDef {`,' ClassDef}
   *            | [case] object ObjectDef {`,' ObjectDef}
   */
  def clsDef(mods: int): Array[Tree] = {
    s.token match {
      case TRAIT =>
        classDef(mods | Modifiers.TRAIT | Modifiers.ABSTRACT);
      case CLASS =>
        classDef(mods);
      case CASECLASS =>
        classDef(mods | Modifiers.CASE);
      case OBJECT =>
        objectDef(mods);
      case CASEOBJECT =>
        objectDef(mods | Modifiers.CASE);
      case _ =>
        NewArray.Tree(syntaxError("illegal start of definition", true))
    }
  }

  /** PatDef ::= Pattern2 [`:' Type] `=' Expr
   *  ValDcl ::= Id `:' Type
   */
  def patDefOrDcl(mods: int): Array[Tree] = {
	var lhs = new myTreeList();
	do {
	  s.nextToken();
	  lhs.append(pattern2());
	} while (s.token == COMMA);
	val tp = typedOpt();
	val rhs = if (tp == Tree.Empty || s.token == EQUALS) equalsExpr() else Tree.Empty;
	val ls = lhs.toArray();
	val ts = new myTreeList();
	var i = 0;
	if (rhs == Tree.Empty) {
	  while (i < ls.length) {
		ls(i) match {
		  case Tree$Ident(name) =>
			ts.append(popComment(
				make.ValDef(ls(i).pos, mods | Modifiers.DEFERRED, name, tp.duplicate(), Tree.Empty)));
		  case t =>
			syntaxError(t.pos, "cannot defer pattern definition", false);
		}
		i = i + 1;
	  }
	} else {
	  while (i < ls.length) {
		ls(i) match {
		  case Tree$Ident(name) =>
			ts.append(popComment(
				make.ValDef(ls(i).pos, mods, name, tp.duplicate(), rhs.duplicate())));
		  case t =>
			ts.append(popComment(
				make.PatDef(t.pos, mods, t, rhs.duplicate())));
		}
		i = i + 1;
	  }
	}
	ts.toArray()
  }

  /** VarDef ::= Id [`:' Type] `=' Expr
   *           | Id `:' Type `=' `_'
   *  VarDcl ::= Id `:' Type
   */
  def varDefOrDcl(mods: int): Array[Tree] = {
    var newmods = mods | Modifiers.MUTABLE;
    val names = new Buffer[Pair[Int, Name]];
	do {
	  s.nextToken();
	  names.append(Pair(s.pos, ident()));
	} while (s.token == COMMA);
	val tp = typedOpt();
	val rhs = if (tp == Tree.Empty || s.token == EQUALS) {
	  accept(EQUALS);
	  if (tp != Tree.Empty && s.token == USCORE) {
		  s.nextToken();
		  Tree.Empty
	  } else
		  expr();
	} else {
	  newmods = newmods | Modifiers.DEFERRED;
	  Tree.Empty;
	}
	val ts = new myTreeList();
	names foreach { case Pair(p, n) =>
		ts.append(popComment(
			make.ValDef(p, newmods, n, tp.duplicate(), rhs.duplicate())));
	}
	ts.toArray()
  }

  /** FunDef ::= Id [FunTypeParamClause] {ParamClauses} [`:' Type] `=' Expr
   *           | this ParamClause `=' ConstrExpr
   *  FunDcl ::= Id [FunTypeParamClause] {ParamClauses} `:' Type
   */
  def funDefOrDcl(mods: int): Array[Tree] = {
    val ts = new myTreeList();
    s.nextToken();
    if (s.token == THIS) {
      val pos = s.pos;
      s.nextToken();
      val vparams = NewArray.ValDefArray(paramClause());
      accept(EQUALS);
      ts.append(popComment(
      	make.DefDef(
        	pos, mods, Names.CONSTRUCTOR,
        	Tree.AbsTypeDef_EMPTY_ARRAY, vparams, Tree.Empty,
        	constrExpr())));
    } else {
	  var newmods = mods;
	  val lhs = new Buffer[Tuple4[Int, Name, Array[Tree$AbsTypeDef], Array[Array[Tree$ValDef]]]];
	  var loop = true;
	  while (loop) {
		lhs.append(Tuple4(s.pos, ident(),
		                  typeParamClauseOpt(false),
		                  paramClauses()));
		if (s.token == COMMA)
		  s.nextToken();
		else
		  loop = false;
	  }
	  val restype = typedOpt();
	  val rhs = if (restype == Tree.Empty || s.token == EQUALS) {
		equalsExpr();
	  } else {
		newmods = newmods | Modifiers.DEFERRED;
		Tree.Empty;
	  }
	  lhs foreach { case Tuple4(p, n, tp, vp) =>
		  ts.append(popComment(
			  make.DefDef(p, newmods, n, tp, vp, restype.duplicate(),
			              rhs.duplicate())));
	  }
	}
	ts.toArray()
  }

  /** ConstrExpr      ::=  SelfInvocation
  *                    |  `{' SelfInvocation {`;' BlockStat} `}'
  *  SelfInvocation  ::= this ArgumentExpr
  */
  def constrExpr(): Tree =
    if (s.token == LBRACE) {
      val pos = s.skipToken();
      val statlist = new myTreeList();
      statlist.append(selfInvocation());
      val stats =
        if (s.token == SEMI) { s.nextToken(); blockStatSeq(statlist) }
        else statlist.toArray();
      accept(RBRACE);
      block(pos, stats)
    } else {
      selfInvocation()
    }

  /** SelfInvocation  ::= this ArgumentExprs
  */
  def selfInvocation(): Tree = {
    val pos = s.pos;
    accept(THIS);
    make.Apply(
      s.pos, make.Ident(pos, Names.CONSTRUCTOR), argumentExprs());
  }

  /** TypeDef ::= Id `=' Type
  *  TypeDcl ::= Id TypeBounds
  */
  def typeDefOrDcl(mods: int): Tree = {
    val pos = s.pos;
    val name = ident().toTypeName();
    s.token match {
      case LBRACKET =>
        val tparams = typeParamClauseOpt(true);
        accept(EQUALS);
        make.AliasTypeDef(pos, mods, name, tparams, typ())
      case EQUALS =>
        s.nextToken();
        make.AliasTypeDef(pos, mods, name, Tree.AbsTypeDef_EMPTY_ARRAY, typ())
      case SUPERTYPE | SUBTYPE | SEMI | COMMA | RBRACE =>
        typeBounds(pos, mods | Modifiers.DEFERRED, name)
      case _ =>
        syntaxError("`=', `>:', or `<:' expected", true)
    }
  }

  /** ClassDef ::= Id [TypeParamClause] [ParamClause] [`:' SimpleType] ClassTemplate
   */
  def classDef(mods: int): Array[Tree] = {
	val lhs = new Buffer[Tuple4[Int, Name, Array[Tree$AbsTypeDef], Array[Array[Tree$ValDef]]]];
	do {
	  s.nextToken();
	  lhs.append(Tuple4(s.pos,
	                    ident().toTypeName(),
		                typeParamClauseOpt(true),
		                paramClauseOpt()));
    } while (s.token == COMMA);
	val thistpe = simpleTypedOpt();
    val template = classTemplate();
    val ts = new myTreeList();
	lhs foreach { case Tuple4(p, n, tp, vp) =>
		ts.append(popComment(
			make.ClassDef(p, mods, n, tp, vp,
                          thistpe.duplicate(),
                          template.duplicate().asInstanceOf[Tree$Template])));
	}
	ts.toArray()
  }

  /** ObjectDef       ::= Id [`:' SimpleType] ClassTemplate
   */
  def objectDef(mods: int): Array[Tree] = {
    val lhs = new Buffer[Pair[Int, Name]];
	do {
	  s.nextToken();
	  lhs.append(Pair(s.pos, ident()));
    } while (s.token == COMMA);
	val thistpe = simpleTypedOpt();
    val template = classTemplate();
    val ts = new myTreeList();
	lhs foreach { case Pair(p, n) =>
		ts.append(popComment(
			make.ModuleDef(
      			p, mods, n, thistpe.duplicate(),
      			template.duplicate().asInstanceOf[Tree$Template])));
	}
	ts.toArray()
  }

  /** ClassTemplate ::= [`extends' Constr] {`with' Constr} [TemplateBody]
   */
  def classTemplate(): Tree$Template = {
    val pos = s.pos;
    val parents = new myTreeList();
    if (s.token == EXTENDS) {
      s.nextToken();
      parents.append(constr());
    } else {
      parents.append(scalaAnyRefConstr(pos));
    }
    parents.append(scalaObjectConstr(pos));
    if (s.token == WITH) {
      s.nextToken();
      template(parents)
    } else if (s.token == LBRACE) {
      make.Template(pos, parents.toArray(), templateBody());
    } else {
      if (!(s.token == SEMI || s.token == COMMA || s.token == RBRACE))
        syntaxError("`extends' or `{' expected", true);
      make.Template(
        pos, parents.toArray(), Tree.EMPTY_ARRAY);
    }
  }

////////// TEMPLATES ////////////////////////////////////////////////////////////


  /** Template  ::= Constr {`with' Constr} [TemplateBody]
  */
  def template(): Tree$Template =
    template(new myTreeList());

  def template(parents: myTreeList): Tree$Template = {
    val pos = s.pos;
    parents.append(constr());
    while (s.token == WITH) {
      s.nextToken();
      parents.append(constr());
    }
    val stats = if (s.token == LBRACE) templateBody() else Tree.EMPTY_ARRAY;
    make.Template(pos, parents.toArray(), stats)
  }

  /** Constr ::= StableId [TypeArgs] [`(' [Exprs] `)']
  */
  def constr(): Tree = {
    var t: Tree = convertToConstr(stableId());
    if (s.token == LBRACKET)
      t = make.AppliedType(s.pos, t, typeArgs());
    if (s.token == LPAREN)
      t = make.Apply(s.pos, t, argumentExprs());
    applyConstr(t)
  }

  /** TemplateBody ::= `{' [TemplateStat {`;' TemplateStat}] `}'
  */
  def templateBody(): Array[Tree] = {
    accept(LBRACE);
    var body = templateStatSeq();
    if (body.length == 0)
      body = NewArray.Tree(Tree.Empty);
    accept(RBRACE);
    body
  }

  /** Refinement ::= `{' [RefineStat {`;' RefineStat}] `}'
  */
  def refinement(): Array[Tree] = {
    accept(LBRACE);
    val body = refineStatSeq();
    accept(RBRACE);
    body
  }

/////// STATSEQS //////////////////////////////////////////////////////////////

  /** Packaging ::= package QualId `{' TopStatSeq `}'
  */
  def packaging(): Tree = {
    val pos = accept(PACKAGE);
    val pkg = qualId();
    accept(LBRACE);
    val stats = topStatSeq();
    accept(RBRACE);
    makePackaging(pos, pkg, stats);
  }

  /** TopStatSeq ::= [TopStat {`;' TopStat}]
  *  TopStat ::= Modifiers ClsDef
  *            | Packaging
  *            | Import
  *            |
  */
  def topStatSeq(): Array[Tree] = {
    val stats = new myTreeList();
    while (s.token != RBRACE && s.token != EOF) {
      if (s.token == PACKAGE) {
        stats.append(packaging());
      } else if (s.token == IMPORT) {
        stats.append(importClause());
      } else if (s.token == CLASS ||
                 s.token == CASECLASS ||
                 s.token == TRAIT ||
                 s.token == OBJECT ||
                 s.token == CASEOBJECT ||
                 isModifier()) {
        stats.append(clsDef(modifiers()));
      } else if (s.token != SEMI) {
        syntaxError("illegal start of class or object definition", true);
      }
      if (s.token != RBRACE && s.token != EOF) accept(SEMI);
    }
    stats.toArray()
  }

  /** TemplateStatSeq  ::= TemplateStat {`;' TemplateStat}
   *  TemplateStat     ::= Import
   *                     | Modifiers Def
   *                     | Modifiers Dcl
   *                     | Expr
   *                     |
   */
  def templateStatSeq(): Array[Tree] = {
    val stats = new myTreeList();
    while (s.token != RBRACE && s.token != EOF) {
      if (s.token == IMPORT) {
        stats.append(importClause());
      } else if (isExprIntro()) {
        stats.append(expr());
      } else if (isDefIntro() || isModifier()) {
        stats.append(defOrDcl(modifiers()));
      } else if (s.token != SEMI) {
        syntaxError("illegal start of definition", true);
      }
      if (s.token != RBRACE) accept(SEMI);
    }
    stats.toArray()
  }

  /** RefineStatSeq    ::= RefineStat {`;' RefineStat}
   *  RefineStat       ::= Dcl
   *                     | type TypeDef
   *                     |
   */
  def refineStatSeq(): Array[Tree] = {
    val stats = new myTreeList();
    while (s.token != RBRACE && s.token != EOF) {
      if (isDclIntro()) {
        stats.append(defOrDcl(0));
      } else if (s.token != SEMI) {
        syntaxError("illegal start of declaration", true);
      }
      if (s.token != RBRACE) accept(SEMI);
    }
    stats.toArray()
  }

  /** BlockStatSeq ::= { BlockStat `;' } [Expr]
   *  BlockStat    ::= Import
   *                 | Def
   *                 | LocalModifiers ClsDef
   *                 | Expr
   *                 |
   */
  def blockStatSeq(stats: myTreeList): Array[Tree] = {
    while ((s.token != RBRACE) && (s.token != EOF) && (s.token != CASE)) {
      if (s.token == IMPORT) {
        stats.append(importClause());
        accept(SEMI);
      } else if (isExprIntro()) {
        stats.append(expr(false, true));
        if (s.token != RBRACE && s.token != CASE) accept(SEMI);
      } else if (isDefIntro()) {
        stats.append(defOrDcl(0));
        accept(SEMI);
        if (s.token == RBRACE || s.token == CASE) {
          stats.append(gen.mkUnitLit(s.pos));
        }
      } else if (isLocalModifier()) {
        stats.append(clsDef(localClassModifiers()));
        accept(SEMI);
        if (s.token == RBRACE || s.token == CASE) {
          stats.append(gen.mkUnitLit(s.pos));
        }
      } else if (s.token == SEMI) {
        s.nextToken();
      } else {
        syntaxError("illegal start of statement", true);
      }
    }
    stats.toArray()
  }

  /** CompilationUnit ::= [ package QualId ( `;' | `{' TopStatSeq `}' ) ] TopStatSeq .
  */
  def compilationUnit(): Array[Tree] = {
    if (s.token == PACKAGE) {
      val pos = s.skipToken();
      val pkg = qualId();
      if (s.token == SEMI) {
        s.nextToken();
        NewArray.Tree(makePackaging(pos, pkg, topStatSeq()));
      } else {
        val stats = new myTreeList();
        accept(LBRACE);
        stats.append(makePackaging(pos, pkg, topStatSeq()));
        accept(RBRACE);
        stats.append(topStatSeq());
        stats.toArray()
      }
    } else {
      topStatSeq()
    }
  }
}
}
