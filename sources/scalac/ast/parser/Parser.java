/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast.parser;

import java.util.*;
import scalac.*;
import scalac.util.*;
import scalac.symtab.Modifiers;
import scalac.ast.*;
import Tree.*;

/** A recursive descent parser for the programming language Scala.
 *
 *  @author     Martin Odersky, Matthias Zenger
 *  @version    1.2
 */
public class Parser implements Tokens {

    /** the lexical analyzer
     */
    Scanner s;

    /** the tree factory
     */
    TreeFactory make;

    public Parser(Unit unit) {
        s = new Scanner(unit);
        make = unit.global.make;
    }

    /** this is the general parse method
     */
    public Tree[] parse() {
        Tree[] ts = s.unit.console ? templateStatSeq() : compilationUnit();
        accept(EOF);
        return ts;
    }

/////// ERROR HANDLING //////////////////////////////////////////////////////

    private void skip() {
	//System.out.println("<skipping> " + s.token2string(s.token));//DEBUG
        int nparens = 0;
	int nbraces = 0;
        while (true) {
            switch (s.token) {
	    case EOF:
		return;
	    case SEMI:
		if (nparens == 0 && nbraces == 0)
		    return;
		break;
	    case RPAREN:
		nparens--;
		break;
	    case RBRACE:
		if (nbraces == 0)
		    return;
		nbraces--;
		break;
	    case LPAREN:
		nparens++;
		break;
	    case LBRACE:
		nbraces++;
		break;
            }
	    //System.out.println("skipped: " + s.token2string(s.token));//DEBUG
            s.nextToken();
        }
    }

    Tree syntaxError(String msg, boolean skip) {
	return syntaxError(s.pos, msg, skip);
    }

    Tree syntaxError(int pos, String msg, boolean skip) {
        if (pos != s.errpos) {
            s.unit.error(pos, msg);
	    s.errpos = pos;
	}
        if (skip) skip();
        return make.Bad(pos);
    }

    int accept(int token) {
	int pos = s.pos;
        if (s.token != token) {
            int errpos = ((s.pos >>> Position.LINESHIFT) >
			  (s.lastpos >>> Position.LINESHIFT)) ?
		s.lastpos : s.pos;
            syntaxError(errpos, s.token2string(token) + " expected but " +
			s.token2string(s.token) + " found.", true);
        }
	if (s.token == token) s.nextToken();
	return pos;
    }

/////// TOKEN CLASSES //////////////////////////////////////////////////////

    boolean isModifier() {
        return (s.token == ABSTRACT)
	    || (s.token == FINAL)
	    || (s.token == PRIVATE)
	    || (s.token == PROTECTED)
//          || (s.token == QUALIFIED)
	    || (s.token == OVERRIDE);
    }

    boolean isLocalClassModifier() {
        return (s.token == ABSTRACT)
	    || (s.token == FINAL);
    }

    boolean isDefIntro() {
	switch (s.token) {
	case VAL: case VAR: case DEF: case CONSTR: case TYPE:
	case OBJECT: case CASEOBJECT: case CLASS: case CASECLASS: case TRAIT:
	    return true;
	default:
	    return false;
	}
    }

    boolean isDclIntro() {
	switch (s.token) {
	case VAL: case VAR: case DEF: case CONSTR: case TYPE:
	    return true;
	default:
	    return false;
	}
    }

    boolean isExprIntro() {
	switch (s.token) {
	case CHARLIT: case INTLIT: case LONGLIT:
	case FLOATLIT: case DOUBLELIT: case STRINGLIT:
	case SYMBOLLIT: case TRUE: case FALSE: case NULL: case IDENTIFIER:
	case THIS: case SUPER: case IF:
	case FOR: case NEW: case USCORE:
	case LPAREN: case LBRACE:
	    return true;
	default:
	    return false;
	}
    }

/////// TREE CONSTRUCTION ////////////////////////////////////////////////////

    /** Name supply
     */
    int fresh = 0;

    Name fresh() {
        return Name.fromString("x$" + (fresh++));
    }

    /** Create tree representing binary operation expression or pattern.
     */
    Tree makeBinop(boolean isExpr, int pos, Tree left, Name op, Tree right) {
	if (isExpr) {
	    if (op.isLeftAssoc()) {
		return make.Apply(pos,
		    make.Select(pos, left, NameTransformer.encode(op)),
		    new Tree[]{right});
	    } else {
		Name x = fresh();
		return make.Block(pos,
		    new Tree[]{
			make.ValDef(pos, 0, x, Tree.Empty, left),
			make.Apply(pos,
			    make.Select(pos, right, NameTransformer.encode(op)),
			    new Tree[]{make.Ident(left.pos, x)})});
	    }
	} else {
	    return make.Apply(pos,
		make.Ident(pos, NameTransformer.encode(op).toTypeName()),
		new Tree[]{left, right});
	}
    }


    Tree scalaDot(int pos, Name name) {
	return make.Select(pos, make.Ident(pos, Names.scala), name);
    }

    Tree scalaBooleanDot(int pos, Name name) {
	return make.Select(pos, scalaDot(pos, Names.Boolean), name);
    }

    Tree scalaObjectConstr(int pos) {
	return make.Apply(
	    pos, scalaDot(pos, Names.Object.toConstrName()), Tree.EMPTY_ARRAY);
    }

    /** Create tree for for-comprehension <for (enums) do body> or
     *   <for (enums) yield body> where mapName and flatmapName are chosen
     *  corresponding to whether this is a for-do or a for-yield.
     */
    Tree makeFor(int pos, Tree[] enums, Name mapName, Name flatmapName, Tree body) {
        switch (enums[0]) {
	case PatDef(int mods, Tree pat, Tree rhs):
	    if (enums.length == 1)
		return makeFor1(pos, mapName, pat, rhs, body);
	    Tree[] newenums = new Tree[enums.length - 1];
	    switch (enums[1]) {
	    case PatDef(int mods2, Tree pat2, Tree rhs2):
		System.arraycopy(enums, 1, newenums, 0, newenums.length);
		return makeFor1(pos, flatmapName, pat, rhs,
				makeFor(enums[1].pos, newenums, mapName, flatmapName, body));
	    default:
		System.arraycopy(enums, 2, newenums, 1, newenums.length - 1);
		newenums[0] = make.PatDef(
		    enums[0].pos, mods, pat,
		    makeFor1(enums[1].pos, Names.filter, pat.duplicate(), rhs, enums[1]));
		return makeFor(pos, newenums, mapName, flatmapName, body);
	    }
	default:
	    throw new ApplicationError();
        }
    }

    //where
	Tree makeFor1(int pos, Name name, Tree pat, Tree rhs, Tree body) {
	    Tree cont;
	    switch (pat) {
	    case Ident(Name name1):
		cont = make.Function(pos,
		    new Tree.ValDef[]{
			(ValDef)
			make.ValDef(pat.pos, Modifiers.PARAM, name1, Tree.Empty, Tree.Empty)},
		    body);
		break;
	    default:
		cont = make.Visitor(pos, new Tree.CaseDef[]{
		    (CaseDef)make.CaseDef(pos, pat, Tree.Empty, body)});
	    }
	    return make.Apply(pos, make.Select(pos, rhs, name), new Tree[]{cont});
	}

    /** Convert tree to formal parameter list
     */
    ValDef[] convertToParams(Tree t) {
	switch (t) {
	case Function(ValDef[] params, Tree.Empty):
	    return params;
	case Ident(_):
	case Typed(Ident(_), _):
	    return new ValDef[]{convertToParam(t)};
	case Block(Tree[] stats):
	    if (stats.length == 0) return Tree.ValDef_EMPTY_ARRAY;
	}
	syntaxError(t.pos, "malformed formal parameter list", false);
	return Tree.ValDef_EMPTY_ARRAY;
    }

    /** Convert list of trees to formal parameter list
     */
    ValDef[] convertToParams(Tree[] ts) {
        ValDef[] res = new ValDef[ts.length];
        for (int i = 0; i < res.length; i++)
	    res[i] = convertToParam(ts[i]);
	return res;
    }

    /** Convert tree to formal parameter
     */
    ValDef convertToParam(Tree tree) {
	switch (tree) {
	case Ident(Name name):
	    return (ValDef)make.ValDef(
		tree.pos, Modifiers.PARAM, name, Tree.Empty, Tree.Empty);
	case Typed(Ident(Name name), Tree tpe):
	    return (ValDef)make.ValDef(
		tree.pos, Modifiers.PARAM, name, tpe, Tree.Empty);
	default:
	    Tree tpe = syntaxError(tree.pos, "not a legal formal parameter", false);
	    return (ValDef)make.ValDef(
		tree.pos, Modifiers.PARAM, Names.ERROR, tpe, Tree.Empty);
	}
    }

    /** Convert (qual)ident to type identifier
     */
    Tree convertToTypeId(Tree t) {
	switch (t) {
	case Ident(Name name):
	    return make.Ident(t.pos, name.toTypeName());
	case Select(Tree qual, Name name):
	    return make.Select(t.pos, qual, name.toTypeName());
	default:
	    return t;
	}
    }

    /** Convert (qual)ident to constructor identifier
     */
    Tree convertToConstr(Tree t) {
	switch (t) {
	case Apply(Tree fn, Tree[] args):
	    return make.Apply(t.pos, convertToConstr(fn), args);
	case TypeApply(Tree fn, Tree[] args):
	    return make.TypeApply(t.pos, convertToConstr(fn), args);
	case Ident(Name name):
	    return make.Ident(t.pos, name.toConstrName());
	case Select(Tree qual, Name name):
	    return make.Select(t.pos, qual, name.toConstrName());
	default:
	    return syntaxError(t.pos, "class constructor expected", false);
	}
    }

    /** Complete unapplied constructor with `()' arguments
     */
    Tree applyConstr(Tree t) {
	switch (t) {
	case Apply(_, _):
	    return t;
	default:
	    return make.Apply(t.pos, t, Tree.EMPTY_ARRAY);
	}
    }

/////// OPERAND/OPERATOR STACK /////////////////////////////////////////////////

    Tree[] operands = new Tree[8];
    int[] positions = new int[8];
    Name[] operators = new Name[8];
    int sp = 0;

    void push(Tree od, int pos, Name op) {
	if (sp == operands.length) {
	    Tree[] operands1 = new Tree[sp * 2];
	    System.arraycopy(operands, 0, operands1, 0, sp);
	    operands = operands1;
	    int[] positions1 = new int[sp * 2];
	    System.arraycopy(positions, 0, positions1, 0, sp);
	    positions = positions1;
	    Name[] operators1 = new Name[sp * 2];
	    System.arraycopy(operators, 0, operators1, 0, sp);
	    operators = operators1;
	}
	operands[sp] = od;
	positions[sp] = pos;
	operators[sp] = op;
	sp++;
    }

    Tree reduceStack(boolean isExpr, int base, Tree top,
		     int prec, boolean leftAssoc) {
	if (sp != base &&
	    operators[sp-1].precedence() == prec &&
	    operators[sp-1].isLeftAssoc() != leftAssoc) {
	    syntaxError(
		positions[sp-1],
		"left- and right-associative operators with same precedence may not be mixed",
		false);
	}
	while (sp != base &&
	       (prec < operators[sp-1].precedence() ||
		(leftAssoc && prec == operators[sp-1].precedence()))) {
	    sp--;
	    top = makeBinop(isExpr, positions[sp], operands[sp], operators[sp], top);
	}
	return top;
    }

/////// IDENTIFIERS AND LITERALS ////////////////////////////////////////////////////////////

    static final Name MINUS = Name.fromString("-");
    static final Name PLUS = Name.fromString("+");
    static final Name BANG = Name.fromString("!");
    static final Name TILDE = Name.fromString("~");
    static final Name STAR = Name.fromString("*");
    static final Name BAR  = Name.fromString("|");
    static final Name OPT  = Name.fromString("?");

    Name ident() {
        if (s.token == IDENTIFIER) {
            Name name = NameTransformer.encode(s.name);
            s.nextToken();
            return name;
        } else {
            accept(IDENTIFIER);
            return Names.ERROR;
        }
    }

    /** StableRef  ::=  StableId
     *               |  [Ident `.'] this
     *  SimpleType ::=  StableRef [`.' type]
     */
    Tree stableRef(boolean thisOK, boolean typeOK) {
	Tree t;
	if (s.token == THIS) {
	    t = make.This(s.skipToken(), Tree.Empty);
	    if (!thisOK || s.token == DOT)
		t = selectors(accept(DOT), t, typeOK);
	} else if (s.token == SUPER) {
	    t = make.Super(s.skipToken(), Tree.Empty);
	    t = selectors(accept(DOT), t, typeOK);
	} else {
	    t = make.Ident(s.pos, ident());
	    if (s.token == DOT) {
		int pos = s.skipToken();
		if (s.token == THIS) {
		    s.nextToken();
		    t = make.This(pos, convertToTypeId(t));
		    if (!thisOK || s.token == DOT)
			t = selectors(accept(DOT), t, typeOK);
		} else {
		    t = selectors(pos, t, typeOK);
		}
	    }
	}
	return t;
    }

    Tree selectors(int pos, Tree t, boolean typeOK) {
	if (typeOK && s.token == TYPE) {
	    s.nextToken();
	    return make.SingletonType(pos, t);
	} else {
	    t = make.Select(pos, t, ident());
	    if (s.token == DOT) {
		t = selectors(s.skipToken(), t, typeOK);
	    }
	    return t;
	}
    }

    /** StableId ::= [[Ident `.'] this `.' | super] {Id `.'} Id
     */
    Tree stableId() {
	return stableRef(false, false);
    }

    /** QualId ::= Id {`.' Id}
     */
    Tree qualId() {
	Tree id = make.Ident(s.pos, ident());
	if (s.token == DOT) return selectors(s.skipToken(), id, false);
	else return id;
    }

    /** SimpleExpr    ::= literal
     *                  | symbol [ArgumentExprs]
     *                  | null
     */
    Tree literal(boolean isPattern) {
	Tree t;
	switch (s.token) {
	case CHARLIT:
	    t = make.Literal(s.pos, new Character((char)s.intVal));
	    break;
	case INTLIT:
	    t = make.Literal(s.pos, new Integer((int)s.intVal));
	    break;
	case LONGLIT:
	    t = make.Literal(s.pos, new Long(s.intVal));
	    break;
	case FLOATLIT:
	    t = make.Literal(s.pos, new Float((float)s.floatVal));
	    break;
	case DOUBLELIT:
	    t = make.Literal(s.pos, new Double(s.floatVal));
	    break;
	case STRINGLIT:
	    t = make.Literal(s.pos, s.name.toString());
	    break;
	case TRUE:
	    t = make.Literal(s.pos, Boolean.TRUE);
	    break;
	case FALSE:
	    t = make.Literal(s.pos, Boolean.FALSE);
	    break;
	case NULL:
	    t = make.Ident(s.pos, Names.null_);
	    break;
	case SYMBOLLIT:
	    Tree symt = scalaDot(s.pos, Names.Symbol);
	    if (isPattern) symt = convertToTypeId(symt);
	    t = make.Apply(s.pos,
		symt,
		new Tree[]{make.Literal(s.pos, s.name.toString())});
	    s.nextToken();
	    if (s.token == LPAREN || s.token == LBRACE) {
		Tree labt = scalaDot(s.pos, Names.Labelled);
		if (isPattern) labt = convertToTypeId(labt);
		Tree listt = isPattern ? scalaDot(s.pos, Names.List.toTypeName())
		    : make.Select(s.pos, scalaDot(s.pos, Names.Predef), Names.List);
		t = make.Apply(s.pos,
		    labt,
		    new Tree[]{t, make.Apply(s.pos, listt, argumentExprs())});
	    }
	    return t;
	default:
	    return syntaxError("illegal literal", true);
	}
	s.nextToken();
	return t;
    }

//////// TYPES ///////////////////////////////////////////////////////////////

    /** TypedOpt ::= [`:' Type]
     */
    Tree typedOpt() {
        if (s.token == COLON) {
            s.nextToken();
            return type();
        } else {
            return Tree.Empty;
	}
    }

    /** SimpleTypedOpt ::= [`:' Type]
     */
    Tree simpleTypedOpt() {
        if (s.token == COLON) {
            s.nextToken();
            return simpleType();
        } else {
            return Tree.Empty;
	}
    }

    /** Types ::= Type {`,' Type}
     */
    Tree[] types() {
	TreeList ts = new TreeList();
	ts.append(type());
        while (s.token == COMMA) {
	    s.nextToken();
	    ts.append(type());
	}
	return ts.toArray();
    }

    /** Type ::= Type1 `=>' Type
     *         | `(' [Types] `)' `=>' Type
     *         | Type1
     */
    Tree type() {
	Tree t;
	if (s.token == LPAREN) {
	    s.nextToken();
	    if (s.token == RPAREN) {
		s.nextToken();
		int pos = accept(ARROW);
		return make.FunType(pos, Tree.EMPTY_ARRAY, type());
	    } else {
		t = type();
		if (s.token == COMMA) {
		    s.nextToken();
		    TreeList ts = new TreeList();
		    ts.append(t);
		    ts.append(types());
		    accept(RPAREN);
		    int pos = accept(ARROW);
		    return make.FunType(pos, ts.toArray(), type());
		} else {
		    accept(RPAREN);
		}
	    }
	} else {
	    t = type1();
	}
	if (s.token == ARROW)
	    return make.FunType(s.skipToken(), new Tree[]{t}, type());
	else
	    return t;
    }

    /** Type1 ::= SimpleType {with SimpleType} [Refinement]
     */
    Tree type1() {
	int pos = s.pos;
	Tree t = simpleType();
	if (s.token == WITH || s.token == LBRACE) {
	    TreeList ts = new TreeList();
	    ts.append(t);
	    while (s.token == WITH) {
		s.nextToken();
		ts.append(simpleType());
	    }
	    Tree[] rs = (s.token == LBRACE) ? refinement() : Tree.EMPTY_ARRAY;
	    return make.CompoundType(pos, ts.toArray(), rs);
	} else {
	    return t;
	}
    }

    /** SimpleType ::= SimpleType TypeArgs
     *               | SimpleType `#' Id
     *               | StableId
     *               | StableRef `.' type
     *               | `(' Type `)'
     */
    Tree simpleType() {
	int pos = s.pos;
	Tree t;
	if (s.token == LPAREN) {
	    s.nextToken();
	    t = type();
	    accept(RPAREN);
	} else {
	    t = convertToTypeId(stableRef(false, true));
	}
	while (true) {
	    if (s.token == HASH)
		t = make.SelectFromType(s.skipToken(), t, ident().toTypeName());
	    else if (s.token == LBRACKET)
		t = make.AppliedType(pos, t, typeArgs());
	    else break;
	}
	return t;
    }

    /** TypeArgs ::= `[' Types `]'
     */
    Tree[] typeArgs() {
        accept(LBRACKET);
	Tree[] ts = types();
	accept(RBRACKET);
	return ts;
    }

//////// EXPRESSIONS ////////////////////////////////////////////////////////

    /** EqualsExpr ::= `=' Expr
     */
    Tree equalsExpr() {
        accept(EQUALS);
        return expr();
    }

    /** Exprs ::= Expr {`,' Expr}
     */
    Tree[] exprs() {
        TreeList ts = new TreeList();
        ts.append(expr());
        while (s.token == COMMA) {
            s.nextToken();
            ts.append(expr());
        }
        return ts.toArray();
    }

    /** Expr     ::= Bindings `=>' Expr
     *             | if `(' Expr `)' Expr [[`;'] else Expr]
     *             | for `(' Enumerators `)' [do | yield] Expr
     *             | Designator `=' Expr
     *             | SimpleExpr ArgumentExprs `=' Expr
     *             | PostfixExpr [`:' Type1 | as Type1 | is Type1]
     *  Bindings ::= Id [`:' Type1]
     *             | `(' [Binding {`,' Binding}] `)'
     *  Binding  ::= Id [`:' Type]
     */
    Tree expr() {
	if (s.token == IF) {
	    int pos = s.skipToken();
	    accept(LPAREN);
	    Tree cond = expr();
	    accept(RPAREN);
	    Tree thenp = expr();
	    Tree elsep = Tree.Empty;
	    if (s.token == ELSE) {
		s.nextToken();
		elsep = expr();
	    } else {
		elsep = Tree.Empty;
	    }
	    return make.If(pos, cond, thenp, elsep) ;
	} else if (s.token == FOR) {
	    s.nextToken();
	    Tree[] enums;
	    if (s.token == LBRACE) {
		accept(LBRACE);
		enums = enumerators();
		accept(RBRACE);
	    } else {
		accept(LPAREN);
		enums = enumerators();
		accept(RPAREN);
	    }
	    if (s.token == DO) {
		return makeFor(s.skipToken(), enums, Names.foreach, Names.foreach, expr());
	    } else if (s.token == YIELD) {
		return makeFor(s.skipToken(), enums, Names.map, Names.flatmap, expr());
	    } else {
		return syntaxError("`do' or `yield' expected", true);
	    }
//	} else if (s.token == ARROW) {
//	    return make.Function(s.skipToken(), new ValDef[]{}, expr());
	} else {
	    Tree t = postfixExpr();
	    if (s.token == EQUALS) {
		switch (t) {
		case Ident(_):
		case Select(_, _):
		case Apply(_, _):
		    t = make.Assign(s.skipToken(), t, expr());
		}
	    } else if (s.token == COLON) {
		int pos = s.skipToken();
		Tree tp = type1();
		t = make.Typed(pos, t, tp);
	    } else if (s.token == AS || s.token == IS) {
		Name op = (s.token == AS) ? Names.as : Names.is;
		int pos = s.skipToken();
		t = make.TypeApply(pos, make.Select(pos, t, op), new Tree[]{type1()});
	    }
	    if (s.token == ARROW) {
		t = make.Function(s.skipToken(), convertToParams(t), expr());
	    }
	    return t;
	}
    }

    /** PostfixExpr   ::= InfixExpr [Id]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr Id InfixExpr
     */
    Tree postfixExpr() {
        int base = sp;
	Tree top = prefixExpr();
        while (s.token == IDENTIFIER) {
	    top = reduceStack(
		true, base, top, s.name.precedence(), s.name.isLeftAssoc());
	    push(top, s.pos, s.name);
	    ident();
	    if (isExprIntro()) {
		top = prefixExpr();
	    } else {
		sp--;
		int pos = positions[sp];
		Name postOp = operators[sp];
		top = reduceStack(true, base, operands[sp], 0, true);
		return make.Select(pos, top, NameTransformer.encode(postOp));
	    }
	}
	return reduceStack(true, base, top, 0, true);
    }

    /** PrefixExpr   ::= [op] SimpleExpr
     */
    Tree prefixExpr() {
	Tree t;
	if (s.token == IDENTIFIER &&
	    (s.name == MINUS ||
	     s.name == PLUS ||
	     s.name == TILDE ||
	     s.name == BANG)) {
            Name name = ident();
            t = make.Select(s.pos, simpleExpr(), name);
	} else {
	    t = simpleExpr();
	}
	return t;
    }

    /* SimpleExpr    ::= SimpleExpr1
     *                 | SimpleExpr ArgumentExprs
     *                 | new Template
     *                 | BlockExpr
     *                 | `(' [Expr] `)'
     *
     * SimpleExpr1   ::= literal
     *                 | null
     *                 | StableRef
     *                 | super `.' Id
     *                 | SimpleExpr `.' Id
     *                 | SimpleExpr TypeArgs
     */
    Tree simpleExpr() {
	Tree t;
	switch (s.token) {
	case CHARLIT:
	case INTLIT:
	case LONGLIT:
	case FLOATLIT:
	case DOUBLELIT:
	case STRINGLIT:
	case SYMBOLLIT:
	case TRUE:
	case FALSE:
	case NULL:
	    t = literal(false);
	    break;
	case IDENTIFIER:
	case THIS:
	    t = stableRef(true, false);
	    break;
	case SUPER:
	    int pos = s.skipToken();
	    t = make.Select(accept(DOT), make.Super(pos, Tree.Empty), ident());
	    break;
	case LPAREN:
	    int pos = s.skipToken();
	    if (s.token == RPAREN) {
		s.nextToken();
		t = make.Block(pos, Tree.EMPTY_ARRAY);
	    } else {
		t = expr();
		if (s.token == COMMA) {
		    int commapos = s.skipToken();
		    TreeList ts = new TreeList();
		    ts.append(t);
		    ts.append(exprs());
		    accept(RPAREN);
		    if (s.token == ARROW) {
			t = make.Function(pos, convertToParams(ts.toArray()), Tree.Empty);
		    } else {
			t = syntaxError(commapos, "`)' expected", false);
		    }
		} else {
		    accept(RPAREN);
		}
	    }
	    break;
	case LBRACE:
	    t = blockExpr();
	    break;
	case NEW:
	    t = make.New(s.skipToken(), template());
	    break;
	default:
	    return syntaxError("illegal start of expression", true);
	}
        while (true) {
            switch (s.token) {
	    case DOT:
		t = make.Select(s.skipToken(), t, ident());
		break;
	    case LBRACKET:
		switch (t) {
		case Ident(_):
		case Select(_, _):
		    t = make.TypeApply(s.pos, t, typeArgs());
		    break;
		default:
		    return t;
		}
		break;
	    case LPAREN:
	    case LBRACE:
		t = make.Apply(s.pos, t, argumentExprs());
		break;
	    default:
		return t;
            }
	}
    }

    /** ArgumentExprs ::= `(' [Exprs] `)'
     *                  | BlockExpr
     */
    Tree[] argumentExprs() {
	Tree[] ts = Tree.EMPTY_ARRAY;
	if (s.token == LBRACE) {
	    ts = new Tree[]{blockExpr()};
	} else {
	    accept(LPAREN);
	    if (s.token != RPAREN)
		ts = exprs();
	    accept(RPAREN);
	}
	return ts;
    }

    /** BlockExpr ::= `{' CaseClause {CaseClause} `}'
     *              | `{' Block `}'
     */
    Tree blockExpr() {
	Tree res;
	int pos = accept(LBRACE);
	if (s.token == CASE) {
	    TreeList stats = new TreeList();
	    do {
		stats.append(caseClause());
	    } while (s.token == CASE);
	    res = make.Visitor(
		pos, (CaseDef[]) stats.copyTo(new CaseDef[stats.length()]));
	} else {
	    res = block(pos);
	}
	accept(RBRACE);
	return res;
    }

    /** BlockConstr ::= `{' Block `}'
     */
    Tree blockConstr() {
	int pos = accept(LBRACE);
	Tree res = block(pos);
	switch (res) {
	case Block(Tree[] stats):
	    if (stats.length > 0)
		stats[stats.length - 1] = applyConstr(
		    convertToConstr(stats[stats.length - 1]));
	    else
		syntaxError(res.pos, "class constructor expected", false);
	}
	accept(RBRACE);
	return res;
    }

    /** Block ::= BlockStatSeq
     */
    Tree block(int pos) {
	Tree[] stats = blockStatSeq(new TreeList());
	if (stats.length == 1 && stats[0].isTerm()) return stats[0];
	else return make.Block(pos, stats);
    }

    /** CaseClause ::= case Pattern [if `(' Expr `)'] `=>' Block
     */
    Tree caseClause() {
	int pos = accept(CASE);
	Tree pat = pattern();
	Tree guard = Tree.Empty;
	if (s.token == IF) {
	    s.nextToken();
	    accept(LPAREN);
	    guard = expr();
	    accept(RPAREN);
	}
	accept(ARROW);
	return make.CaseDef(pos, pat, guard, block(s.pos));
    }

    /** Enumerators ::= Generator {`;' Enumerator}
     *  Enumerator  ::= Generator
     *                | Expr
     */
    Tree[] enumerators() {
	TreeList enums = new TreeList();
	enums.append(generator());
	while (s.token == SEMI) {
	    s.nextToken();
	    if (s.token == VAL) enums.append(generator());
	    else enums.append(expr());
	}
	return enums.toArray();
    }

    /** Generator ::= val Pattern `<-' Expr
     */
    Tree generator() {
	int pos = accept(VAL);
	Tree pat = pattern();
	accept(LARROW);
	Tree rhs = expr();
	if (!TreeInfo.isVarPattern(pat))
	    rhs = make.Apply(
		rhs.pos,
		make.Select(rhs.pos, rhs, Names.filter),
		new Tree[]{
		    make.Visitor(
			rhs.pos,
			new Tree.CaseDef[]{
			    (CaseDef)make.CaseDef(
				rhs.pos, pat.duplicate(), Tree.Empty,
				make.Literal(s.pos, Boolean.TRUE)),
			    (CaseDef)make.CaseDef(
				rhs.pos, make.Ident(rhs.pos, Names.WILDCARD), Tree.Empty,
				make.Literal(s.pos, Boolean.FALSE))})});
	return make.PatDef(pos, 0, pat, rhs);
    }

//////// PATTERNS ////////////////////////////////////////////////////////////

    /** Patterns ::= Pattern {`,' Pattern}
     */
    Tree[] patterns() {
        TreeList ts = new TreeList();
        ts.append(pattern());
        while (s.token == COMMA) {
            s.nextToken();
            ts.append(pattern());
        }
        return ts.toArray();
    }

    /**   Pattern  ::=  TreePattern { '|' TreePattern }
     */

    Tree pattern() {
	int pos = s.pos;
	Tree first = treePattern();
	if(( s.token == IDENTIFIER )&&( s.name == BAR )) {
	    TreeList choices = new TreeList();
	    choices.append( first );
	    while(( s.token == IDENTIFIER )&&( s.name == BAR )) {
		s.nextToken();
		choices.append( treePattern() );
	    }
	    return make.Alternative( pos, choices.toArray() );
	}
	return first;
    }

    /**   TreePattern  ::=  varid `:' Type1
     *                   |  `_' `:' Type1
     *                   |  SimplePattern [ '*' | '?' | '+' ]
     *                   |  SimplePattern {Id SimplePattern}
     */
    Tree treePattern() {
        int base = sp;
	Tree top = simplePattern();
	if (s.token == COLON) {
	    if (TreeInfo.isVarPattern(top))
		return make.Typed(s.skipToken(), top, type1());
	}
	if( s.token == IDENTIFIER )
	    {
		if ( s.name == STAR ) /*         p*  becomes  z@( |(p,z))       */
		    {
			s.nextToken();
			Name zname= fresh();
			Tree zvar = make.Ident( s.pos, zname );

			return make.Bind( s.pos, zname,
					  make.Alternative( s.pos, new Tree[] {
					      make.Subsequence( s.pos, Tree.EMPTY_ARRAY ),
					      make.Subsequence( s.pos, new Tree[] {
						  top,
						  zvar })
					  }));
		    }
		else if ( s.name == PLUS ) /*    p+   becomes   z@(p,(z| ))     */
		    {
			s.nextToken();
			Name zname= fresh();
			Tree zvar = make.Ident( s.pos, zname );

			return make.Bind( s.pos, zname,
					  make.Subsequence( s.pos, new Tree[] {
					      top,
					      make.Alternative( s.pos, new Tree[] {
						  zvar,
			 			  make.Subsequence( s.pos, Tree.EMPTY_ARRAY ) })
					  }));
		    }
		else if ( s.name == OPT ) /*    p?   becomes   (p| )            */
		    {
			s.nextToken();
			return make.Alternative( s.pos, new Tree[] {
			    top,
			    make.Subsequence( s.pos, Tree.EMPTY_ARRAY )});
		    }
	    }
	while ((s.token == IDENTIFIER)&&( s.name != BAR )) {
	    top = reduceStack(
		false, base, top, s.name.precedence(), s.name.isLeftAssoc());
	    push(top, s.pos, s.name);
	    ident();
	    top = simplePattern();
	}
	return reduceStack(false, base, top, 0, true);
    }

    /** SimplePattern ::= varid [ '@' SimplePattern ]
     *                 | `_'
     *                 | literal
     *                 | null
     *                 | StableId {ArgumentPatterns}
     *                 | `(' Patterns `)'
     *                 | ((nothing))
     */
    Tree simplePattern() {
	switch (s.token) {
	case COMMA:
	    return make.Subsequence( s.pos, Tree.EMPTY_ARRAY ); // ((nothing))
	case IDENTIFIER:
	    if( s.name == BAR )
		{
		    return make.Subsequence( s.pos, Tree.EMPTY_ARRAY ); // ((nothing))
		}
	    // else fall through to case THIS
	case THIS:
	    Tree t = stableId();
	    switch( t ) {
	    case Ident( Name name ):
		if(( name.isVariable() )&&( s.token == AT ))
		    {
			int pos = s.pos;
			s.nextToken();
			return make.Bind( pos, name, simplePattern() );
		    }
	    }
	    while (s.token == LPAREN) {
		t = make.Apply(s.pos, convertToTypeId(t), argumentPatterns());
	    }
	    return t;
	case USCORE:
	    return make.Ident(s.skipToken(), Names.WILDCARD);
	case CHARLIT:
	case INTLIT:
	case LONGLIT:
	case FLOATLIT:
	case DOUBLELIT:
	case STRINGLIT:
	case SYMBOLLIT:
	case TRUE:
	case FALSE:
	case NULL:
	    return literal(true);
	case LPAREN:
	    int p = s.pos;
	    s.nextToken();
	    Tree[] ts = patterns();
	    Tree   t;
	    if( ts.length == 1 )
		t = ts[ 0 ];
	    else
		t = make.Subsequence( s.pos, ts );
	    accept(RPAREN);
	    return t;
	default:
	    return syntaxError("illegal start of pattern", true);
	}
    }

    /** ArgumentPatterns ::= `(' [Patterns] `)'
     */
    Tree[] argumentPatterns() {
	Tree[] ts = Tree.EMPTY_ARRAY;
	accept(LPAREN);
	if (s.token != RPAREN)
	    ts = patterns();
	accept(RPAREN);
	return ts;
    }

////////// MODIFIERS ////////////////////////////////////////////////////////////

    /** Modifiers ::= {Modifier}
     *  Modifier  ::= final
     *              | private
     *              | protected
     *              | override
     *              | abstract
     */
    int modifiers() {
	int mods = 0;
        while (true) {
	    int mod;
            switch (s.token) {
	    case ABSTRACT:
		mod = Modifiers.ABSTRACTCLASS;
		break;
	    case FINAL:
		mod = Modifiers.FINAL;
		break;
	    case PRIVATE:
		mod = Modifiers.PRIVATE;
		break;
	    case PROTECTED:
		mod = Modifiers.PROTECTED;
		break;
	    case OVERRIDE:
		mod = Modifiers.OVERRIDE;
		break;
	    default:
		return mods;
            }
	    if ((mods & mod) != 0)
		syntaxError(s.pos, "repeated modifier", false);
	    mods |= mod;
            s.nextToken();
        }
    }

    /** LocalClassModifiers ::= {LocalClassModifier}
     *  LocalClassModifier  ::= final
     *                   | private
     */
    int localClassModifiers() {
	int mods = 0;
        while (true) {
	    int mod;
            switch (s.token) {
	    case ABSTRACT:
		mod = Modifiers.ABSTRACTCLASS;
		break;
	    case FINAL:
		mod = Modifiers.FINAL;
		break;
	    default:
		return mods;
            }
	    if ((mods & mod) != 0)
		syntaxError(s.pos, "repeated modifier", false);
	    mods |= mod;
            s.nextToken();
        }
    }

//////// PARAMETERS //////////////////////////////////////////////////////////

    /** ParamClauses ::= {ParamClause}
     */
    ValDef[][] paramClauses() {
        ArrayList ts = new ArrayList();
        while (s.token == LPAREN)
            ts.add(paramClause());
        return (ValDef[][])ts.toArray(new ValDef[ts.size()][]);
    }

    /** ParamClause ::= `(' [Param {`,' Param}] `)'
     */
    ValDef[] paramClause() {
        int pos = accept(LPAREN);
        TreeList params = new TreeList();
	if (s.token != RPAREN) {
            params.append(param());
	    while (s.token == COMMA) {
                s.nextToken();
		params.append(param());
	    }
        }
        accept(RPAREN);
        return (ValDef[])params.copyTo(new ValDef[params.length()]);
    }

    /** Param ::= [def] Id `:' Type [`*']
     */
    ValDef param() {
        int pos = s.pos;
        int mods = Modifiers.PARAM;
        if (s.token == DEF) {
            mods |= Modifiers.DEF;
            s.nextToken();
        }
	Name name = ident();
	accept(COLON);
	Tree tp = type();
	if (s.token == IDENTIFIER && s.name == STAR) {
	    s.nextToken();
	    mods |= Modifiers.REPEATED;
	    tp = make.AppliedType(tp.pos,
		scalaDot(tp.pos, Names.Seq.toTypeName()),
		new Tree[]{tp});
	}
        return (ValDef)make.ValDef(pos, mods, name, tp, Tree.Empty);
    }

    /** TypeParamClauseOpt ::= [`[' TypeParam {`,' TypeParam} `]']
     *  FunTypeParamClauseOpt ::= [`[' FunTypeParam {`,' FunTypeParam} `]']
     */
    TypeDef[] typeParamClauseOpt(boolean variant) {
        TreeList params = new TreeList();
	if (s.token == LBRACKET) {
	    s.nextToken();
	    params.append(typeParam(variant));
	    while (s.token == COMMA) {
		s.nextToken();
		params.append(typeParam(variant));
	    }
	    accept(RBRACKET);
	}
        return (TypeDef[])params.copyTo(new TypeDef[params.length()]);
    }

    /** TypeParam   ::= [`+' | `-'] FunTypeParam
     *  FunTypeParam ::= Id TypeBounds
     */
    Tree typeParam(boolean variant) {
	int mods = Modifiers.PARAM;
	if (variant && s.token == IDENTIFIER) {
	    if (s.name == PLUS) {
		s.nextToken();
		mods |= Modifiers.COVARIANT;
	    } else if (s.name == MINUS) {
		s.nextToken();
		mods |= Modifiers.CONTRAVARIANT;
	    }
	}
	return typeBounds(s.pos, mods, ident());
    }

    /** TypeBounds ::= [`>:' Type] [`<:' Type]
     */
    Tree typeBounds(int pos, int mods, Name name) {
	Tree lobound;
	Tree hibound;
	if (s.token == SUPERTYPE) {
	    s.nextToken();
	    lobound = type();
	} else {
	    lobound = scalaDot(pos, Names.All.toTypeName());
	}
	if (s.token == SUBTYPE) {
	    s.nextToken();
	    hibound = type();
	} else {
	    hibound = scalaDot(pos, Names.Any.toTypeName());
	}
	return make.TypeDef(pos, mods, name.toTypeName(), hibound, lobound);
    }

//////// DEFS ////////////////////////////////////////////////////////////////

    /** Import  ::= import ImportRef {`,' ImportRef}
     */
    Tree[] importClause() {
        accept(IMPORT);
        TreeList ts = new TreeList();
	ts.append(importRef());
	while (s.token == COMMA) {
	    s.nextToken();
	    ts.append(importRef());
	}
	return ts.toArray();
    }

    /**  ImportRef ::= StableId `.' (Id | `_' | ImportSelectors)
     */
    Tree importRef() {
	Tree t;
	int startpos = s.pos;
	int pos;
	if (s.token == THIS) {
	    t = make.This(s.skipToken(), Tree.Empty);
	    t = make.Select(accept(DOT), t, ident());
	    pos = accept(DOT);
	} else {
	    t = make.Ident(s.pos, ident());
	    pos = accept(DOT);
	    if (s.token == THIS) {
		s.nextToken();
		t = make.This(pos, convertToTypeId(t));
		t = make.Select(accept(DOT), t, ident());
		pos = accept(DOT);
	    }
	}
	while (true) {
	    if (s.token == USCORE) {
		s.nextToken();
		return make.Import(startpos, t, new Name[]{Names.WILDCARD});
	    } else if (s.token == LBRACE) {
		return make.Import(startpos, t, importSelectors());
	    } else {
		Name name = ident();
		if (s.token == DOT) {
		    t = make.Select(pos, t, name);
		    pos = accept(DOT);
		} else {
		    return make.Import(startpos, t, new Name[]{name, name});
		}
	    }
	}
    }

    /** ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     */
    Name[] importSelectors() {
	LinkedList/*<Name>*/ names = new LinkedList();
	accept(LBRACE);
	boolean isLast = importSelector(names);
	while (!isLast && s.token == COMMA) {
	    s.nextToken();
	    isLast = importSelector(names);
	}
	accept(RBRACE);
	return (Name[])names.toArray(new Name[]{});
    }

    /** ImportSelector ::= Id [`=>' [Id | `_']]
     */
    boolean importSelector(LinkedList/*<Name>*/ names) {
	if (s.token == USCORE) {
	    s.nextToken();
	    names.add(Names.WILDCARD);
	    return true;
	} else {
	    Name name = ident();
	    names.add(name);
	    if (s.token == ARROW) {
		s.nextToken();
		if (s.token == USCORE) {
		    s.nextToken();
		    names.add(Names.WILDCARD);
		} else {
		    names.add(ident());
		}
	    } else {
		names.add(name);
	    }
	    return false;
	}
    }

    /** Def    ::= val PatDef {`,' PatDef}
     *           | var VarDef {`,' VarDef}
     *           | def FunDef {`,' FunDef}
     *           | constr ConstrDef {`,' ConstrDef}
     *           | type TypeDef {`,' TypeDef}
     *           | TopDef
     *  Dcl    ::= val ValSig {`,' ValSig}
     *           | var ValSig {`,' ValSig}
     *           | def FunSig {`,' FunSig}
     *           | constr ConstrSig {`,' ConstrSig}
     *           | type TypeDcl {`,' TypeDcl}
     */
    Tree[] defOrDcl(int mods) {
        TreeList ts = new TreeList();
        switch (s.token) {
	case VAL:
	    do {
		s.nextToken();
		ts.append(patDefOrSig(mods));
	    } while (s.token == COMMA);
	    return ts.toArray();
	case VAR:
	    do {
		s.nextToken();
		ts.append(varDefOrSig(mods));
	    } while (s.token == COMMA);
	    return ts.toArray();
	case DEF:
	    do {
		s.nextToken();
		ts.append(funDefOrSig(mods));
	    } while (s.token == COMMA);
	    return ts.toArray();
	case CONSTR:
	    do {
		s.nextToken();
		ts.append(constrDefOrSig(mods));
	    } while (s.token == COMMA);
	    return ts.toArray();
	case TYPE:
	    do {
		s.nextToken();
		ts.append(typeDefOrSig(mods));
	    } while (s.token == COMMA);
	    return ts.toArray();
	default:
	    return topDef(mods);
	}
    }

    /**  TopDef ::= ([case] class | trait) ClassDef {`,' ClassDef}
     *            | [case] object ModuleDef {`,' ModuleDef}
     */
    Tree[] topDef(int mods) {
        TreeList ts = new TreeList();
	switch (s.token) {
	case CLASS:
	case CASECLASS:
	case TRAIT:
	    if (s.token == CASECLASS) mods |= Modifiers.CASE;
	    else if (s.token == TRAIT) mods |= Modifiers.TRAIT | Modifiers.ABSTRACTCLASS;
	    do {
		s.nextToken();
		ts.append(classDef(mods));
	    } while (s.token == COMMA);
	    return ts.toArray();
	case OBJECT:
	case CASEOBJECT:
	    do {
		s.nextToken();
		ts.append(moduleDef(mods));
	    } while (s.token == COMMA);
	    return ts.toArray();
	default:
	    return new Tree[]{syntaxError("illegal start of definition", true)};
        }
    }

    /** PatDef ::= Pattern `=' Expr
     *  ValSig ::= Id `:' Type
     */
    Tree patDefOrSig(int mods) {
        int pos = s.pos;
        Tree pat = pattern();
	Tree tp;
        switch (pat) {
	case Typed(Tree pat1, Tree tp1):
	    pat = pat1;
	    tp = tp1;
	    break;
	default:
	    if (s.token == COLON) tp = typedOpt();
	    else tp = Tree.Empty;
	}
	switch (pat) {
	case Ident(Name name):
	    if (tp == Tree.Empty || s.token == EQUALS)
		return make.ValDef(pos, mods, name, tp, equalsExpr());
	    else
		return make.ValDef(pos, mods | Modifiers.DEFERRED, name, tp, Tree.Empty);
	default:
	    return make.PatDef(pos, mods, pat, equalsExpr());
        }
    }

    /** VarDef ::= Id [`:' Type] `=' Expr
     *           | Id `:' Type `=' `_'
     *  VarSig ::= Id `:' Type
     */
    Tree varDefOrSig(int mods) {
        int pos = s.pos;
        Name name = ident();
        Tree type = typedOpt();
        if (type == Tree.Empty || s.token == EQUALS) {
	    accept(EQUALS);
	    Tree rhs;
	    if (type != Tree.Empty && s.token == USCORE) {
		rhs = Tree.Empty;
		s.nextToken();
	    } else {
		rhs = expr();
	    }
            return make.ValDef(pos, mods | Modifiers.MUTABLE, name, type, rhs);
        } else {
            return make.ValDef(pos, mods | Modifiers.MUTABLE | Modifiers.DEFERRED,
			       name, type, Tree.Empty);
	}
    }

    /** FunDef ::= Id [FunTypeParamClause] {ParamClause} [`:' Type] `=' Expr
     *  FunSig ::= Id [FunTypeParamClause] {ParamClause} `:' Type
     */
    Tree funDefOrSig(int mods) {
        int pos = s.pos;
        Name name = ident();
        TypeDef[] tparams = typeParamClauseOpt(false);
        ValDef[][] vparams = paramClauses();
        Tree restype = typedOpt();
	if (s.token == EQUALS || restype == Tree.Empty)
            return make.DefDef(pos, mods, name, tparams, vparams,
                               restype, equalsExpr());
        else
            return make.DefDef(pos, mods | Modifiers.DEFERRED, name,
                               tparams, vparams, restype, Tree.Empty);
    }

    /*  ConstrDef ::= Id [FunTypeParamClause] [ParamClause] [`:' Type] `=' (Constr | BlockConstr)
     */
    Tree constrDefOrSig(int mods) {
        int pos = s.pos;
        Name name = ident().toConstrName();
        TypeDef[] tparams = typeParamClauseOpt(false);
        ValDef[][] vparams = new ValDef[][]{paramClause()};
        Tree restype = typedOpt();
	if (s.token == EQUALS || restype == Tree.Empty) {
	    accept(EQUALS);
            return make.DefDef(pos, mods, name, tparams, vparams,
                               restype, (s.token == LBRACE) ? blockConstr() : constr());
        } else
            return make.DefDef(pos, mods | Modifiers.DEFERRED, name,
                               tparams, vparams, restype, Tree.Empty);
    }

    /** TypeDef ::= Id `=' Type
     *  TypeDcl ::= Id TypeBounds
     */
    Tree typeDefOrSig(int mods) {
        int pos = s.pos;
        Name name = ident().toTypeName();
	switch (s.token) {
	case EQUALS:
	    s.nextToken();
            return make.TypeDef(pos, mods, name, type(), Tree.Empty);
	case SUPERTYPE:
	case SUBTYPE:
	case SEMI:
	case COMMA:
	case RBRACE:
	    return typeBounds(pos, mods | Modifiers.DEFERRED, name);
	default:
 	    return syntaxError("`=', `>:', or `<:' expected", true);
	}
    }

    /** ClassDef ::= Id [TypeParamClause] [`:' SimpleType] ClassTemplate
     */
    Tree classDef(int mods) {
	int pos = s.pos;
	Name name = ident();
	TypeDef[] tparams = typeParamClauseOpt(true);
	ValDef[][] params = (s.token == LPAREN) ? new ValDef[][]{paramClause()}
	    : Tree.ValDef_EMPTY_ARRAY_ARRAY;
        return make.ClassDef(pos, mods, name.toTypeName(), tparams, params,
			     simpleTypedOpt(), classTemplate());
    }

    /** ModuleDef       ::= Id [`:' SimpleType] ClassTemplate
     */
    Tree moduleDef(int mods) {
        return make.ModuleDef(
	    s.pos, mods, ident(), simpleTypedOpt(), classTemplate());
    }

    /** ClassTemplate ::= [`extends' Constr] {`with' Constr} [TemplateBody]
     */
    Template classTemplate() {
        int pos = s.pos;
	if (s.token == EXTENDS) {
	    s.nextToken();
	    return template();
	} else if (s.token == WITH) {
	    s.nextToken();
	    TreeList parents = new TreeList();
	    parents.append(scalaObjectConstr(pos));
	    return template(parents);
	} else if (s.token == LBRACE) {
	    return (Template)make.Template(
		pos, new Tree[]{scalaObjectConstr(pos)}, templateBody());
	} else {
	    if (!(s.token == SEMI || s.token == COMMA || s.token == RBRACE))
		syntaxError("`extends' or `{' expected", true);
	    return (Template)make.Template(
		pos, new Tree[]{scalaObjectConstr(pos)}, Tree.EMPTY_ARRAY);
	}
    }

////////// TEMPLATES ////////////////////////////////////////////////////////////


    /** Template  ::= Constr {`with' Constr} [TemplateBody]
     */
    Template template() {
	return template(new TreeList());
    }

    Template template(TreeList parents) {
	int pos = s.pos;
	parents.append(constr());
	while (s.token == WITH) {
	    s.nextToken();
	    parents.append(constr());
	}
	Tree[] stats = (s.token == LBRACE) ? templateBody() : Tree.EMPTY_ARRAY;
	return (Template)make.Template(pos, parents.toArray(), stats);
    }

    /** Constr ::= StableId [TypeArgs] [`(' [Exprs] `)']
     */
    Tree constr() {
        Tree t = convertToConstr(stableId());
	if (s.token == LBRACKET)
	    t = make.TypeApply(s.pos, t, typeArgs());
	if (s.token == LPAREN)
	    t = make.Apply(s.pos, t, argumentExprs());
	return applyConstr(t);
    }

    /** TemplateBody ::= `{' [TemplateStat {`;' TemplateStat}] `}'
     */
    Tree[] templateBody() {
	accept(LBRACE);
	Tree[] body = templateStatSeq();
	accept(RBRACE);
	return body;
    }

    /** Refinement ::= `{' [RefineStat {`;' RefineStat}] `}'
     */
    Tree[] refinement() {
	accept(LBRACE);
	Tree[] body = refineStatSeq();
	accept(RBRACE);
	return body;
    }

/////// STATSEQS //////////////////////////////////////////////////////////////

    /** Packaging ::= package QualId `{' TopStatSeq `}'
     */
    Tree packaging() {
	int pos = accept(PACKAGE);
	Tree pkg = qualId();
	accept(LBRACE);
	Tree[] stats = topStatSeq();
	accept(RBRACE);
	return
	    make.PackageDef(pos, pkg, make.Template(pos, Tree.EMPTY_ARRAY, stats));
    }

    /** TopStatSeq ::= [TopStat {`;' TopStat}]
     *  TopStat ::= Modifiers TopDef
     *            | Packaging
     *            | Import
     *            |
     */
    Tree[] topStatSeq() {
        TreeList stats = new TreeList();
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
		stats.append(topDef(modifiers()));
	    } else if (s.token != SEMI) {
		syntaxError("illegal start of class or object definition", true);
	    }
	    if (s.token != RBRACE && s.token != EOF) accept(SEMI);
	}
	return stats.toArray();
    }

    /** TemplateStatSeq  ::= TemplateStat {`;' TemplateStat}
     *  TemplateStat     ::= Import
     *                	   | Modifiers Def
     *	                   | Modifiers Dcl
     *	                   | Expr
     * %                   | val this `:' Type
     *                     |
     */
    Tree[] templateStatSeq() {
        TreeList stats = new TreeList();
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
	return stats.toArray();
    }

    /** RefineStatSeq    ::= RefineStat {`;' RefineStat}
     *  RefineStat       ::= Dcl
     *                     | type TypeDef {`,' TypeDef}
     *                     |
     */
    Tree[] refineStatSeq() {
        TreeList stats = new TreeList();
        while (s.token != RBRACE && s.token != EOF) {
	    if (isDclIntro()) {
		stats.append(defOrDcl(0));
	    } else if (s.token != SEMI) {
		syntaxError("illegal start of declaration", true);
	    }
	    if (s.token != RBRACE) accept(SEMI);
	}
	return stats.toArray();
    }

    /** BlockStatSeq ::= { BlockStat `;' } [Expr]
     *  BlockStat    ::= Import
     *                 | Def
     *                 | LocalClassModifiers TopDef
     *	               | Expr
     *                 |
     */
    Tree[] blockStatSeq(TreeList stats) {
        while ((s.token != RBRACE) && (s.token != EOF) && (s.token != CASE)) {
	    if (s.token == IMPORT) {
		stats.append(importClause());
		accept(SEMI);
	    } else if (isExprIntro()) {
		stats.append(expr());
		if (s.token != RBRACE && s.token != CASE) accept(SEMI);
	    } else if (isDefIntro()) {
		stats.append(defOrDcl(0));
		accept(SEMI);
		if (s.token == RBRACE) {
		    stats.append(make.Block(s.pos, Tree.EMPTY_ARRAY));
		}
	    } else if (isLocalClassModifier()) {
		stats.append(topDef(localClassModifiers()));
		accept(SEMI);
		if (s.token == RBRACE) {
		    stats.append(make.Block(s.pos, Tree.EMPTY_ARRAY));
		}
	    } else if (s.token == SEMI) {
		s.nextToken();
	    } else {
		syntaxError("illegal start of statement", true);
	    }
	}
	return stats.toArray();
    }


    /** CompilationUnit = [package QualId `;'] TopStatSeq
     */
    Tree[] compilationUnit() {
	if (s.token == PACKAGE) {
	    int pos = s.skipToken();
	    Tree pkg = qualId();
	    if (s.token == SEMI) {
		s.nextToken();
		return new Tree[]{
		    make.PackageDef(
			pos, pkg, make.Template(pos, Tree.EMPTY_ARRAY, topStatSeq()))};
	    } else {
		TreeList stats = new TreeList();
		accept(LBRACE);
		stats.append(
		    make.PackageDef(
			pos, pkg, make.Template(pos, Tree.EMPTY_ARRAY, topStatSeq())));
		accept(RBRACE);
 		stats.append(topStatSeq());
		return stats.toArray();
	    }
	} else {
	    return topStatSeq();
	}
    }
}

