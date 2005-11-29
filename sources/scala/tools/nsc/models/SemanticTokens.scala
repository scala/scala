
package scala.tools.nsc.models;

import scala.tools.nsc.{Global => Compiler};
import scala.tools.nsc.symtab.{Flags,Names};
import scala.tools.nsc.util.{NameTransformer,Position,SourceFile};
import scala.collection.mutable.{HashMap,HashSet};



class SemanticTokens(val compiler: Compiler) {
  import compiler._;

  abstract class Kind {}
  object OBJECT  extends Kind;
  object CLASS   extends Kind;
  object   DEF   extends Kind;
  object   VAL   extends Kind;
  object   VAR   extends Kind;
  object   ARG   extends Kind;
  object  TPARAM extends Kind;

  // static constants here.

  abstract class Token {
    def length : Int;

    def prev : HasNext;
    def next : HasPrev;
  }

  [_trait_] abstract class HasNext extends Token {
    var next0 : HasPrev = _;
    def next = next0;
  }
  [_trait_] abstract class HasPrev extends Token {
    var prev0 : HasNext = _;
    def prev = prev0;
  }
  abstract class Actual extends HasNext with HasPrev {
    def convertToGap : Pair[Int,Actual] = {
      val nextGap = next.isInstanceOf[Gap];
      val prevGap = prev.isInstanceOf[Gap];
      if (prevGap) {
	val ret = prev.length;
	val gap = prev.asInstanceOf[Gap];
	gap.setLength(gap.length + length);
	if (nextGap) {
	  gap.setLength(gap.length + next.length);
	  gap.next0 = next.next;
	  next.next.prev0 = gap;
	} else {
	  gap.next0 = next;
	  next.prev0 = gap;
	}
	new Pair(ret,gap);
      } else if (nextGap) {
	val gap = next.asInstanceOf[Gap];
	gap.setLength(gap.length + length);
	gap.prev0 = prev;
	prev.next0 = gap;
	new Pair(0,gap);
      } else {
	prev.next0 = next;
	next.prev0 = prev;
	val gap = new Gap(prev);
	gap.setLength(length);
	new Pair(0,gap);
      }
    }
  }
  final class Gap extends Actual {
    def this(prev1 : HasNext) = {
      this();
      next0 = prev1.next;
      prev0 = prev1;
      prev0.next0 = this;
      next0.prev0 = this;
    }
    override def toString() = "gap-" + length;


    var length0 : Int = -1;
    def length : Int = length0;
    def setLength(length1 : Int) = length0 = length1;

    // already gap
    override def convertToGap : Pair[Int,Actual] = new Pair(0, this);
  }
  val sources = new HashMap[String /* path */, Process];
  val reverse = new HashMap[Symbol, String /* path */];



  class Process(val unit : CompilationUnit) {
    def source = unit.source;

    if (sources.contains(source.path)) {
      for (val sym : Symbol <- sources(source.path).symbols.keys; reverse(sym) == source.path)
	reverse -= sym;
      sources -= source.path;
    }
    sources.update(source.path, this);

    val symbols = new HashMap[Symbol,Info];


    class Info(val symbol : Symbol) {
      var defined : Def = _;
      val uses = new HashSet[Use];
      symbols.update(symbol, this);
    }
    def info(symbol : Symbol) : Info = if (symbols.contains(symbol)) symbols(symbol) else new Info(symbol);

    abstract class Semantic(val symbol : Symbol) extends Actual {
      val name = NameTransformer.decode(symbol.name.toString()).toString().trim();


      def length = name.length();
      def info : Info = if (symbols.contains(symbol)) symbols(symbol) else new Info(symbol);

      def kind = {
	val term0 = symbol;
	if (false) null;
	else if (term0.isVariable)       VAR;
	else if (term0.isValueParameter) ARG;
	else if (term0.isMethod)         DEF;
	else if (term0.isClass)          CLASS;
	else if (term0.isModule)         OBJECT;
	else if (term0.isValue   )       VAL;
	else if (term0.isTypeParameter)  TPARAM;
	else {
	  System.err.println("UNRECOGNIZED SYMBOL: " + term0 + " " + name);
	  null;
	}
      };



    }
    class Def(tree0 : DefTree) extends Semantic(tree0.symbol) {
      if (info.defined != null) throw new Error("old=" + info.defined + " vs. new=" + this);
      info.defined = this;
      reverse.update(symbol, source.path);

      override def toString() = "def-" + name;

      // if (name.equals("x$0")) throw new Error("SYM=" + symbol + " TREE: " + tree0);

    }
    class Use(symbol0 : Symbol) extends Semantic(symbol0) {
      info.uses += this;

      override def toString() = "use-" + name;
    }



    val list = new TokenList;

    build(unit.body);

    // ok start building....
    def build[T <: Tree](trees : List[T]) : Unit = for (val tree : T <- trees) build(tree);

    def build(tree0 : Tree) : Unit = if (tree0.pos != Position.NOPOS) tree0 match {
      case tree : ImplDef     =>
        list.put(tree.namePos(unit.source), new Def(tree));
        tree match {
	  case cdef : ClassDef => build(cdef.tparams);
	  case   _  => ;
	}
	build(tree.impl0.parents);
	build(tree.impl0.body);

      case tree : ValOrDefDef =>
      // System.err.println("VAL: " + tree + " @ " + tree.pos);


	if (tree.name0.toString().equals("<init>")) {
	  //System.err.println("IGNORE: " + tree.name0.toString() + " " + tree.pos);
	} else if (tree.symbol.hasFlag(Flags.ACCESSOR)) {
	  // ignore
	  ;
	} else {
	  val pos = tree.namePos(unit.source);
	  if (pos != Position.NOPOS) {

	    if (!tree.hasFlag(Flags.SYNTHETIC))
	      list.put(pos, new Def(tree));

	    if (tree.isInstanceOf[DefDef]) {
	      val ddef = tree.asInstanceOf[DefDef];
	      build(ddef.tparams);

	      for (val l0 <- ddef.vparamss; val arg <- l0) {

		val pos0 = if (!unit.source.beginsWith(arg.pos, "val ")) arg.pos;
			   else unit.source.skipWhitespace(arg.pos + ("val ").length());
		list.put(pos0, new Def(arg));
		build(arg.tpt0);
	      }
	    }
	    build(tree.tpt0);
	    build(tree.rhs0);
	  }
	}
      case tree : PackageDef =>
        val pos = tree.namePos(unit.source);
        list.put(pos, new Def(tree));
        build(tree.stats);
      case tree : Function   =>
        for (val arg <- tree.vparams) if (arg.pos != Position.NOPOS) {
	  val name = arg.name0.toString().trim();
	  val pos : Int =
	    if (unit.source.beginsWith(arg.pos, "val ")) unit.source.skipWhitespace(arg.pos + ("val ").length());
	    else if (unit.source.content(arg.pos) == ':') {
	      var posx = arg.pos;
	      while (Character.isWhitespace(unit.source.content(posx - 1))) posx = posx - 1;
	      posx - name.length();
	    } else arg.pos;
	  list.put(pos, new Def(arg));
	  build(arg.tpt0);
	}
        build(tree.body);
      case tree : TypeTree =>
        val tree1 = if (tree.original != null) tree.original; else tree;
        build(tree1, tree.tpe);
        def build( tree : Tree, tpe : Type) : Unit = if (tree.pos != Position.NOPOS) tpe match {
	  case tpe0 : TypeRef => tree match {
	    case apt : AppliedTypeTree =>
	      buildSym(tpe0.sym, apt.tpt.pos);
	      buildTs (apt.args, tpe0.args);
	    case ident : Ident => buildSym(tpe0.sym, ident.pos);
	    case select : Select =>
	      val pos = selectPos(select);
	      buildSym(tpe0.sym, pos);
	    case tpt : TypeTree =>
	      // System.err.println("UNKNOWN TPT: " + tree + " vs. " + tpe0 + " " + tpt.original + " " + unit.source.content(tree.pos));
	    case _ => System.err.println("UNKNOWN TPE: " + tree + " vs. " + tpe0 + " " + tree.getClass() + " " + unit.source.content(tree.pos));
	  }
	  case tpe0 : MethodType => tree match {
	    case tpt: TypeTree =>
	      if (tpt.original != null) build(tpt.original, tpe);
	      else {
		System.err.println("UNKNOWN TPT: " + tree + " vs. " + tpe0 + " " + unit.source.content(tree.pos));
	      }
	    case ident  : Ident  => build(ident,  tpe0.resultType);
	    case select : Select => build(select, tpe0.resultType);
	    case _ => System.err.println("UNKNOWN TPE: " + tree + " vs. " + tpe0 + " " + tree.getClass());
	  }
	  case _ => System.err.println("UNKNOWN: " + tree + " " + tree.getClass() + " vs. " + tpe + " " + tpe.getClass() + " " + tree.pos);
	};
        def buildTs(trees : List[Tree], types : List[Type]): Unit = if (!trees.isEmpty || !types.isEmpty) {
	  build  (trees.head, types.head);
	  buildTs(trees.tail, types.tail);
	};
      case tree : AbsTypeDef =>
        list.put(tree.namePos, new Def(tree));
      case tree : Bind =>
        list.put(tree.pos, new Def(tree));
        build(tree.body);
      case tree : Ident      => buildSym(tree.symbol, tree.pos);
      case tree : Select     =>
        build(tree.qualifier);
        buildSym(tree.symbol, selectPos(tree));
      case tree : GenericApply =>
      // System.err.println("APPLY: " + tree.fun0 + " " + tree.args0);
        build(tree.fun0); build(tree.args0);
      case tree : Typed        => build(tree.expr); build(tree.tpt);
      case tree : Import     => build(tree.expr);
      case tree : Block      => build(tree.stats); build(tree.expr);
      case tree : CaseDef    =>
        build(tree.pat); build(tree.guard); build(tree.body);
      case tree : Sequence   => build(tree.trees);
      case tree : Assign     => build(tree.lhs); build(tree.rhs);
      case tree : If         => build(tree.cond); build(tree.thenp); build(tree.elsep);
      case tree : Match      => build(tree.selector); build(tree.cases);
      case tree : Return     => build(tree.expr);
      case tree : Literal => ;
      case tree : This    => ;
      case _ => ;
        if (tree0 != EmptyTree)
	  System.err.println("BAIL: " + tree0.pos + " " + tree0 + " " + tree0.getClass());
    }
    def buildSym(term : Symbol, pos : Int) : Unit = {
      val term0 = if (term.hasFlag(Flags.ACCESSOR)) term.accessed; else term;
      val name = NameTransformer.decode(term0.name.toString()).toString().trim();
      val buf = unit.source.content;

      val cs = name.toChars;
      var idx = 0;
      if (cs.length + pos > buf.length) {
	return;
      }
      else while (idx < cs.length)
	if (buf(pos + idx) != cs(idx)) {
	  // System.err.println("BAD SYM: " + term + " " + term.getClass());
	  return;
	}
        else idx = idx + 1;

      list.put(pos, new Use(term0));
    }
    def selectPos(tree : Select): Int = if (tree.pos == Position.NOPOS) Position.NOPOS else {
      val buf = unit.source.content;
      val pos =
	if (buf(tree.pos) != '.') tree.pos;
	else {
	  def f(x : Int) : Int = {
	    if (Character.isWhitespace(buf(x))) f(x + 1);
	    else x;
	  }
	  f(tree.pos + 1);
	}
      pos;
    };
    class TokenList {
      object begin extends HasNext {
	def prev = this;
	def length = 0;
      }
      object end extends HasPrev {
	def next = this;
	def length = 0;
      }
      // initialize
      begin.next0 = end;
      end.prev0 = begin;

      private var length : Int = 0;

      def tokenAt(offset : Int) = if (offset >= length) null else {
	//System.err.println("CURSOR-0: " + cursor.offset);
	cursor.seek(offset);
	//System.err.println("CURSOR-1: " + cursor.offset + " " + cursor.token + " " + offset);

	if (cursor.token.isInstanceOf[Semantic]) cursor.token.asInstanceOf[Semantic];
	else null;
      };
      def put(offset : Int, tok : Semantic) = {
	grow(offset);
	if (offset == length) append(tok);
	else {
	  grow(offset + tok.length);
	  cursor.seek(offset);
	  assert(cursor.token.isInstanceOf[Gap]);
	  val gap = cursor.token.asInstanceOf[Gap];
	  if (!(offset - cursor.offset + tok.length <= gap.length)) {
	    System.err.println("LIST  =" + this);
	    System.err.println("OFFSET=" + offset + " " + tok + " " + tok.length);
	    System.err.println("       " + cursor.offset + " " + gap.length);
	    throw new Error();
	  }
	  if (offset == cursor.offset) {
	    // replace or prepend
	    tok.prev0 = gap.prev0;
	    if (tok.length == gap.length) { // replace gap
	      tok.next0 = gap.next0;
	    } else {
	      gap.setLength(gap.length - tok.length);
	      tok.next0 = gap;
	    }
	    tok.next0.prev0 = tok;
	    tok.prev0.next0 = tok;
	    cursor.token = tok;
	  } else {
	    // append
	    val diff = (cursor.offset + gap.length) - (offset + tok.length);

	    gap.setLength(gap.length - tok.length - diff);
	    tok.prev0 = gap;
	    tok.next0 = gap.next;
	    tok.next0.prev0 = tok;
	    tok.prev0.next0 = tok;
	    if (diff != 0) {
	      val gap0 = new Gap(tok);
	      gap0.setLength(diff);
	    }
	  }
	}
	// System.err.println("PUT: " + this);
      }
      override def toString() = {
	var node = begin.next;
	var str = "";
	while (node != end) {
	  str = str + " " + node;
	  node = node.next;
	}
	str;
      };
      def grow(length0 : Int) = if (length0 > length) {
	if (false) {
	  System.err.println("GROW: " + length + " -> " + length0);
	  System.err.println("      " + this);
	}

	assert(length < length0);

	if (end.prev.isInstanceOf[Gap]) {
	  val gap = end.prev.asInstanceOf[Gap];
	  gap.setLength(gap.length + (length0 - length));
	} else {
	  val gap = new Gap;
	  gap.setLength(length0 - length);
	  gap.prev0 = end.prev;
	  gap.next0 = end;
	  gap.prev0.next0 = gap;
	  end.prev0 = gap;
	}
	if (cursor.token == end) {
	  assert(cursor.offset == length);
	  cursor.offset = length0;
	}
	length = length0;
      }
      def append(tok : Semantic) : Unit = {
	tok.prev0 = end.prev;
	tok.next0 = end;
	tok.prev0.next0 = tok;
	end.prev0 = tok;
	length = length + tok.length;
	if (cursor.token == end) cursor.offset = length;
      }
      private object cursor {
	var token  : Token = end;
	var offset : Int   = 0;

	def next : Unit = if (token == end) end else {
	  offset = offset + token.length;
	  token  = token.next;
	}
	def prev : Unit = if (token.prev == begin) token else {
	  offset = offset - token.prev.length;
	  token = token.prev;
	}
	def seek(soffset : Int) : Unit = if (soffset == 0) {
	  token = begin.next;
	  offset = 0;
	} else if (soffset == length) {
	  token = end;
	  offset = length;
	} else {
	  assert(soffset > 0);
	  assert(soffset < length);
	  while (offset                >  soffset) prev;
	  while (offset + token.length <= soffset) next;
	}
	def convertToGap = if (token.isInstanceOf[Actual]) {
	  val ret = token.asInstanceOf[Actual].convertToGap;
	  offset  = offset - ret._1;
	  token   = ret._2;
	}
      }
      // add or delete characters
      def adjust(offset: Int, /* where */
		 length: Int, /* how many characters are modified */
		 to    : Int  /* length of new string */) = {
       cursor.seek(offset);
       cursor.convertToGap;
       while (cursor.offset + cursor.token.length < offset + length && cursor.token.next != end) {
	 val save = cursor.offset;
	 cursor.next;
	 cursor.convertToGap;
	 assert(cursor.offset == save);
       }
       if (length != to) {
	 val diff = to - length;
	 val gap = cursor.token.asInstanceOf[Gap];
	 gap.setLength(gap.length + diff);
       };
     }
    };
  }
}

