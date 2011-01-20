/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package models

import java.lang.Character.isJavaIdentifierPart
import java.lang.Thread

import scala.collection.mutable.{HashMap, HashSet}
import scala.tools.nsc.Global
import scala.tools.nsc.symtab.{Flags, Names}
import scala.tools.nsc.symtab.Flags.DEFERRED
import scala.tools.nsc.util.{BatchSourceFile, SourceFile}
import scala.reflect.NameTransformer

class SemanticTokens(val compiler: Global) {
  import compiler._
  object walker extends symtab.SymbolWalker {
    lazy val global : compiler.type = compiler
  }

  abstract class Kind {}
  object OBJECT extends Kind
  object CLASS  extends Kind
  object TRAIT  extends Kind
  object DEF    extends Kind
  object VAL    extends Kind
  object VAR    extends Kind
  object ARG    extends Kind
  object TPARAM extends Kind

  type AnyClass = Class[_]

  // static constants here

  abstract class Token {
    def length: Int
    def prev: HasNext
    def next: HasPrev
  }

  def eatKeyword(source: BatchSourceFile, pos: Int, keywords: List[String]) : Int = {
    if (keywords.isEmpty)
      pos
    else if (pos == source.length)
      -1
    else if (source.beginsWith(pos, " "))
      eatKeywords(source, pos + 1)
    else if (source.beginsWith(pos, keywords.head + " "))
      eatKeywords(source, pos + keywords.head.length + 1)
    else
      eatKeyword(source, pos, keywords.tail)
  }

  def eatKeywords(source: BatchSourceFile, pos: Int): Int = {
    val keywords =
      "package" :: "val" :: "var" :: "def" :: "class" :: "trait" :: "override" :: "case" ::
      "object" :: "sealed" :: "private" :: "protected" :: Nil
    if (pos != -1) eatKeyword(source, pos, keywords)
    else pos
  }

  trait HasNext extends Token {
    var next0: HasPrev = _
    def next = next0
  }

  trait HasPrev extends Token {
    var prev0: HasNext = _
    def prev = prev0
  }

  abstract class Actual extends HasNext with HasPrev {
    def convertToGap: (Int, Actual) = {
      val nextGap = next.isInstanceOf[Gap]
      val prevGap = prev.isInstanceOf[Gap]

      if (prevGap) {
        val ret = prev.length
        val gap = prev.asInstanceOf[Gap]
        gap.setLength(gap.length + length)
        if (nextGap) {
          gap.setLength(gap.length + next.length)
          gap.next0 = next.next
          next.next.prev0 = gap
        } else {
          gap.next0 = next
          next.prev0 = gap
        }
        (ret, gap)
      }
      else if (nextGap) {
        val gap = next.asInstanceOf[Gap]
        gap.setLength(gap.length + length)
        gap.prev0 = prev
        prev.next0 = gap
        (0, gap)
      }
      else {
        prev.next0 = next
        next.prev0 = prev
        val gap = new Gap(prev)
        gap.setLength(length)
        (0, gap)
      }

    }
    def insert(prev1: HasNext) {
      next0 = prev1.next
      prev0 = prev1
      prev0.next0 = this
      next0.prev0 = this
    }

  } // Actual

  final class Gap extends Actual {
    def this(prev1: HasNext) = {
      this()
      insert(prev1)
    }
    override def toString() = "gap-" + length

    var length0: Int = -1
    def length: Int = length0
    def setLength(length1: Int) = length0 = length1

    // already gap
    override def convertToGap: (Int, Actual) = (0, this)
  }

  def Process(unit: CompilationUnit) = new Process(unit)
  class Process(val unit: CompilationUnit) {
    private var doLog = true
    def source = unit.source

    def dbg(tree: Tree) = {
      def treePos: Position = if (tree ne null) tree.pos else NoPosition;
      (
        "TREE=" + tree +
          (if (tree ne null) (" CLASS=" + tree.getClass()) else "") +
            " SYM=" + tree.symbol +
              " POS=" +
                treePos.dbgString
    )}

    val symbols = new HashMap[Symbol, Info]

    class Info(val symbol: Symbol) {
      var defined : Def = _
      val uses = new HashSet[Use]
      symbols.update(symbol, this)
    }

    def info(symbol: Symbol): Info =
      if (symbols.contains(symbol)) symbols(symbol)
      else new Info(symbol)

    abstract class Semantic(val symbol: Symbol) extends Actual {
      val name = NameTransformer.decode(symbol.name.toString).trim()
      assert(symbol != NoSymbol)
      def myOuter = Process.this

      def tpe: Type = symbol.tpe

      def length = name.length()
      def info: Info = if (symbols.contains(symbol)) symbols(symbol) else new Info(symbol)

      def kind = {
        val term0 = symbol
        if (false) null
        else if (term0.isVariable)       VAR
        else if (term0.isValueParameter) ARG
        else if (term0.isMethod)         DEF
        else if (term0.isClass)          CLASS
        else if (term0.isModule)         OBJECT
        else if (term0.isValue)          VAL
        else if (term0.isTypeParameter)  TPARAM
        else if (term0.isType         )  TPARAM
        else {
          // Console.err.println("UNRECOGNIZED SYMBOL: " + term0 + " " + name);
          null
        }
      }
    }

    class Def(symbol0: Symbol) extends Semantic(symbol0) {
      info.defined = this
      override def toString() = "def-" + name + "-" + symbol.getClass()
    }
    class Use(symbol0: Symbol, tpe0: Type) extends Semantic(symbol0) {
      info.uses += this

      override def tpe : Type = if (tpe0 ne null) tpe0 else super.tpe;
      override def toString() = "use-" + name + "-" + symbol.getClass();
    }
    val list = new TokenList

    //build(unit.body)
    val map = new scala.collection.mutable.LinkedHashMap[Int,Symbol]
    map.clear // populate the map.
      class visitor extends walker.Visitor {
      def contains(pos : Position) = map.contains(pos.point)
      def apply(pos : Position) = map(pos.point)
      def update(pos : Position, sym : Symbol) : Unit = if (pos.isDefined) {
        val offset = pos.point
        map(offset) = sym
        val isDef = pos.point == sym.pos.point
        list.put(offset, (if (isDef) new Def(sym) else new Use(sym, NoType)));
      }
    }
    walker.walk(unit.body, new visitor)(offset => unit.source.identifier(offset, compiler))


    // ok start building....
    def build[T <: Tree](trees: List[T]) {
      for (tree <- trees) build(tree)
    }

    def build(tree0: Tree): Unit = try {
      /* if (tree0.pos != NoPosition) */ tree0 match {
      case tree: ImplDef =>
        val pos = eatKeywords(unit.source.asInstanceOf[BatchSourceFile], tree.pos.point)
        if (pos == -1) {

        } else buildDef(tree.symbol, eatKeywords(unit.source.asInstanceOf[BatchSourceFile], tree.pos.point));
        tree match {
          case cdef: ClassDef => build(cdef.tparams)
          case _ => ;
        }
        build(tree.impl.parents)
        build(tree.impl.body)
      case tree: ValOrDefDef =>
        if (!tree.symbol.hasAccessorFlag || tree.symbol.isDeferred) {
          // MO: I added !tree.symbol.hasFlag(DEFERRED) in a refactoring where
          // getters now can be abstract whereas before they could not.
          // Adding the condition thus keeps the old behavior.
          // todo: review whether this is correct, or whether abstract getters should be included.
          {
            val pos : Int = if (tree.name.toString().equals("<init>")) -1 else
              eatKeywords(unit.source.asInstanceOf[BatchSourceFile], tree.pos.point);
          if (false) Console.err.println("VALDEF: tree=" + tree + " sym=" + tree.symbol + " pos0=" +
            tree.symbol.pos + " alias=" + tree.symbol.alias + " pos1=" +
            pos + " pos2=" + tree.pos.dbgString + " " + tree.symbol.isSynthetic);

          if (pos != -1 && !tree.isSynthetic)
            buildDef(tree.symbol, pos);
              }

              if (tree.isInstanceOf[DefDef]) {
                val ddef = tree.asInstanceOf[DefDef];
                build(ddef.tparams);

                for (l0 <- ddef.vparamss; arg <- l0) {
                  val pos0 : Int = if (!unit.source.beginsWith(arg.pos.point, "val ")) arg.pos.point;
                                             else unit.source.skipWhitespace(arg.pos.point + ("val ").length());
                  buildDef(arg.symbol, pos0);
                  build(arg.tpt);
                }
              }
          //TPT=scala.Iterator[DocGenerator.this.compiler0.CompilationUnit] 260 class scala.tools.nsc.ast.Trees$TypeTree scala.Iterator[DocGenerator.this.compiler0.CompilationUnit] class scala.tools.nsc.symtab.Types$$anon$5
          if ((tree.tpt eq null) || (tree.tpt.tpe eq null)) {
            //Console.err.println("BAD: " + tree.tpt + " in " + tree);
          } else {
            //Console.err.println("TPT=" + tree.tpt + " " + tree.tpt.pos + " " + tree.tpt.getClass() + " " + tree.tpt.tpe + " " + tree.tpt.tpe.getClass() + " " + tree.tpt.tpe.getClass().getSuperclass());
                  build(tree.tpt);
          }
        //Console.err.println("RHS: " + tree.rhs + " " + tree.rhs.getClass() + " " + tree.rhs.getClass().getSuperclass());
              build(tree.rhs);
            }
      case tree: PackageDef =>
        //Console.err.println("PACKAGE: " + tree.name);
        if (false) {
          val pos = eatKeywords(unit.source.asInstanceOf[BatchSourceFile], tree.pos.pointOrElse(-1))
          if (pos != -1)
            buildDef(tree.symbol, pos)
        }
        build(tree.stats)
      case tree: Function =>
        for (arg <- tree.vparams if arg.pos != NoPosition) {
          val name = arg.name.toString().trim()
          val pos: Int =
            if (unit.source.beginsWith(arg.pos.pointOrElse(-1), "val "))
              unit.source.skipWhitespace(arg.pos.pointOrElse(-1) + ("val ").length())
            else if (unit.source.asInstanceOf[BatchSourceFile].content(arg.pos.point) == ':') {
              var posx : Int = arg.pos.point
              while (unit.source.asInstanceOf[BatchSourceFile].content(posx - 1).isWhitespace) posx = posx - 1
              posx - name.length()
            } else arg.pos.point
          buildDef(arg.symbol, pos)
          build(arg.tpt)
        }
        build(tree.body)
      case tree : TypeTree =>
        val treex = tree
        val tree1 = if (tree.original ne null) tree.original else tree
        def classes(clazz: AnyClass): List[AnyClass] =
          if (clazz eq null) Nil
          else clazz :: classes(clazz.getSuperclass())
        if (tree.original eq null) {
          if (false) Console.err.println("NO_ORIGINAL: " + tree + " " + tree.tpe + " " + classes(tree.tpe.getClass()));
        }
        if (tree.tpe ne null) buildT(tree1, tree.tpe);
        def buildT( tree : Tree, tpe : Type) : Unit = if (tree.pos != NoPosition) tpe match {
          case tpe0 : TypeRef => tree match {
            case apt : AppliedTypeTree =>
              buildUse(tpe.typeSymbol, apt.tpt.pos.pointOrElse(-1), tpe0);
          //Console.err.println("APT: " + treex + " vs. " + treex.original);
          //Console.err.println("APT: " + treex.pos + " vs. " + treex.original.pos + " " + unit.source.dbg(treex.original.pos));
              //Console.err.println("APT: " + apt.tpt + " sym0=" + apt.tpt.symbol + " sym1=" + tpe0.sym + " apt.args=" + apt.args + " tpe0.args=" + tpe0.args);

              buildTs (apt.args, tpe0.args);
            case ident : Ident => buildUse(tpe0.sym, ident.pos.pointOrElse(-1), tpe0);
            case select : Select =>
          if (select.symbol == NoSymbol)
              try {
                // build(select);
            buildUse(tpe0.typeSymbol, selectPos(select), tpe0);
            //Console.err.println("QUALIFIER: " + select.qualifier + " " + unit.source.dbg(select.qualifier.pos) + " " + tpe0.prefix + " " + tpe0.prefix.getClass() + " " + tpe0.prefix.getClass().getSuperclass() +" " + tpe0.prefix.widen + " " + tpe0.prefix.toLongString);
                        buildT(select.qualifier, tpe0.prefix);
          } catch {
                        case e : Error =>
                          Console.err.println("BUILD_SELECT: " + select + " @ " + tpe0 + " " + (select.pos).dbgString);
                          throw e;
              }
            case tpt : TypeTree =>
          if (tpt.symbol ne null) {
            Console.err.println("SYM0 " + tpt.symbol + " " + (tpt.pos).dbgString);
            buildUse(tpt.symbol, tpt.pos.pointOrElse(-1), tpe0);
          } else if (tpe0.typeSymbol ne null) {
            //Console.err.println("TYPE_SYM1 " + tpe0.symbol + " " + unit.source.dbg(tpt.pos));
            buildUse(tpe0.typeSymbol, tpt.pos.pointOrElse(-1), tpe0);
          } else {
            Console.err.println("UNKNOWN TPT0: " + (tpt.pos).dbgString + " tpt=" + tpt + " " + tpt.symbol + " tpe0="+ tpe0 + " " + tpe0.typeSymbol + " tpe0.args=" + tpe0.args);
          }
            case sft : SelectFromTypeTree =>
              build(sft.qualifier); // XXX: broken
                if (false) Console.err.println("SFTT: " + sft + " sym=" + sft.symbol + " name=" + sft.name + " qual=" + sft.qualifier + " qual.sym=" +
                          sft.qualifier.symbol +
                          " qual.pos=" + (sft.qualifier.pos).dbgString + " symbol=" + sft.symbol + " type=" + tpe0 +
                          " type.sym=" + tpe0.typeSymbol);
            case _ => Console.err.println("UNKNOWN TPT2: " + tree + " vs. " + tpe0 + " " + tree.getClass() + " " + (tree.pos).dbgString);
          }
          case tpe0 : MethodType => tree match {
            case tpt: TypeTree =>
              if (tpt.original ne null) buildT(tpt.original, tpe);
              else {
                        Console.err.println("UNKNOWN TPT3: " + tree + " vs. " + tpe0 + " " + (tree.pos).dbgString);
              }
            case ident  : Ident  => buildT(ident,  tpe0.resultType);
            case select : Select => buildT(select, tpe0.resultType);
            case _ => Console.err.println("UNKNOWN TPE: " + tree + " vs. " + tpe0 + " " + tree.getClass());
          }
          case tpe0 : RefinedType => tree match {
              case cpt : CompoundTypeTree =>
                  buildTs(cpt.templ.parents, tpe0.parents);

              case _ : TypeTree =>
                // Console.err.println("UNKNOWN TPE13: " + dbg(tree) + " tpe0=" + tpe0 + " " + tpe0.parents);
              case _ =>
                  if (false) Console.err.println("UNKNOWN TPE5: " + dbg(tree) + " tpe0=" + tpe0 + " " + tpe0.parents);
          }
          case tpe0 : ThisType => tree match {
            case stt : SingletonTypeTree => stt.ref match {
                case ths : This => build(ths);

          case _ => Console.err.println("UNKNOWN TPE11: " + tpe0 + " " + stt + " " + stt.ref + " " + stt.ref.getClass() + " " + (tree.pos).dbgString);
        }
        case tt : This =>
        case _ : Ident =>
        case _ : Select =>
        case tt : TypeTree =>
          if (false) Console.err.println("UNKNOWN TPE12: " + tpe0 + " " + tree + " " + tree.getClass() + " " + (tree.pos).dbgString);
        case _ =>
          if (false) Console.err.println("UNKNOWN TPE10: " + tpe0 + " " + tree + " " + tree.getClass() + " " + (tree.pos).dbgString);
          }
        case tpe0 : SingleType => tree match {
          case ident  : Ident  => buildUse(tpe0.sym, ident.pos.pointOrElse(-1), tpe0);
          case select : Select =>
            buildUse(tpe0.termSymbol, selectPos(select), tpe0);
            //Console.err.println("QUALIFIER-0: " + select.qualifier + " " + unit.source.dbg(select.qualifier.pos) + " " + tpe0.prefix + " " + tpe0.prefix.getClass() + " " + tpe0.prefix.getClass().getSuperclass() +" " + tpe0.prefix.widen + " " + tpe0.prefix.toLongString);
            buildT(select.qualifier, tpe0.prefix);

          case _ =>
            if (false) Console.err.println("UNKNOWN TPE8: " + tree + " " + (tree.pos).dbgString + " TPE=" + tpe0 + " PRE=" + tpe0.pre + " SYM=" + tpe0.sym);

        }
      case ctype : ConstantType =>
          case ErrorType =>
          case _ => {
        if (false) Console.err.println("UNKNOWN TPE4: " + tree + " " + tpe + " " + tpe.getClass() + " " + (tree.pos).dbgString);
          }
        };
    def buildTs(trees : List[Tree], types : List[Type]): Unit = if (!trees.isEmpty && !types.isEmpty) {
              buildT (trees.head, types.head);
              buildTs(trees.tail, types.tail);
        } else if (trees.isEmpty != types.isEmpty) {
             if (false && doLog) {
        Console.println("" + treex + " vs. " + treex.original);
        if (treex.original ne null)
          Console.println("" + treex.tpe + " vs. " + treex.original.tpe);
               logError("Tree vs. Type mismatch: " + trees + " " + types + " " + (tree.pos).dbgString, null);
        doLog = false;
      }
    };
      case tree: Bind =>
        buildDef(tree.symbol, tree.pos.pointOrElse(-1))
        build(tree.body)
      case tree: Ident =>
        buildUse(tree.symbol, tree.pos.pointOrElse(-1), tree.tpe)
      case tree: Select =>
        try {
          build(tree.qualifier)
            } catch {
              case e : Error => Console.err.println("SELECTQ: " + tree + " " + tree.qualifier + " " + (tree.qualifier.pos).dbgString); throw e;
            }
            try {
              if (tree.pos.isDefined && tree.pos.point >= unit.source.length) {
                if (false) Console.err.println("BAD_SELECT_QUALIFIER " + tree + " @ " + (tree.pos).dbgString);

        } else {
          //Console.err.println("SELECT-0: " + tree.symbol + " " + tree.pos.dbgString + " " + (tree.pos - selectPos(tree)));
                    buildUse(tree.symbol, selectPos(tree), tree.tpe);
              }
            } catch {
              case e : Error => Console.err.println("SELECTU: " + tree + " " + tree.symbol + " " + tree.pos.dbgString); throw e;
            }
    case tree: TypeApply =>
      //Console.err.println("TYPE_APPLY: " + tree + " " + tree.pos.dbgString);
      if (!tree.args.isEmpty) {
        //Console.err.println("ARGS: " + unit.source.dbg(tree.args0.head.pos));
      }
      build(tree.fun)
      build(tree.args)
    case tree: Apply =>

      build(tree.fun)
      build(tree.args)
      case tree: GenericApply =>

        build(tree.fun)
        build(tree.args)
      case tree: Typed =>
        build(tree.expr)
        build(tree.tpt)
      case tree: Block =>
        if (false) {
          if (!tree.stats.isEmpty)
            Console.err.println("BLOCKS: " + tree.stats.head + " " + tree.stats.head.getClass());
          Console.err.println("BLOCKE: " + tree.expr + " " + tree.expr.getClass())
        }
        build(tree.stats)
        build(tree.expr)
      case tree: CaseDef =>
        build(tree.pat)
        build(tree.guard)
        build(tree.body)
      case tree : Assign     => build(tree.lhs); build(tree.rhs);
      case tree : If         => build(tree.cond); build(tree.thenp); build(tree.elsep);
      case tree : New        =>
        //Console.err.println("NEW: " + tree.tpt + " " + tree.tpt.getClass());
      build(tree.tpt);
      case tree : Match      => build(tree.selector); build(tree.cases);
      case tree : Return     => build(tree.expr);
      case tree : LabelDef   => build(tree.rhs);
      case tree : Throw      => build(tree.expr);
      case tree : Try        => build(tree.block); build(tree.catches); build(tree.finalizer);
      case tree : Alternative => build(tree.trees);
      case tree : This    =>

        if (tree.symbol ne null) buildUse(tree.symbol, tree.pos.pointOrElse(-1), tree.tpe);
        //Thread.dumpStack();
      case tree : TypeDef =>
        //Console.err.println("ALIAS: " + tree);
        build(tree.rhs); build(tree.tparams); buildDef(tree.symbol, tree.pos.pointOrElse(-1));
      case tree : DocDef     => build(tree.definition);
      case tree: Import => build(tree.expr)
      case tree: AppliedTypeTree => ;
      case tree: Annotated => ;
      case tree: SingletonTypeTree => ;
      case tree: Super   => ;
      case tree: Literal => ;
      case EmptyTree => ;
      case _ => ;
        Console.err.println("BAIL: " + (tree0.pos) + " " + tree0 + " " + tree0.getClass());
    }
  } catch {
    case t: Throwable =>
      logError("Error occured at " + (tree0.pos), t)
  }

  def buildUse(term: Symbol, pos: Int, tpe: Type) = buildSym(term, pos, false, tpe)
  def buildDef(term: Symbol, pos: Int) = buildSym(term, pos, true, null)

  def buildSym(term: Symbol, pos: Int, isDef: Boolean, tpe: Type): Unit =
    if (term.hasAccessorFlag)
      buildSym(analyzer.underlying(term), pos, isDef, tpe)
    else if (pos == -1) {
      //Console.err.println("NOPOS: " + term)
      //Thread.dumpStack()
    }
    else if (term != NoSymbol) {
      val name = NameTransformer.decode(term.name.toString).trim()
      val buf = unit.source.asInstanceOf[BatchSourceFile].content
      val cs = name.toChars
      var idx = 0
      if (cs.length + pos > buf.length) return
      else while (idx < cs.length) {
        if (buf(pos + idx) != cs(idx)) {
          //Console.err.println("MISMATCH: " + name + "[" + idx + "] " + unit.source.dbg(pos));
          //Thread.dumpStack();
          return;
        }
        else idx = idx + 1;
      }
      if (cs.length + pos + 1 < buf.length) {
        if (isJavaIdentifierPart(buf(pos + cs.length))) {
          //Console.err.println("MISMATCH: " + name + "[last] " + unit.source.dbg(pos));
          return;
        }
      }
      try {
        list.put(pos, (if (isDef) new Def(term) else new Use(term, tpe)));
      } catch {
        case e : Error => e.printStackTrace();
      }
    }

  def selectPos(tree: Select): Int = if (tree.pos == NoPosition) -1 else {
    val buf = unit.source.asInstanceOf[BatchSourceFile].content
    if (tree.pos.point >= buf.length) {
      if (false) {
        Console.err.println("" + tree + "@" + tree.pos + " not in " +
                           unit.source.file.name + "[" + buf.length + "]");
        Thread.dumpStack()
        abort()
      }
      return 0
    }

    val pos : Int =
      if (buf(tree.pos.point) != '.') tree.pos.point
      else {
        def f(x : Int) : Int = {
          if (buf(x).isWhitespace) f(x + 1)
          else x
        }
        f(tree.pos.point + 1)
      }
    pos
  };

  class TokenList {
    object begin extends HasNext {
      def prev = this
      def length = 0
    }
    object end extends HasPrev {
      def next = this
      def length = 0
    }
    // initialize
    begin.next0 = end
    end.prev0 = begin

    def tokenAt(offset: Int) = {
      cursor.seek(offset)
      if (cursor.token.isInstanceOf[Semantic]) cursor.token.asInstanceOf[Semantic]
      else null
    }

    def put(offset: Int, tok: Actual): Unit = tok match {
      case tok0: Semantic => put(offset, tok0)
      case gap: Gap      =>
    }

    def put(offset: Int, tok: Semantic) {
      cursor.seek(offset);
      if (cursor.token == end) {
        assert(offset >= cursor.offset);
        if (offset > cursor.offset) {
          // add a gap.
          val gap = new Gap(end.prev);
          gap.setLength(offset - cursor.offset);
          cursor.offset = offset;
        }
        // append.
        tok.insert(end.prev);
        cursor.offset = cursor.offset + tok.length;
      } else if (!cursor.token.isInstanceOf[Gap]) {
        val sem = cursor.token.asInstanceOf[Semantic];
        if (sem.symbol == tok.symbol) return;
        if (sem.symbol != tok.symbol &&
            sem.symbol.getClass() == tok.symbol.getClass() &&
            sem.symbol.pos == tok.symbol.pos) return;
      } else {
        val gap = cursor.token.asInstanceOf[Gap];
        if (!(offset - cursor.offset + tok.length <= gap.length)) {
          Console.err.println("LIST  =" + this);
          Console.err.println("OFFSET=" + offset + " " + tok + " " + tok.length);
          Console.err.println("       " + cursor.offset + " " + gap.length);
          gap.length0 = offset - cursor.offset + tok.length
          //abort();
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
    }

    override def toString(): String = {
      var node = begin.next
      var str = ""
      while (node != end) {
        str = str + " " + node
        node = node.next
      }
      str
    }

    object cursor {
      var token: Token = end
      var offset: Int = 0

      def next: Unit = if (token == end) end else {
        offset = offset + token.length
        token  = token.next
      }
      def prev: Unit = if (token.prev == begin) token else {
        offset = offset - token.prev.length
        token = token.prev
      }
      def seek(soffset: Int): Unit = if (soffset == 0) {
        token = begin.next
        offset = 0
      } else {
        assert(soffset > 0)
        while (offset                >  soffset) prev;
        while (offset + token.length <= soffset && token != end) {
          val len0 = offset;
          next;
        }
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
      cursor.seek(offset)
      if (cursor.token != end) {
        cursor.convertToGap
        while (cursor.offset + cursor.token.length < offset + length && cursor.token.next != end) {
          val save = cursor.offset
          cursor.next
          cursor.convertToGap
          assert(cursor.offset == save)
        }
         if (length != to && cursor.token != end) {
           val diff = to - length;
           val gap = cursor.token.asInstanceOf[Gap];
           gap.setLength(gap.length + diff);
         };
      }
    }

  } // TokenList

  }
}

