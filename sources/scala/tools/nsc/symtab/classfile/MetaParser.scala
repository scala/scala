/** $Id: MetaParser.scala
*/
package scala.tools.nsc.symtab.classfile;

import java.util.{StringTokenizer, NoSuchElementException}
import scala.collection.mutable.ListBuffer;
import scala.tools.util.Position;

abstract class MetaParser{

  val global: Global;
  import global._;

  private var scanner: StringTokenizer = _;
  private var owner: Symbol = _;
  private var ownertype: Type = _;
  private var token: String = _;
  private var locals: Scope = null;

  def parse(meta: String, sym: Symbol, symtype: Type): unit = {
    //System.out.println("parse meta for " + sym + ":" + meta + ", locals = " + locals);//DEBUG
    this.scanner = new StringTokenizer(meta, "()[], \t<;", true);
    this.owner = sym;
    this.ownertype = symtype;
    nextToken();
    if (token == "class") parseClass()
    else if (token == "method") parseMethod()
    else if (token == "field") parseField()
    else if (token == "constr") parseConstr()
    else owner.setInfo(symtype);
  }

  protected def nextToken(): unit =
    try {
      do { token = scanner.nextToken().trim() } while (token.length() == 0)
    } catch {
      case ex: NoSuchElementException => token = ""
    }

  protected def parseType(): Type = {
    val str = token;
    nextToken();
    val sym = locals.lookup(newTypeName(str));
    if (sym != NoSymbol) sym.tpe
    else {
      val tp = definitions.getClass(str).tpe;
      if (token != "[") tp
      else {
	val args = new ListBuffer[Type];
	do {
	  nextToken(); args append parseType();
	} while (token == ",");
	nextToken();
	appliedType(tp, args.toList)
      }
    }
  }

  protected def parseTypeParam(): Symbol = {
    val vflag =
      if (token == "+") { nextToken(); Flags.COVARIANT }
      else if (token == "-") { nextToken(); Flags.CONTRAVARIANT }
      else 0;
    assert(token.startsWith("?"));
    val sym = owner.newTypeParameter(Position.NOPOS, newTypeName(token))
      .setFlag(vflag)
      .setInfo(TypeBounds(
	definitions.AllClass.tpe,
	definitions.AnyClass.tpe));
    locals enter sym;
    nextToken();
    sym
  }

  protected def parseTypeParams(): List[Symbol] = {
    nextToken();
    val syms = new ListBuffer[Symbol];
    if (token != "]") {
      syms append parseTypeParam();
      while (token == ",") {
        nextToken(); syms append parseTypeParam();
      }
    }
    assert(token == "]");
    syms.toList
  }

  protected def parseParams(): List[Type] = {
    nextToken();
    val tps = new ListBuffer[Type];
    if (token != ")") {
      tps append parseType();
      while (token == ",") {
        nextToken(); tps append parseType();
      }
    }
    assert(token == ")");
    tps.toList
  }

  protected def parseClass(): unit = {
    locals = new Scope();
    def parse(): Type = {
      nextToken();
      if (token == "[") {
	PolyType(parseTypeParams(), parse())
      } else if (token == "extends") {
	val tps = new ListBuffer[Type];
	do {
          nextToken(); tps append parseType()
	} while (token == "with");
	ownertype match {
          case ClassInfoType(parents, defs, clazz) =>
            ClassInfoType(tps.toList, defs, clazz)
	}
      } else ownertype
    }
    owner.setInfo(parse());
    assert(token == ";")
  }

  protected def parseMethod(): unit = {
    val globals = locals;
    locals = if (locals == null) new Scope() else new Scope(locals);
    def parse(): Type = {
      nextToken();
      if (token == "[") PolyType(parseTypeParams(), parse())
      else if (token == "(") MethodType(parseParams(), parse())
      else parseType()
    }
    owner.setInfo(parse());
    locals = globals;
    assert(token == ";")
  }

  protected def parseField(): unit = {
    nextToken();
    owner.setInfo(parseType());
    assert(token == ";")
  }

  protected def parseConstr(): unit = {
    def parse(): Type = {
      nextToken();
      if (token == "(") MethodType(parseParams(), parse())
      else definitions.UnitClass.tpe
    }
    owner.setInfo(parse());
    assert(token == ";")
  }
}
