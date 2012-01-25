/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab
package classfile

import java.util.{StringTokenizer, NoSuchElementException}

import scala.collection.mutable.ListBuffer

abstract class MetaParser{

  val global: Global
  import global._

  private var scanner: StringTokenizer = _
  private var owner: Symbol = _
  private var ownertype: Type = _
  private var token: String = _
  private var locals: Scope = null

  def parse(meta: String, sym: Symbol, symtype: Type) {
    //System.out.println("parse meta for " + sym + ":" + meta + ", locals = " + locals);//DEBUG
    this.scanner = new StringTokenizer(meta, "()[], \t<;", true)
    this.owner = sym
    this.ownertype = symtype
    nextToken()
    if (token == "class") parseClass()
    else if (token == "method") parseMethod()
    else if (token == "field") parseField()
    else if (token == "constr") parseConstr()
    else owner.setInfo(symtype);
  }

  protected def nextToken() {
    try {
      do { token = scanner.nextToken().trim() } while (token.length() == 0)
    } catch {
      case ex: NoSuchElementException => token = ""
    }
  }

  protected def parseType(): Type = {
    val str = token
    nextToken()
    val sym = locals.lookup(newTypeName(str))
    if (sym != NoSymbol) sym.tpe
    else {
      val tp = definitions.getRequiredClass(str).tpe;
      if (token != "[") tp
      else {
        val args = new ListBuffer[Type];
        do {
          nextToken(); args += parseType();
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
    assert(token startsWith "?", token)
    val sym = owner.newTypeParameter(newTypeName(token)).setFlag(vflag)
    nextToken()
    val lo =
      if (token == ">") { nextToken(); parseType() }
      else definitions.NothingClass.tpe
    val hi =
      if (token == "<") { nextToken(); parseType() }
      else definitions.AnyClass.tpe
    sym.setInfo(TypeBounds(lo, hi))
    locals enter sym;
    sym
  }

  protected def parseTypeParams(): List[Symbol] = {
    nextToken()
    val syms = new ListBuffer[Symbol]
    if (token != "]") {
      syms += parseTypeParam()
      while (token == ",") {
        nextToken(); syms += parseTypeParam();
      }
    }
    assert(token == "]")
    syms.toList
  }

  protected def parseParams(): List[Type] = {
    nextToken()
    val tps = new ListBuffer[Type]
    if (token != ")") {
      tps += parseType()
      while (token == ",") {
        nextToken(); tps += parseType()
      }
    }
    assert(token == ")")
    tps.toList
  }

  protected def parseClass() {
    locals = newScope
    def parse(): Type = {
      nextToken()
      if (token == "[") {
        PolyType(parseTypeParams(), parse())
      } else if (token == "extends") {
        val tps = new ListBuffer[Type]
        do {
          nextToken(); tps += parseType()
        } while (token == "with");
        ownertype match {
          case ClassInfoType(parents, decls, clazz) =>
            ClassInfoType(tps.toList, decls, clazz)
        }
      } else ownertype
    }
    owner.setInfo(parse())
    assert(token == ";")
  }

  protected def parseMethod() {
    val globals = locals
    locals = if (locals eq null) newScope else newNestedScope(locals)
    def parse(): Type = {
      nextToken();
      if (token == "[") PolyType(parseTypeParams(), parse())
      else if (token == "(") {
        val formals = parseParams()
        MethodType(owner.newSyntheticValueParams(formals), parse())
      }
      else parseType()
    }
    owner.setInfo(parse())
    locals = globals
    assert(token == ";")
  }

  protected def parseField() {
    nextToken()
    owner.setInfo(parseType())
    assert(token == ";")
  }

  protected def parseConstr() {
    def parse(): Type = {
      nextToken()
      if (token == "(") {
        val formals = parseParams()
        MethodType(owner.newSyntheticValueParams(formals), parse())
      }
      else owner.owner.tpe
    }
    owner.setInfo(parse())
    assert(token == ";")
  }
}
