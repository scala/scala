/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab.classfile {

//import scalac_symtab.Modifiers;
//import scalac_symtab.EntryTags._;
import java.io.PrintStream;
import scala.tools.util.{Position, UTF8Codec};
import java.lang.{Float, Double};

/***************************************************
 * Symbol table attribute format: see EntryTags.java
 */
abstract class UnPickle {

  val global: Global;
  import global._;

  private var classRoot: Symbol = _;
  private var moduleRoot: Symbol = _;
  private var bytes: Array[byte] = _;
  private var bp: int = _;
  private var index: Array[int] = _;
  private var entries: Array[AnyRef] = _;

  def parse(bytes: Array[byte], clazz: Symbol, module: Symbol): unit = {
    warning("cannot yet unpickle: " + clazz.fullNameString('.'));
  }
}}
/*
    this.classRoot = clazz;
    this.moduleRoot = module;
    this.bytes = bytes;
    this.bp = 0;
    if (settings.debug.value) global.log("unpickle " + classRoot + " and " + moduleRoot);
    createIndex();
    if (settings.debug.value) printAll(System.out);
    entries = new Array[Any](index.length);
    for (val i <- Iterator.range(0, index.length)) {
      if (isSymbolEntry(i)) getSymbol(i)
    }
    if (settings.debug.value) global.log("unpickled " + classRoot + ":" + classRoot.rawInfo + ", " + moduleRoot + ":" + moduleRoot.rawInfo);//debug
  }

  def readByte(): int = {
    val x = bytes(bp); bp = bp + 1; x
  }

  def readNat(): int = {
    var b = 0;
    var x = 0;
    do {
      b = readByte();
      x = (x << 7) + (b & 0x7f);
    } while ((b & 0x80) != 0);
    x
  }

  def readLong(len: int): long = {
    var x = 0L;
    var i = 0;
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff);
      i = i + 1
    }
    val leading = 64 - (len << 3);
    x << leading >> leading
  }

  private def at[T](start: int)(op: => T): T = {
    val savedBp = bp;
    bp = start;
    val result = op;
    bp = savedBp;
    result
  }

  private def createIndex(): unit = {
    index = new Array[int](readNat());
    for (val i <- Iterator.range(0, index.length)) {
      index(i) = readByte();
      bp = readNat() + bp
    }
  }

  def isTypeEntry(i: int): boolean = {
    val tag = bytes(index(i));
    (firstTypeTag <= tag && tag <= lastTypeTag) || tag == NOpre;
  }

  def isSymbolEntry(i: int): boolean = {
    val tag = bytes(index(i));
    firstSymTag <= tag && tag <= lastSymTag
  }

  def getName(i: int): Name = {
    var name: Name = entries(i).asInstanceOf[Name];
    if (name == null) {
      at(index(i)) {
        val tag = readByte();
        val len = readNat();
        val cs = new Array[char](len);
        val defined = UTF8Codec.decode(bytes, bp, cs, 0, len);
        name = tag match {
	  case TERMname => newTermName(cs, 0, defined)
	  case TYPEname => newTypeName(cs, 0, defined)
	  case _        => errorBadSignature("");
        }
        entries(i) = name
      }
    }
    name
  }

  def readNameRef(): Name = getName(readNat());

  def getSymbol(i: int): Symbol = {
    var sym: Symbol = entries(i).asInstanceOf[Symbol];
    if (sym == null) {
      at(index(i)) {
        val tag = readByte();
        val end = readNat() + bp;
        tag match {
          case NONEsym =>
            sym = NoSymbol;
            entries(i) = sym
            case EXTref | EXTMODCLASSref =>
              val name = readNameRef();
              val owner =
                if (bp == end) definitions.RootClass
                else { assert(bp < end); readSymbolRef() }
              sym =
                if (name.toTermName == nme.ROOT && owner == NoSymbol) {
                  assert(tag != EXTref);
                  global.definitions.RootClass
                } else if (tag == EXTMODCLASSref) {
                  owner.info.decl(name).moduleClass
                } else {
                  owner.info.decl(name)
                }
              entries(i) = sym;
              if (sym == NoSymbol)
                errorBadSignature(
                  "reference " + (if (name.isTypeName) "type " else "value ") +
                  name.decode + " of " + owner + " refers to nonexisting symbol.");
          case _ =>
            assert(isSymbolEntry(i));
            val name = readNameRef();
            if (settings.debug.value) global.log("reading " + name + " at " + i);
            val owner = readSymbolRef();
            if (entries(i) != null)
              sym = entries(i).asInstanceOf[Symbol]
            else {
              val pflags = readNat();
              val inforef = readNat();
              var syminfo: Type = null;
              tag match {
                case TYPEsym =>
                  sym = owner.newAbstractType(Position.NOPOS, name);
                  entries(i) = sym;
		  syminfo = new LazyBoundsTypeRef(
		    inforef, readNat(), (pflags & Modifiers.VIEWBOUND) != 0)
                case ALIASsym =>
                  sym = owner.newAliasType(Position.NOPOS, name);
                  entries(i) = sym;
                  syminfo = new LazyTypeRef(inforef, sym);
                  val constr = readSymbolRef();
                  if (!constr.typeParams.isEmpty)
                    syminfo = new LazyPolyType(
                      constr.typeParams map (.cloneSymbol(sym)),
                      syminfo);
                case CLASSsym =>
                  sym =
                    if ((pflags & Modifiers.MODUL) != 0) {
                      val modulesym = readSymbolRef();
                      modulesym.moduleClass
                    } else if (name == classRoot.name && owner == classRoot.owner) {
                      if (settings.debug.value) global.log("overwriting " + classRoot);
                      classRoot
                    } else {
                      owner.newClass(Position.NOPOS, name)
                    }
                  entries(i) = sym;
                  syminfo = new LazyTypeRef(inforef, sym);
                  sym.setTypeOfThis(new LazyTypeRef(readNat(), sym));
                  val constr = readSymbolRef();
                  if (!constr.typeParams.isEmpty)
                    syminfo = new LazyPolyType(
                      constr.typeParams map (.cloneSymbol(sym)),
                      syminfo);
                case VALsym =>
                  sym =
                    if (name == moduleRoot.name && owner == moduleRoot.owner) {
                      if (settings.debug.value) global.log("overwriting " + moduleRoot);
                      moduleRoot
                    } else if ((pflags & Modifiers.MODUL) != 0) {
                      owner.newModule(Position.NOPOS, name)
                    } else {
                      if (name == nme.CONSTRUCTOR && bp < end) readSymbolRef();
                      owner.newValue(Position.NOPOS, name);
                    }
                    if (sym.isModule) {
                      val clazz = readSymbolRef();
                      assert(clazz == sym.moduleClass, sym)
                    }
                    syminfo = new LazyTypeRef(inforef, sym);
                case _ =>
                  errorBadSignature("");
              }
              sym.setFlag(transFlags(pflags));
              sym.setInfo(syminfo);
              enterSymbol(sym);
            }
        }
      }
    }
    sym
  }

  def readSymbolRef(): Symbol = getSymbol(readNat());

  def readSymbolRefs(end: int): List[Symbol] =
    if (bp == end) List()
    else {
      val symref = readNat();
      if (!isSymbolEntry(symref)) List()
      else getSymbol(symref) :: readSymbolRefs(end)
    }

  def enterSymbol(sym: Symbol): unit =
    if (sym.owner.isClass && !sym.isModuleClass) {
      if (settings.debug.value) global.log("entering " + sym + ":" + sym.tpe + " in " + sym.owner);//debug
      val scope = sym.owner.info.decls;
      val other = scope.lookup(sym.name);
      if (other != sym) {
        sym.info match {
          case OverloadedType(alts) => alts foreach scope.enter
          case _ => scope.enter(sym)
        }
      }
    }

  def getType(i: int, owner: Symbol): Type = {
    var tpe = entries(i).asInstanceOf[Type];
    if (tpe == null) {
      at(index(i)) {
	val tag = readByte();
	val end = readNat() + bp;
	tpe = tag match {
	  case NOtpe =>
	    NoType
	  case NOpre =>
	    NoPrefix
	  case THIStpe =>
	    ThisType(readSymbolRef())
	  case SINGLEtpe =>
	    singleType(readTypeRef(owner), readSymbolRef());
	  case CONSTANTtpe =>
            ConstantType(readTypeRef(owner), readConstantRef())
	  case TYPEREFtpe =>
            // create a type-ref as found, without checks or rebinds
	    new ExtTypeRef(readTypeRef(owner), readSymbolRef(), readTypeRefs(end, owner))
	  case COMPOUNDtpe =>
            val isCompoundSym = readByte() != 0;
            val ctOwner = if (isCompoundSym) readSymbolRef() else null;
            val ctClassRef = readNat();
            val ctClass: Symbol =
	      if (isCompoundSym) entries(ctClassRef).asInstanceOf[Symbol]
	      else getSymbol(ctClassRef);
	    val parents = readTypeRefs(end, owner);
	    if (ctClass == null)
	      refinedType(intersectionType(parents), ctOwner)
	    else if (isCompoundSym)
	      new ExtRefinedType(intersectionType(parents), new Scope, ctClass)
	    else
	      ClassInfoType(parents, new Scope, ctClass);
	  case METHODtpe =>
	    val restype = readTypeRef(owner);
            val argtps = at(bp)(readTypeRefs(end, owner)) map { argtp =>
              val flags = getFlags(readNat());
              if ((flags & DEFflag) != 0)
                PolyType(List(), argtp)
              else if ((flags & REPEATEDflag) != 0)
                appliedType(definitions.RepeatedParamClass.tpe, List(argtp))
              else
                argtp
            }
            MethodType(argtps, restype)
	  case POLYtpe =>
            val restype = readTypeRef(owner);
	    PolyType(readSymbolRefs(end), restype)
	  case OVERLOADEDtpe =>
            val alts = readSymbolRefs(end);
            readTypeRefs(end, NoSymbol);
            OverloadedType(alts)
	  case FLAGGEDtpe =>
	    readNat(); // skip flags
	    readTypeRef(owner)
	  case _ =>
	    errorBadSignature("");
	}
	if (tag != METHODtpe) entries(i) = tpe;
      }
    }
    tpe
  }

  def readTypeRef(owner: Symbol): Type = getType(readNat(), owner);

  def readTypeRefs(end: int, owner: Symbol): List[Type] =
    if (bp == end) List()
    else {
      val typeref = readNat();
      if (!isTypeEntry(typeref)) List()
      else getType(typeref, owner) :: readTypeRefs(end, owner)
    }

  def getFlags(i: int): int =
    at(index(i)) {
      val tag = readByte();
      val end = readNat() + bp;
      if (tag == FLAGGEDtpe) readNat() else 0
    }

  def getConstant(i: int): Any = {
    var value = entries(i);
    if (value == null) {
      at(index(i)) {
        val tag = readByte();
        val len = readNat();
        value = tag match {
          case LITERALunit    => ()
          case LITERALboolean => if (readByte() == 0) false else true
          case LITERALbyte    => readLong(len).asInstanceOf[byte]
          case LITERALshort   => readLong(len).asInstanceOf[short]
          case LITERALchar    => readLong(len).asInstanceOf[char]
          case LITERALint     => readLong(len).asInstanceOf[int]
          case LITERALlong    => readLong(len)
          case LITERALfloat   => Float.intBitsToFloat(readLong(len).asInstanceOf[int])
          case LITERALdouble  => Double.longBitsToDouble(readLong(len))
          case LITERALstring  => readNameRef().toString()
          case LITERALnull    => null
          case _              => errorBadSignature("bad constant tag: " + tag)
        }
        entries(i)= value
      }
    }
    value
  }

  def readConstantRef(): Any = getConstant(readNat());

  def errorBadSignature(msg: String) =
    throw new RuntimeException("malformed Scala signature at " + bp + "; " + msg);

  private case class OverloadedType(alts: List[Symbol]) extends Type;

  private class LazyTypeRef(tpref: int, owner: Symbol) extends LazyType {
    override def complete(sym: Symbol): unit =
      sym.setInfo(getType(tpref, owner));
  }

  private class LazyBoundsTypeRef(hiref: int, loref: int, vubound: boolean) extends LazyType {
    override def complete(sym: Symbol): unit = {
      val hi = if (vubound) definitions.AnyClass.tpe else getType(hiref, sym);
      val vu = if (vubound) getType(hiref, sym) else definitions.AnyClass.tpe;
      val lo = getType(loref, sym);
      sym.setInfo(TypeBounds(lo, hi, vu))
    }
  }

// --- print symbl files -------------------------------------------------

  private def tag2string(tag: int): String = tag match {
    case TERMname => "TERMname";
    case TYPEname => "TYPEname";
    case NONEsym => "NONEsym";
    case TYPEsym => "TYPEsym";
    case ALIASsym => "ALIASsym";
    case CLASSsym => "CLASSsym";
    case VALsym => "VALsym";
    case EXTref => "EXTref";
    case EXTMODCLASSref => "EXTMODCLASSref";
    case NOtpe => "NOtpe";
    case THIStpe => "THIStpe";
    case SINGLEtpe => "SINGLEtpe";
    case TYPEREFtpe => "TYPEREFtpe";
    case CONSTANTtpe => "CONSTANTtpe";
    case COMPOUNDtpe => "COMPOUNDtpe";
    case METHODtpe => "METHODtpe";
    case POLYtpe => "POLYtpe";
    case OVERLOADEDtpe => "OVERLOADEDtpe";
    case UNBOXEDtpe => "UNBOXEDtpe";
    case UNBOXEDARRAYtpe => "UNBOXEDARRAYtpe";
    case FLAGGEDtpe => "FLAGGEDtpe";
    case ERRORtpe => "ERRORtpe";
    case LITERALunit => "LITERALunit";
    case LITERALboolean => "LITERALboolean";
    case LITERALbyte => "LITERALbyte";
    case LITERALshort => "LITERALshort";
    case LITERALchar => "LITERALchar";
    case LITERALint => "LITERALint";
    case LITERALlong => "LITERALlong";
    case LITERALfloat => "LITERALfloat";
    case LITERALdouble => "LITERALdouble";
    case LITERALstring => "LITERALstring";
    case LITERALnull => "LITERALnull";
    case LITERALzero => "LITERALzero";
    case _ => "***BAD TAG***(" + tag + ")";
  }

  def printAll(out: PrintStream): unit = {
    out.println("symbl attribute for " + classRoot + ":");
    for (val i <- Iterator.range(0, index.length)) {
      out.print(i + "," + index(i) + ": ");
      bp = index(i);
      val tag = readByte();
      out.print(tag2string(tag));
      val len = readNat();
      val end = len + bp;
      out.print(" " + len);
      tag match {
	case TERMname | TYPEname =>
	  out.print(" " + UTF8Codec.decode(bytes, bp, len));
	  bp = end;
	case NONEsym =>
        case TYPEsym | ALIASsym | CLASSsym | VALsym =>
	  out.print(" " + readNat()); //name
	  out.print(" " + readNat()); //owner
	  out.print(" " + Integer.toHexString(readNat())); //flags
	  out.print(" " + readNat()); //type
	case FLAGGEDtpe =>
	  out.print(" " + Integer.toHexString(readNat())); //flags
      }
      while (bp < end) out.print(" " + readNat());
      out.println();
    }
  }

  def transFlags(pflags: int): long = {
    var res = 0L;
    if ((pflags & Modifiers.DEFERRED     ) != 0) res = res | Flags.DEFERRED   ;
    if ((pflags & Modifiers.FINAL        ) != 0) res = res | Flags.FINAL      ;
    if ((pflags & Modifiers.PRIVATE      ) != 0) res = res | Flags.PRIVATE    ;
    if ((pflags & Modifiers.PROTECTED    ) != 0) res = res | Flags.PROTECTED  ;
    if ((pflags & Modifiers.SEALED       ) != 0) res = res | Flags.SEALED     ;
    if ((pflags & Modifiers.OVERRIDE     ) != 0) res = res | Flags.OVERRIDE   ;
    if ((pflags & Modifiers.CASE         ) != 0) res = res | Flags.CASE       ;
    if ((pflags & Modifiers.ABSTRACT     ) != 0) res = res | Flags.ABSTRACT   ;
    if ((pflags & Modifiers.SYNTHETIC    ) != 0) res = res | Flags.SYNTHETIC  ;
    if ((pflags & Modifiers.DEPRECATED   ) != 0) res = res | Flags.DEPRECATED ;
    if ((pflags & Modifiers.JAVA         ) != 0) res = res | Flags.JAVA       ;
    if ((pflags & Modifiers.MODUL        ) != 0) res = res | Flags.MODULE     ;
    if ((pflags & Modifiers.MUTABLE      ) != 0) res = res | Flags.MUTABLE    ;
    if ((pflags & Modifiers.PARAM        ) != 0) res = res | Flags.PARAM      ;
    if ((pflags & Modifiers.INITIALIZED  ) != 0) res = res | Flags.INITIALIZED;
    if ((pflags & Modifiers.LOCKED       ) != 0) res = res | Flags.LOCKED     ;
    if ((pflags & Modifiers.ACCESSED     ) != 0) res = res | Flags.ACCESSED   ;
    if ((pflags & Modifiers.SELECTOR     ) != 0) res = res | Flags.SELECTOR   ;
    if ((pflags & Modifiers.PACKAGE      ) != 0) res = res | Flags.PACKAGE    ;
    if ((pflags & Modifiers.STABLE       ) != 0) res = res | Flags.STABLE     ;
    if ((pflags & Modifiers.CAPTURED     ) != 0) res = res | Flags.CAPTURED   ;
    if ((pflags & Modifiers.INCONSTRUCTOR) != 0) res = res | Flags.INCONSTRUCTOR;
    if ((pflags & Modifiers.PARAMACCESSOR) != 0) res = res | Flags.PARAMACCESSOR;
    if ((pflags & Modifiers.ACCESSOR     ) != 0) res = res | Flags.ACCESSOR   ;
    if ((pflags & Modifiers.BRIDGE       ) != 0) res = res | Flags.BRIDGE     ;
    if ((pflags & Modifiers.LIFTED       ) != 0) res = res | Flags.LIFTED     ;
    if ((pflags & Modifiers.ALTERNATIVE  ) != 0) res = res | Flags.ALTERNATIVE;
    if ((pflags & Modifiers.INTERFACE    ) != 0) res = res | Flags.TRAIT      ;
    if ((pflags & Modifiers.TRAIT        ) != 0) res = res | Flags.TRAIT      ;
    if ((pflags & Modifiers.COVARIANT    ) != 0) res = res | Flags.COVARIANT  ;
    if ((pflags & Modifiers.CONTRAVARIANT) != 0) res = res | Flags.CONTRAVARIANT;
    res
  }
}
}
*/
