package scalap;

import java.io._;
import scala.collection.mutable._;


/** Entities are either text, symbols, or types.
 */
trait Entity {
	def isText: Boolean = false;
	def isType: Boolean = false;
	def isSymbol: Boolean = false;
	def toSource: String = toString();
}

/** Text refers to a single string.
 */
case class Text(str: String) extends Entity {
    override def isText: Boolean = true;
    override def toString(): String = str;
}

/** Types
 */
trait Type extends Entity {
    override def isType: Boolean = true;
    override def toSource: String = {
    	val writer = new ScalaWriter(new StringWriter());
    	writer.setIndentString(null)*;
    	writer.printType(this);
    	writer.toString()
    }
}

case object NoType extends Type;

case class ThisType(sym: Symbol) extends Type;

case class SingletonType(tpe: Type, sym: Symbol) extends Type;

case class TypeRef(tpe: Type, sym: Symbol, args: List[Type]) extends Type;

case class CompoundType(clazz: Symbol, components: List[Type]) extends Type;

case class MethodType(argtpe: List[Type], restpe: Type) extends Type;

case class PolyType(tpe: Type, tvars: List[Symbol]) extends Type;

case class OverloadedType(members: List[Symbol], tpes: List[Type]) extends Type;

case class TypeFlag(tpe: Type, flags: Int) extends Type;

/** Symbols
 */
abstract case class Symbol(name: String, flags: Int) extends Entity {
    var tpe: Type = NoType;
    var owner: Symbol = NoSymbol;
    def fullname: String = owner match {
    	case s: ClassSymbol => {
    		val prefix = s.fullname;
    		if (prefix.length() == 0) name else (prefix + "." + name)
    	}
    	case s: ExternalSymbol => {
    		val prefix = s.fullname;
    		if (prefix.length() == 0) name else (prefix + "." + name)
    	}
    	case _ => name
    }
    def fix(tpe: Type, owner: Symbol): Unit = {
        this.tpe = tpe;
        this.owner = owner;
        owner.enter(this);
    }
    override def isSymbol: Boolean = true;
    override def toString(): String = name;
    def fix(tpe: Type): Unit = {}
    def fix(sym: Symbol): Unit = {}
    def enter(sym: Symbol): Unit = {}
    def members: Buffer[Symbol] = error("symbol does not have members");
}

object NoSymbol extends Symbol("<nosymbol>", 0) {
	override def fix(tpe: Type, owner: Symbol): Unit = {}
}

class TypeSymbol(name: String, flags: Int) extends Symbol(name, flags) {
    var lower: Type = NoType;
    override def fix(tpe: Type): Unit = {
        lower = tpe;
    }
}

class AliasSymbol(name: String, flags: Int) extends Symbol(name, flags) {
    var constr: Symbol = NoSymbol;
    override def fix(sym: Symbol): Unit = {
        constr = sym;
    }
}

class ClassSymbol(name: String, flags: Int) extends Symbol(name, flags) {
    var thistpe: Type = NoType;
    var constr: Symbol = NoSymbol;
    var scope: Buffer[Symbol] = new Buffer;
    override def fix(tpe: Type): Unit = {
        thistpe = tpe;
    }
    override def fix(sym: Symbol): Unit = {
        constr = sym;
    }
    override def enter(sym: Symbol): Unit = scope += sym;
    override def members: Buffer[Symbol] = scope;
}

class ValSymbol(name: String, flags: Int) extends Symbol(name, flags) {
    var clazz: Symbol = NoSymbol;
 	override def fix(sym: Symbol): Unit = {
        clazz = sym;
    }
}

class ExternalSymbol(name: String, mod: Boolean) extends Symbol(name, 0) {
    override def fix(sym: Symbol): Unit = { owner = sym; }
}
