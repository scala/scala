/* NSC -- new scala compiler
 * Copyright 2004-2011 LAMP/EPFL
 */


package scala.tools.nsc
package symtab
package clr

import java.io.File
import java.util.{Comparator, StringTokenizer}
import scala.util.Sorting
import ch.epfl.lamp.compiler.msil._
import scala.collection.{ mutable, immutable }
import scala.reflect.internal.util.{Position, NoPosition}

/**
 * Collects all types from all reference assemblies.
 */
abstract class CLRTypes {

  val global: Global
  import global.Symbol
  import global.definitions

  //##########################################################################

  var BYTE: Type = _
  var UBYTE: Type = _
  var SHORT: Type = _
  var USHORT: Type = _
  var CHAR: Type = _
  var INT: Type = _
  var UINT: Type = _
  var LONG: Type = _
  var ULONG: Type = _
  var FLOAT: Type = _
  var DOUBLE: Type = _
  var BOOLEAN: Type = _
  var VOID: Type = _
  var ENUM: Type = _
  var DELEGATE: Type = _

  var OBJECT: Type = _
  var STRING: Type = _
  var STRING_ARRAY: Type = _

  var VALUE_TYPE: Type = _

  var SCALA_SYMTAB_ATTR: Type = _
  var SYMTAB_CONSTR: ConstructorInfo = _
  var SYMTAB_DEFAULT_CONSTR: ConstructorInfo = _

  var DELEGATE_COMBINE: MethodInfo = _
  var DELEGATE_REMOVE: MethodInfo = _

  val types: mutable.Map[Symbol,Type] = new mutable.HashMap
  val constructors: mutable.Map[Symbol,ConstructorInfo] = new mutable.HashMap
  val methods: mutable.Map[Symbol,MethodInfo] = new mutable.HashMap
  val fields: mutable.Map[Symbol, FieldInfo] = new mutable.HashMap
  val sym2type: mutable.Map[Type,Symbol] = new mutable.HashMap
  val addressOfViews = new mutable.HashSet[Symbol]
  val mdgptrcls4clssym: mutable.Map[ /*cls*/ Symbol, /*cls*/ Symbol] = new mutable.HashMap

  def isAddressOf(msym : Symbol) = addressOfViews.contains(msym)

  def isNonEnumValuetype(cls: Symbol) = {
    val msilTOpt = types.get(cls)
    val res = msilTOpt.isDefined && {
      val msilT = msilTOpt.get
      msilT.IsValueType && !msilT.IsEnum
    }
    res
  }

  def isValueType(cls: Symbol): Boolean = {
    val opt = types.get(cls)
    opt.isDefined && opt.get.IsValueType
  }

  def init() = try { // initialize
    // the MsilClasspath (nsc/util/Classpath.scala) initializes the msil-library by calling
    // Assembly.LoadFrom("mscorlib.dll"), so this type should be found
    Type.initMSCORLIB(getTypeSafe("System.String").Assembly)

    BYTE     = getTypeSafe("System.SByte")
    UBYTE    = getTypeSafe("System.Byte")
    CHAR     = getTypeSafe("System.Char")
    SHORT    = getTypeSafe("System.Int16")
    USHORT   = getTypeSafe("System.UInt16")
    INT      = getTypeSafe("System.Int32")
    UINT     = getTypeSafe("System.UInt32")
    LONG     = getTypeSafe("System.Int64")
    ULONG    = getTypeSafe("System.UInt64")
    FLOAT    = getTypeSafe("System.Single")
    DOUBLE   = getTypeSafe("System.Double")
    BOOLEAN  = getTypeSafe("System.Boolean")
    VOID     = getTypeSafe("System.Void")
    ENUM     = getTypeSafe("System.Enum")
    DELEGATE = getTypeSafe("System.MulticastDelegate")

    OBJECT = getTypeSafe("System.Object")
    STRING = getTypeSafe("System.String")
    STRING_ARRAY = getTypeSafe("System.String[]")
    VALUE_TYPE = getTypeSafe("System.ValueType")

    SCALA_SYMTAB_ATTR = getTypeSafe("scala.runtime.SymtabAttribute")
    val bytearray: Array[Type] = Array(Type.GetType("System.Byte[]"))
    SYMTAB_CONSTR = SCALA_SYMTAB_ATTR.GetConstructor(bytearray)
    SYMTAB_DEFAULT_CONSTR = SCALA_SYMTAB_ATTR.GetConstructor(Type.EmptyTypes)

    val delegate: Type = getTypeSafe("System.Delegate")
    val dargs: Array[Type] = Array(delegate, delegate)
    DELEGATE_COMBINE = delegate.GetMethod("Combine", dargs)
    DELEGATE_REMOVE = delegate.GetMethod("Remove", dargs)
  }
  catch {
    case e: RuntimeException =>
      Console.println(e.getMessage)
      throw e
  }

  //##########################################################################
  // type mapping and lookup

  def getType(name: String): Type = Type.GetType(name)

  def getTypeSafe(name: String): Type = {
    val t = Type.GetType(name)
    assert(t != null, name)
    t
  }

  def mkArrayType(elemType: Type): Type = getType(elemType.FullName + "[]")

  def isDelegateType(t: Type): Boolean = { t.BaseType() == DELEGATE }
}  // CLRTypes
