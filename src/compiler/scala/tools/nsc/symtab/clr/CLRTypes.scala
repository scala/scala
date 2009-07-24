/* NSC -- new scala compiler
 * Copyright 2004-2009 LAMP/EPFL
 */

// $Id$

package scala.tools.nsc
package symtab
package clr

import java.io.File
import java.util.{Comparator, StringTokenizer}
import scala.util.Sorting

import ch.epfl.lamp.compiler.msil._

import scala.collection.mutable.{ListBuffer, Map, HashMap, Set, HashSet}
import scala.tools.nsc.util.{Position, NoPosition}

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

  val types: Map[Symbol,Type] = new HashMap
  val constructors: Map[Symbol,ConstructorInfo] = new HashMap
  val methods: Map[Symbol,MethodInfo] = new HashMap
  val fields: Map[Symbol, FieldInfo] = new HashMap
  val sym2type: Map[Type,Symbol] = new HashMap

  private var alltypes: Array[Type] = _

  def init() = try { // initialize
/*
    val assems = new StringTokenizer(global.settings.assemrefs.value, File.pathSeparator)
    while (assems.hasMoreTokens()) {
      assemrefs += new File(assems.nextToken())
    }
    */

    val mscorlib = findAssembly("mscorlib.dll")
    Type.initMSCORLIB(mscorlib)
    findAssembly("scalaruntime.dll")
    findAllAssemblies()

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

    //assert(SCALA_SYMTAB_ATTR != null)

    val delegate: Type = getTypeSafe("System.Delegate")
    val dargs: Array[Type] = Array(delegate, delegate)
    DELEGATE_COMBINE = delegate.GetMethod("Combine", dargs)
    DELEGATE_REMOVE = delegate.GetMethod("Remove", dargs)
    //assert(DELEGATE_COMBINE != null)
    //assert(DELEGATE_REMOVE != null)


    var alltypes: Array[Type] = Type.EmptyTypes
    for (assem <- assemblies) {
      val atypes = assem.GetTypes().filter((typ: Type) => typ.DeclaringType == null)
      alltypes = Array.concat(alltypes, atypes)
    }

    Sorting.stableSort(alltypes, (t1: Type, t2: Type) => (t1.FullName compareTo t2.FullName) < 0)
    this.alltypes = alltypes
  }
  catch {
    case e: RuntimeException =>
      Console.println(e.getMessage)
      // no bloody exits! exit(1)
  }

  //##########################################################################
  // type mapping and lookup

//   private class MyHashMap[A, B <: AnyRef] extends HashMap[A, B] {
//     override def default(key: A): B = null;
//   }

  def getType(name: String): Type = Type.GetType(name)

  def getTypeSafe(name: String): Type = {
    val t = Type.GetType(name)
    assert(t != null, name)
    t
  }

  def mkArrayType(elemType: Type): Type = getType(elemType.FullName + "[]")

  def isDelegateType(t: Type): Boolean = { t.BaseType() == DELEGATE }

  //##########################################################################
  // assembly loading methods

  // a list of all loaded assemblies
  private var assemblies: ListBuffer[Assembly] = new ListBuffer()

  // a set of all directories and assembly files
  //private var assemrefs: Set[File] = new HashSet()

  //def assembly(file : File) = assemrefs += file

  /** Load the assembly with the given name
   */
  private def findAssembly(name: String): Assembly = {
    // see if the assembly is referenced directly
    for (file <- global.assemrefs.iterator if file.getName() == name) {
      val assem = Assembly.LoadFrom(file.getPath())
      if (assem != null) {
	global.assemrefs -= file
	assemblies += assem
	return assem
      }
    }
    // look in directories specified with the '-r' option
    for (dir <- global.assemrefs.iterator if dir.isDirectory()) {
      val file = new File(dir, name)
      if (file.exists()) {
	val assem = Assembly.LoadFrom(file.getPath())
	if (assem != null) {
	  assemblies += assem
	  return assem
	}
      }
    }
    // try in the current directory
    val file = new File(".", name)
    if (file.exists()) {
      val assem = Assembly.LoadFrom(file.getPath())
      if (assem != null) {
	assemblies += assem
	return assem
      }
    }
    throw new RuntimeException(
      "cannot find assembly " + name + "; use the -Xassem-path option to specify its location")
  }

  /** Load the rest of the assemblies specified with the '-r' option
   */
  private def findAllAssemblies() {
    for (file <- global.assemrefs.iterator) {
      if (file.isFile()) {
        //System.out.println("Loading assembly " + file)
	val assem = Assembly.LoadFrom(file.getPath())
	if (assem != null) {
	  assemblies += assem
	}
      }
    }
    global.assemrefs.clear
  }

  //##########################################################################
  // collect the members contained in a given namespace

  /** Find the position of the first type whose name starts with
   *  the given prefix; return the length of the types array if no match
   *  is found so the result canbe used to terminate loop conditions
   */
  private def findFirst(prefix: String): Int = {
    var m = 0
    var n = alltypes.length - 1
    while (m < n) {
      val l = (m + n) / 2
      val res = alltypes(l).FullName.compareTo(prefix)
      if (res < 0) m = l + 1
      else n = l
    }
    if (alltypes(m).FullName.startsWith(prefix)) m else alltypes.length
  }

  /** Collects the members contained in the given Scala package (namespace)
   */
  def collectMembers(pakage: Symbol, typesMap: Map[String,Type], namespacesSet: Set[String]) = {
    val namespace = if (pakage.isRoot) "" else pakage.fullNameString + "."
    val nl = namespace.length()
    var i = findFirst(namespace)
    while (i < alltypes.length && alltypes(i).FullName.startsWith(namespace)) {
      val typ = alltypes(i)
      if (typ.FullName != "java.lang.Object" && typ.FullName != "java.lang.String") {
	val k = typ.FullName.indexOf(".", nl)
	if (k < 0) {
	  typesMap.update(typ.Name, typ)
	} else {
	  namespacesSet += (typ.Namespace.substring(nl, k))
	}
      }
      i += 1
    }
  }

    //##########################################################################
}  // CLRTypes
