package scala.reflect
package runtime

import java.lang.{Class => jClass, Package => jPackage}
import java.lang.reflect.{
  Method => jMethod, Constructor => jConstructor, Modifier => jModifier, Field => jField,
  Member => jMember, Type => jType, GenericDeclaration}
import internal.pickling.ByteCodecs
import internal.ClassfileConstants._
import internal.pickling.UnPickler
import collection.mutable.HashMap

trait JavaConversions { self: Universe =>

  private object unpickler extends UnPickler {
    val global: JavaConversions.this.type = self
  }

  /** A cache that maintains a bijection between Java reflection type `J`
   *  and Scala reflection type `S`.
   */
  private class TwoWayCache[J, S] {
    private val toScalaMap = new HashMap[J, S]
    private val toJavaMap = new HashMap[S, J]

    def toScala(key: J)(body: => S) = toScalaMap.getOrElseUpdate(key, body)
    def toJava(key: S)(body: => J) = toJavaMap.getOrElseUpdate(key, body)
  }

  private val classCache = new TwoWayCache[jClass[_], Symbol]
  private val packageCache = new TwoWayCache[jPackage, Symbol]
  private val methodCache = new TwoWayCache[jMethod, Symbol]
  private val constructorCache = new TwoWayCache[jConstructor[_], Symbol]
  private val fieldCache = new TwoWayCache[jField, Symbol]

  /** Generate types for top-level Scala root class and root companion object
   *  from the pickled information stored in a corresponding Java class
   *  @param   clazz   The top-level Scala class for which info is unpickled
   *  @param   module  The top-level Scala companion object for which info is unpickled
   *  @param   jclazz  The Java class which contains the unpickled information in a
   *                   ScalaSignature or ScalaLongSignature annotation.
   */
  def unpickleClass(clazz: Symbol, module: Symbol, jclazz: jClass[_]): Unit = {
    println("unpickling "+clazz+" "+module)
    val ssig = jclazz.getAnnotation(classOf[scala.reflect.ScalaSignature])
    if (ssig != null) {
      val bytes = ssig.bytes.getBytes
      val len = ByteCodecs.decode(bytes)
      unpickler.unpickle(bytes take len, 0, clazz, module, jclazz.getName)
    } else {
      val slsig = jclazz.getAnnotation(classOf[scala.reflect.ScalaLongSignature])
      if (slsig != null) {
        val byteSegments = slsig.bytes map (_.getBytes)
        val lens = byteSegments map ByteCodecs.decode
        val bytes = Array.ofDim[Byte](lens.sum)
        var len = 0
        for ((bs, l) <- byteSegments zip lens) {
          bs.copyToArray(bytes, len, l)
          len += l
        }
        println("long sig")
        unpickler.unpickle(bytes, 0, clazz, module, jclazz.getName)
      } else { // class does not have a Scala signature; it's a Java class
        println("no sig found for "+jclazz)
        copyMembers(clazz, module, jclazz)
      }
    }
  }

  /** Generate types for top-level Scala root class and root companion object
   *  by copying corresponding types from a Java class. This method used
   *  to reflect classes in Scala that do not have a Scala pickle info, be it
   *  because they are local classes or have been compiled from Java sources.
   *  @param   clazz   The top-level Scala class for which info is copied
   *  @param   module  The top-level Scala companion object for which info is copied
   *  @param   jclazz  The Java class
   */
  def copyMembers(clazz: Symbol, module: Symbol, jclazz: jClass[_]) {
    clazz setInfo new ClassInfoType(List(), newScope, clazz)
    module.moduleClass setInfo new ClassInfoType(List(), newScope, module.moduleClass)
    module setInfo module.moduleClass.tpe
  }

  /** If Java modifiers `mods` contain STATIC, return the module class
   *  of the companion module of `clazz`, otherwise the class `clazz` itself.
   */
  private def followStatic(clazz: Symbol, mods: Int) =
    if (jModifier.isStatic(mods)) clazz.companionModule.moduleClass else clazz

  /** The Scala owner of the Scala class corresponding to the Java class `jclazz`
   */
  private def sOwner(jclazz: jClass[_]): Symbol = {
    if (jclazz.isMemberClass)
      followStatic(classToScala(jclazz.getEnclosingClass), jclazz.getModifiers)
    else if (jclazz.isLocalClass)
      methodToScala(jclazz.getEnclosingMethod) orElse constrToScala(jclazz.getEnclosingConstructor)
    else
      packageToScala(jclazz.getPackage)
  }

  /** The Scala owner of the Scala symbol corresponding to the Java member `jmember`
   */
  private def sOwner(jmember: jMember): Symbol = {
    followStatic(classToScala(jmember.getDeclaringClass), jmember.getModifiers)
  }

  /** Find declarations or definition in class `clazz` that maps to a Java
   *  entity with name `jname`. Because of name-mangling, this is more difficult
   *  than a simple name-based lookup via `decl`. If `decl` fails, members
   *  that start with the given name are searched instead.
   */
  private def lookup(clazz: Symbol, jname: String): Symbol =
    clazz.info.decl(newTermName(jname)) orElse {
    (clazz.info.decls.iterator filter (_.name.toString startsWith jname)).toList match {
      case List() => NoSymbol
      case List(sym) => sym
      case alts => clazz.newOverloaded(alts.head.tpe.prefix, alts)
    }
  }

  /** Does method `meth` erase to Java method `jmeth`?
   *  This is true if the Java method type is the same as the Scala method type after performing
   *  all Scala-specific transformations in InfoTransformers. (to be done)
   */
  def erasesTo(meth: Symbol, jmeth: jMethod): Boolean = true //to do: implement

  /** Does constructor `meth` erase to Java method `jconstr`?
   *  This is true if the Java constructor type is the same as the Scala constructor type after performing
   *  all Scala-specific transformations in InfoTransformers. (to be done)
   */
  def erasesTo(meth: Symbol, jconstr: jConstructor[_]): Boolean = true // to do: implement

  /** The Scala method corresponding to given Java method.
   *  @param  jmeth  The Java method
   *  @return A Scala method object that corresponds to `jmeth`.
   */
  def methodToScala(jmeth: jMethod): Symbol = methodCache.toScala(jmeth) {
    val owner = followStatic(classToScala(jmeth.getDeclaringClass), jmeth.getModifiers)
    lookup(owner, jmeth.getName) suchThat (erasesTo(_, jmeth)) orElse jmethodAsScala(jmeth)
  }

  /** The Scala constructor corresponding to given Java constructor.
   *  @param  jconstr  The Java constructor
   *  @return A Scala method object that corresponds to `jconstr`.
   */
  def constrToScala(jconstr: jConstructor[_]): Symbol = constructorCache.toScala(jconstr) {
    val owner = followStatic(classToScala(jconstr.getDeclaringClass), jconstr.getModifiers)
    lookup(owner, "<init>") suchThat (erasesTo(_, jconstr)) orElse jconstrAsScala(jconstr)
  }

  /** The Scala package corresponding to given Java package
   */
  def packageToScala(jpkg: jPackage): Symbol = packageCache.toScala(jpkg) {
    makeScalaPackage(jpkg.getName)
  }

  /** The Scala package with given fully qualified name.
   */
  def packageNameToScala(fullname: String): Symbol = {
    val jpkg = jPackage.getPackage(fullname)
    if (jpkg != null) packageToScala(jpkg) else makeScalaPackage(fullname)
  }

  /** The Scala package with given fully qualified name. Unlike `packageNameToScala`,
   *  this one bypasses the cache.
   */
  private def makeScalaPackage(fullname: String): Symbol = {
    val split = fullname lastIndexOf '.'
    val owner = if (split > 0) packageNameToScala(fullname take split) else definitions.RootClass
    assert(owner.isModuleClass)
    val name = fullname drop (split + 1)
    val pkg = owner.info decl newTermName(name)
    pkg.moduleClass
  }

  /** The Scala class that corresponds to a given Java class.
   *  @param jclazz  The Java class
   *  @return A Scala class symbol that reflects all elements of the Java class,
   *          in the form they appear in the Scala pickling info, or, if that is
   *          not available, wrapped from the Java reflection info.
   */
  def classToScala(jclazz: jClass[_]): Symbol = classCache.toScala(jclazz) {
    if (jclazz.isMemberClass) {
      sOwner(jclazz).info.decl(newTypeName(jclazz.getSimpleName)).asInstanceOf[ClassSymbol]
    } else if (jclazz.isLocalClass) { // local classes not preserved by unpickling - treat as Java
      jclassAsScala(jclazz)
    } else { // jclazz is top-level - get signature
      val (clazz, module) = createClassModule(sOwner(jclazz), newTypeName(jclazz.getSimpleName))
      clazz
    }
  }

  /** The Scala type that corresponds to given Java type (to be done)
   */
  def typeToScala(tpe: jType): Type = NoType

  /** The Scala class that corresponds to given Java class without taking
   *  Scala pickling info into account.
   *  @param jclazz  The Java class
   *  @return A Scala class symbol that wraps all reflection info of `jclazz`
   */
  private def jclassAsScala(jclazz: jClass[_]): Symbol = {
    val (clazz, module) = createClassModule(sOwner(jclazz), newTypeName(jclazz.getSimpleName))
    // fill in clazz, module from jclazz
    copyMembers(clazz, module, jclazz)
    clazz
  }

  /** The Scala field that corresponds to given Java field without taking
   *  Scala pickling info into account.
   *  @param jfield  The Java field
   *  @return A Scala value symbol that wraps all reflection info of `jfield`
   */
  private def jfieldAsScala(jfield: jField): Symbol = fieldCache.toScala(jfield) {
    sOwner(jfield).newValue(NoPosition, newTermName(jfield.getName))
      .setFlag(toScalaFlags(jfield.getModifiers, isClass = false))
      .setInfo(typeToScala(jfield.getGenericType))
      // todo: copy annotations
  }

  /** The Scala method that corresponds to given Java method without taking
   *  Scala pickling info into account.
   *  @param jmeth  The Java method
   *  @return A Scala method symbol that wraps all reflection info of `jmethod`
   */
  private def jmethodAsScala(jmeth: jMethod): Symbol = NoSymbol // to be done

  /** The Scala constructor that corresponds to given Java constructor without taking
   *  Scala pickling info into account.
   *  @param jconstr  The Java constructor
   *  @return A Scala constructor symbol that wraps all reflection info of `jconstr`
   */
  private def jconstrAsScala(jconstr: jConstructor[_]): Symbol = NoSymbol // to be done

  // to be done:

  def packageToJava(pkg: Symbol): jPackage = null // to be done

  /** The Java class corresponding to given Scala class
   */
  def classToJava(clazz: Symbol): jClass[_] = null // to be done
  def fieldToJava(fld: Symbol): jField = null // to be done
  def methodToJava(meth: Symbol): jMethod = null // to be done
  def constrToJava(constr: Symbol): jConstructor[_] = null // to be done
}