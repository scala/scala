package scala.reflect
package runtime

import java.lang.{Class => jClass, Package => jPackage}
import java.lang.reflect.{
  Method => jMethod, Constructor => jConstructor, Modifier => jModifier, Field => jField,
  Member => jMember, Type => jType, GenericDeclaration}
import internal.ByteCodecs
import internal.ClassfileConstants._
import internal.pickling.UnPickler
import scala.collection.{ mutable, immutable }

trait JavaConversions { self: Universe =>

  private object unpickler extends UnPickler {
    val global: JavaConversions.this.type = self
  }

  class TwoWayCache[J, S] {
    val toScalaMap = mutable.HashMap[J, S]()
    val toJavaMap = mutable.HashMap[S, J]()

    def toScala(key: J)(body: => S) = toScalaMap.getOrElseUpdate(key, body)
    def toJava(key: S)(body: => J) = toJavaMap.getOrElseUpdate(key, body)
  }

  private val classCache = new TwoWayCache[jClass[_], Symbol]
  private val packageCache = new TwoWayCache[jPackage, Symbol]
  private val methodCache = new TwoWayCache[jMethod, Symbol]
  private val constructorCache = new TwoWayCache[jConstructor[_], Symbol]
  private val fieldCache = new TwoWayCache[jField, Symbol]

  private def createClassModule(owner: Symbol, name: TypeName) = {
    val clazz = owner.newClass(NoPosition, name)
    val module = owner.newModule(NoPosition, name)
    if (owner.isClass) {
      owner.info.decls enter clazz
      owner.info.decls enter module
    }
    (clazz, module)
  }

  private def unpickleClass(jclazz: jClass[_], bytes: Array[Byte], len: Int): ClassSymbol = {
    val (clazz, module) = createClassModule(sOwner(jclazz), newTypeName(jclazz.getSimpleName))
    unpickler.unpickle(bytes, 0, clazz, module, jclazz.getName)
    println("found: "+len+" bytes from "+bytes.deep)
    clazz
  }

  def followStatic(clazz: Symbol, mods: Int) =
    if (jModifier.isStatic(mods)) clazz.companionModule.moduleClass else clazz

  def sOwner(jclazz: jClass[_]): Symbol = {
    if (jclazz.isMemberClass)
      followStatic(classToScala(jclazz.getEnclosingClass), jclazz.getModifiers)
    else if (jclazz.isLocalClass)
      methodToScala(jclazz.getEnclosingMethod) orElse constrToScala(jclazz.getEnclosingConstructor)
    else
      packageToScala(jclazz.getPackage)
  }

  def sOwner(jmember: jMember): Symbol = {
    followStatic(classToScala(jmember.getDeclaringClass), jmember.getModifiers)
  }

  def lookup(clazz: Symbol, jname: String): Symbol =
    clazz.info.decl(newTermName(jname)) orElse {
    (clazz.info.decls.iterator filter (_.name.toString startsWith jname)).toList match {
      case List() => NoSymbol
      case List(sym) => sym
      case alts => clazz.newOverloaded(alts.head.tpe.prefix, alts)
    }
  }

  def erasesTo(meth: Symbol, jmeth: jMethod): Boolean = true
  def erasesTo(meth: Symbol, jconstr: jConstructor[_]): Boolean = true

  def methodToScala(jmeth: jMethod): Symbol = methodCache.toScala(jmeth) {
    val owner = followStatic(classToScala(jmeth.getDeclaringClass), jmeth.getModifiers)
    lookup(owner, jmeth.getName) suchThat (erasesTo(_, jmeth)) orElse jmethodAsScala(jmeth)
  }

  def constrToScala(jconstr: jConstructor[_]): Symbol = constructorCache.toScala(jconstr) {
    val owner = followStatic(classToScala(jconstr.getDeclaringClass), jconstr.getModifiers)
    lookup(owner, "<init>") suchThat (erasesTo(_, jconstr)) orElse jconstrAsScala(jconstr)
  }

  def packageToScala(jpkg: jPackage): Symbol = packageCache.toScala(jpkg) {
    makeScalaPackage(jpkg.getName)
  }

  def packageNameToScala(fullname: String): Symbol = {
    val jpkg = jPackage.getPackage(fullname)
    if (jpkg != null) packageToScala(jpkg) else makeScalaPackage(fullname)
  }

  private def makeScalaPackage(fullname: String): Symbol = {
    val split = fullname lastIndexOf '.'
    val owner = if (split > 0) packageNameToScala(fullname take split) else definitions.RootClass
    val name = fullname drop (split + 1)
    val pkg = owner.newPackage(NoPosition, newTermName(name))
    pkg.moduleClass setInfo new ClassInfoType(List(), new Scope, pkg)
    pkg setInfo pkg.moduleClass.tpe
    pkg.moduleClass
  }

  def classToScala(jclazz: jClass[_]): Symbol = classCache.toScala(jclazz) {
    if (jclazz.isMemberClass) {
      sOwner(jclazz).info.decl(newTypeName(jclazz.getSimpleName)).asInstanceOf[ClassSymbol]
    } else if (jclazz.isLocalClass) { // local classes not preserved by unpickling - treat as Java
      jclassAsScala(jclazz)
    } else { // jclazz is top-level - get signature
      val ssig = jclazz.getAnnotation(classOf[scala.reflect.ScalaSignature])
      if (ssig != null) {
        val bytes = ssig.bytes.getBytes
        val len = ByteCodecs.decode(bytes)
        unpickleClass(jclazz, bytes, len)
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
          unpickleClass(jclazz, bytes, len)
        } else { // class does not have a Scala signature; it's a Java class
          jclassAsScala(jclazz)
        }
      }
    }
  }

  def typeToScala(tpe: jType): Type = NoType

  def jclassAsScala(jclazz: jClass[_]): Symbol = {
    val (clazz, module) = createClassModule(sOwner(jclazz), newTypeName(jclazz.getSimpleName))
    // fill in clazz, module from jclazz
    clazz
  }

  def jfieldAsScala(jfield: jField): Symbol = fieldCache.toScala(jfield) {
    sOwner(jfield).newValue(NoPosition, newTermName(jfield.getName))
      .setFlag(toScalaFlags(jfield.getModifiers, isClass = false))
      .setInfo(typeToScala(jfield.getGenericType))
  }

  def jmethodAsScala(jmeth: jMethod): Symbol = NoSymbol
  def jconstrAsScala(jconstr: jConstructor[_]): Symbol = NoSymbol
}