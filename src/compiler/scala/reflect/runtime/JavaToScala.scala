package scala.reflect
package runtime

import java.lang.{ Class => jClass, Package => jPackage, ClassLoader => JClassLoader }
import java.io.IOException
import java.lang.reflect.{
  Method => jMethod,
  Constructor => jConstructor,
  Modifier => jModifier,
  Field => jField,
  Member => jMember,
  Type => jType,
  TypeVariable => jTypeVariable,
  GenericDeclaration,
  GenericArrayType,
  ParameterizedType,
  WildcardType,
  AnnotatedElement
}
import internal.MissingRequirementError
import internal.pickling.ByteCodecs
import internal.ClassfileConstants._
import internal.pickling.UnPickler
import collection.mutable.{ HashMap, ListBuffer }
import internal.Flags._
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.nsc.util.ScalaClassLoader._

trait JavaToScala extends ConversionUtil { self: SymbolTable =>

  import definitions._

  private object unpickler extends UnPickler {
    val global: JavaToScala.this.type = self
  }

  protected def defaultReflectiveClassLoader(): JClassLoader = {
    val cl = Thread.currentThread.getContextClassLoader
    if (cl == null) getClass.getClassLoader else cl
  }

  /** Paul: It seems the default class loader does not pick up root classes, whereas the system classloader does.
   *  Can you check with your newly acquired classloader fu whether this implementation makes sense?
   */
  def javaClass(path: String): jClass[_] =
    javaClass(path, defaultReflectiveClassLoader())
  def javaClass(path: String, classLoader: JClassLoader): jClass[_] =
    Class.forName(path, true, classLoader)

  /** Does `path` correspond to a Java class with that fully qualified name? */
  def isJavaClass(path: String): Boolean =
    try {
      javaClass(path)
      true
    } catch {
      case (_: ClassNotFoundException) | (_: NoClassDefFoundError) | (_: IncompatibleClassChangeError) =>
      false
    }

  /**
   * Generate types for top-level Scala root class and root companion object
   *  from the pickled information stored in a corresponding Java class
   *  @param   clazz   The top-level Scala class for which info is unpickled
   *  @param   module  The top-level Scala companion object for which info is unpickled
   *  @param   jclazz  The Java class which contains the unpickled information in a
   *                   ScalaSignature or ScalaLongSignature annotation.
   */
  def unpickleClass(clazz: Symbol, module: Symbol, jclazz: jClass[_]): Unit = {
    def markAbsent(tpe: Type) = setAllInfos(clazz, module, tpe)
    def handleError(ex: Exception) = {
      markAbsent(ErrorType)
      if (settings.debug.value) ex.printStackTrace()
      val msg = ex.getMessage()
      MissingRequirementError.signal(
        (if (msg eq null) "reflection error while loading " + clazz.name
         else "error while loading " + clazz.name) + ", " + msg)
    }
    try {
      markAbsent(NoType)
      val ssig = jclazz.getAnnotation(classOf[scala.reflect.ScalaSignature])
      if (ssig != null) {
        info("unpickling Scala "+clazz + " and " + module+ ", owner = " + clazz.owner)
        val bytes = ssig.bytes.getBytes
        val len = ByteCodecs.decode(bytes)
        unpickler.unpickle(bytes take len, 0, clazz, module, jclazz.getName)
      } else {
        val slsig = jclazz.getAnnotation(classOf[scala.reflect.ScalaLongSignature])
        if (slsig != null) {
          info("unpickling Scala "+clazz + " and " + module + " with long Scala signature")
          val byteSegments = slsig.bytes map (_.getBytes)
          val lens = byteSegments map ByteCodecs.decode
          val bytes = Array.ofDim[Byte](lens.sum)
          var len = 0
          for ((bs, l) <- byteSegments zip lens) {
            bs.copyToArray(bytes, len, l)
            len += l
          }
          unpickler.unpickle(bytes, 0, clazz, module, jclazz.getName)
        } else { // class does not have a Scala signature; it's a Java class
          info("translating reflection info for Java " + jclazz) //debug
          initClassModule(clazz, module, new FromJavaClassCompleter(clazz, module, jclazz))
        }
      }
    } catch {
      case ex: MissingRequirementError =>
        handleError(ex)
      case ex: IOException =>
        handleError(ex)
    }
  }

  /**
   * A fresh Scala type parameter that corresponds to a Java type variable.
   *  The association between Scala type parameter and Java type variable is entered in the cache.
   *  @param   jtvar   The Java type variable
   */
  private def createTypeParameter(jtvar: jTypeVariable[_ <: GenericDeclaration]): Symbol = {
    val tparam = sOwner(jtvar).newTypeParameter(newTypeName(jtvar.getName))
      .setInfo(new TypeParamCompleter(jtvar))
    tparamCache enter (jtvar, tparam)
    tparam
  }

  /**
   * A completer that fills in the type of a Scala type parameter from the bounds of a Java type variable.
   *  @param   jtvar   The Java type variable
   */
  private class TypeParamCompleter(jtvar: jTypeVariable[_ <: GenericDeclaration]) extends LazyType {
    override def load(sym: Symbol) = complete(sym)
    override def complete(sym: Symbol) = {
      sym setInfo TypeBounds.upper(glb(jtvar.getBounds.toList map typeToScala map objToAny))
    }
  }

  /**
   * Copy all annotations of Java annotated element `jann` over to Scala symbol `sym`.
   *  Pre: `sym` is already initialized with a concrete type.
   *  Note: If `sym` is a method or constructor, its parameter annotations are copied as well.
   */
  private def copyAnnotations(sym: Symbol, jann: AnnotatedElement) {
    // to do: implement
  }

  /**
   * A completer that fills in the types of a Scala class and its companion object
   *  by copying corresponding type info from a Java class. This completer is used
   *  to reflect classes in Scala that do not have a Scala pickle info, be it
   *  because they are local classes or have been compiled from Java sources.
   *  @param   clazz   The Scala class for which info is copied
   *  @param   module  The Scala companion object for which info is copied
   *  @param   jclazz  The Java class
   */
  private class FromJavaClassCompleter(clazz: Symbol, module: Symbol, jclazz: jClass[_]) extends LazyType {
    override def load(sym: Symbol) = {
      debugInfo("completing from Java " + sym + "/" + clazz.fullName)//debug
      assert(sym == clazz || (module != NoSymbol && (sym == module || sym == module.moduleClass)), sym)
      val flags = toScalaClassFlags(jclazz.getModifiers)
      clazz setFlag (flags | JAVA)
      if (module != NoSymbol) {
        module setFlag (flags & PRIVATE | JAVA)
        module.moduleClass setFlag (flags & PRIVATE | JAVA)
      }

      copyAnnotations(clazz, jclazz)
      // to do: annotations to set also for module?

      clazz setInfo new LazyPolyType(jclazz.getTypeParameters.toList map createTypeParameter)
      if (module != NoSymbol) {
        module setInfo module.moduleClass.tpe
        module.moduleClass setInfo new LazyPolyType(List())
      }
    }

    override def complete(sym: Symbol): Unit = {
      load(sym)
      completeRest()
    }
    def completeRest(): Unit = self.synchronized {
      val tparams = clazz.rawInfo.typeParams

      val parents = try {
        parentsLevel += 1
        val jsuperclazz = jclazz.getGenericSuperclass
        val superclazz = if (jsuperclazz == null) AnyClass.tpe else typeToScala(jsuperclazz)
        superclazz :: (jclazz.getGenericInterfaces.toList map typeToScala)
      } finally {
        parentsLevel -= 1
      }
      clazz setInfo GenPolyType(tparams, new ClassInfoType(parents, newScope, clazz))
      if (module != NoSymbol) {
        module.moduleClass setInfo new ClassInfoType(List(), newScope, module.moduleClass)
      }

      def enter(sym: Symbol, mods: Int) =
        (if (jModifier.isStatic(mods)) module.moduleClass else clazz).info.decls enter sym

      for (jinner <- jclazz.getDeclaredClasses) {
        enter(jclassAsScala(jinner, clazz), jinner.getModifiers)
      }

      pendingLoadActions = { () =>

        for (jfield <- jclazz.getDeclaredFields)
          enter(jfieldAsScala(jfield), jfield.getModifiers)

        for (jmeth <- jclazz.getDeclaredMethods)
          enter(jmethodAsScala(jmeth), jmeth.getModifiers)

        for (jconstr <- jclazz.getConstructors)
          enter(jconstrAsScala(jconstr), jconstr.getModifiers)

      } :: pendingLoadActions

      if (parentsLevel == 0) {
        while (!pendingLoadActions.isEmpty) {
          val item = pendingLoadActions.head
          pendingLoadActions = pendingLoadActions.tail
          item()
        }
      }
    }
    class LazyPolyType(override val typeParams: List[Symbol]) extends LazyType {
      override def complete(sym: Symbol) {
        completeRest()
      }
    }
  }

  /** used to avoid cyclies */
  var parentsLevel = 0
  var pendingLoadActions: List[() => Unit] = Nil

  /**
   * If Java modifiers `mods` contain STATIC, return the module class
   *  of the companion module of `clazz`, otherwise the class `clazz` itself.
   */
  private def followStatic(clazz: Symbol, mods: Int) =
    if (jModifier.isStatic(mods)) clazz.companionModule.moduleClass else clazz

  /**
   * The Scala owner of the Scala class corresponding to the Java class `jclazz`
   */
  private def sOwner(jclazz: jClass[_]): Symbol = {
    if (jclazz.isMemberClass) {
      val jEnclosingClass = jclazz.getEnclosingClass
      val sEnclosingClass = classToScala(jEnclosingClass)
      followStatic(sEnclosingClass, jclazz.getModifiers)
    } else if (jclazz.isLocalClass) {
      val jEnclosingMethod = jclazz.getEnclosingMethod
      if (jEnclosingMethod != null) {
        methodToScala(jEnclosingMethod)
      } else {
        val jEnclosingConstructor = jclazz.getEnclosingConstructor
        constrToScala(jEnclosingConstructor)
      }
    } else if (jclazz.isPrimitive || jclazz.isArray) {
      ScalaPackageClass
    } else if (jclazz.getPackage != null) {
      val jPackage = jclazz.getPackage
      packageToScala(jPackage)
    } else {
      // @eb: a weird classloader might return a null package for something with a non-empty package name
      // for example, http://groups.google.com/group/scala-internals/browse_thread/thread/7be09ff8f67a1e5c
      // in that case we could invoke packageNameToScala(jPackageName) and, probably, be okay
      // however, I think, it's better to blow up, since weirdness of the class loader might bite us elsewhere
      val jPackageName = jclazz.getName.substring(0, Math.max(jclazz.getName.lastIndexOf("."), 0))
      assert(jPackageName == "")
      EmptyPackageClass
    }
  }

  /**
   * The Scala owner of the Scala symbol corresponding to the Java member `jmember`
   */
  private def sOwner(jmember: jMember): Symbol = {
    followStatic(classToScala(jmember.getDeclaringClass), jmember.getModifiers)
  }

  /**
   * The Scala owner of the Scala type parameter corresponding to the Java type variable `jtvar`
   */
  private def sOwner(jtvar: jTypeVariable[_ <: GenericDeclaration]): Symbol =
    genericDeclarationToScala(jtvar.getGenericDeclaration)

  /**
   * Returns `true` if Scala name `name` equals Java name `jstr`, possibly after
   *  make-not-private expansion.
   */
  private def approximateMatch(sym: Symbol, jstr: String): Boolean =
    (sym.name.toString == jstr) ||
      sym.isPrivate && nme.expandedName(sym.name.toTermName, sym.owner).toString == jstr

  /**
   * Find declarations or definition in class `clazz` that maps to a Java
   *  entity with name `jname`. Because of name-mangling, this is more difficult
   *  than a simple name-based lookup via `decl`. If `decl` fails, members
   *  that start with the given name are searched instead.
   */
  private def lookup(clazz: Symbol, jname: String): Symbol =
    clazz.info.decl(newTermName(jname)) orElse {
      (clazz.info.decls.iterator filter (approximateMatch(_, jname))).toList match {
        case List()    => NoSymbol
        case List(sym) => sym
        case alts      => clazz.newOverloaded(alts.head.tpe.prefix, alts)
      }
    }

  /**
   * The Scala method corresponding to given Java method.
   *  @param  jmeth  The Java method
   *  @return A Scala method object that corresponds to `jmeth`.
   */
  def methodToScala(jmeth: jMethod): Symbol = methodCache.toScala(jmeth) {
    val jOwner = jmeth.getDeclaringClass
    var sOwner = classToScala(jOwner)
    sOwner = followStatic(sOwner, jmeth.getModifiers)
    lookup(sOwner, jmeth.getName) suchThat (erasesTo(_, jmeth)) orElse jmethodAsScala(jmeth)
  }

  /**
   * The Scala constructor corresponding to given Java constructor.
   *  @param  jconstr  The Java constructor
   *  @return A Scala method object that corresponds to `jconstr`.
   */
  def constrToScala(jconstr: jConstructor[_]): Symbol = constructorCache.toScala(jconstr) {
    val owner = followStatic(classToScala(jconstr.getDeclaringClass), jconstr.getModifiers)
    lookup(owner, "<init>") suchThat (erasesTo(_, jconstr)) orElse jconstrAsScala(jconstr)
  }

  /**
   * The Scala package corresponding to given Java package
   */
  def packageToScala(jpkg: jPackage): Symbol = packageCache.toScala(jpkg) {
    makeScalaPackage(jpkg.getName)
  }

  /**
   * The Scala package with given fully qualified name.
   */
  def packageNameToScala(fullname: String): Symbol = {
    val jpkg = jPackage.getPackage(fullname)
    if (jpkg != null) packageToScala(jpkg) else makeScalaPackage(fullname)
  }

  /**
   * The Scala package with given fully qualified name. Unlike `packageNameToScala`,
   *  this one bypasses the cache.
   */
  def makeScalaPackage(fullname: String): Symbol = {
    val split = fullname lastIndexOf '.'
    val owner = if (split > 0) packageNameToScala(fullname take split) else RootClass
    assert(owner.isModuleClass, owner+" when making "+fullname)
    val name = newTermName(fullname drop (split + 1))
    var pkg = owner.info decl name
    if (pkg == NoSymbol) {
      pkg = owner.newPackage(name)
      pkg.moduleClass setInfo new LazyPackageType
      pkg setInfoAndEnter pkg.moduleClass.tpe
      info("made Scala "+pkg)
    } else if (!pkg.isPackage)
      throw new ReflectError(pkg+" is not a package")
    pkg.moduleClass
  }

  private def scalaSimpleName(jclazz: jClass[_]): TypeName = {
    val owner = sOwner(jclazz)
    val enclosingClass = jclazz.getEnclosingClass
    var prefix = if (enclosingClass != null) enclosingClass.getName else ""
    val isObject = owner.isModuleClass && !owner.isPackageClass
    if (isObject && !prefix.endsWith(nme.MODULE_SUFFIX_STRING)) prefix += nme.MODULE_SUFFIX_STRING
    assert(jclazz.getName.startsWith(prefix))
    var name = jclazz.getName.substring(prefix.length)
    name = name.substring(name.lastIndexOf(".") + 1)
    newTypeName(name)
  }

  /**
   * The Scala class that corresponds to a given Java class.
   *  @param jclazz  The Java class
   *  @return A Scala class symbol that reflects all elements of the Java class,
   *          in the form they appear in the Scala pickling info, or, if that is
   *          not available, wrapped from the Java reflection info.
   */
  def classToScala(jclazz: jClass[_]): Symbol = classCache.toScala(jclazz) {
    val jname = javaTypeName(jclazz)
    val owner = sOwner(jclazz)
    val simpleName = scalaSimpleName(jclazz)

    val sym = {
      def lookup = {
        def coreLookup(name: Name): Symbol = {
          val sym = owner.info.decl(name)
          sym orElse {
            if (name.startsWith(nme.NAME_JOIN_STRING))
              coreLookup(name.subName(1, name.length))
            else
              NoSymbol
          }
        }

        if (nme.isModuleName(simpleName)) {
          val moduleName = nme.stripModuleSuffix(simpleName).toTermName
          val sym = coreLookup(moduleName)
          if (sym == NoSymbol) sym else sym.moduleClass
        } else {
          coreLookup(simpleName)
        }
      }

      if (jclazz.isMemberClass && !nme.isImplClassName(jname)) {
        lookup
      } else if (jclazz.isLocalClass || invalidClassName(jname)) {
        // local classes and implementation classes not preserved by unpickling - treat as Java
        jclassAsScala(jclazz)
      } else if (jclazz.isArray) {
        ArrayClass
      } else javaTypeToValueClass(jclazz) orElse {
        // jclazz is top-level - get signature
        lookup
        //        val (clazz, module) = createClassModule(
        //          sOwner(jclazz), newTypeName(jclazz.getSimpleName), new TopClassCompleter(_, _))
        //        classCache enter (jclazz, clazz)
        //        clazz
      }
    }

    if (!sym.isType) {
      def msgNoSym = "no symbol could be loaded from %s (scala equivalent is %s) by name %s".format(owner, jclazz, simpleName)
      def msgIsNotType = "not a type: symbol %s loaded from %s (scala equivalent is %s) by name %s".format(sym, owner, jclazz, simpleName)
      assert(false, if (sym == NoSymbol) msgNoSym else msgIsNotType)
    }

    sym.asInstanceOf[ClassSymbol]
  }

  /**
   * The Scala type parameter that corresponds to a given Java type parameter.
   *  @param jparam  The Java type parameter
   *  @return A Scala type parameter symbol that has the same owner and name as the Java type parameter
   */
  def tparamToScala(jparam: jTypeVariable[_ <: GenericDeclaration]): Symbol = tparamCache.toScala(jparam) {
    val owner = genericDeclarationToScala(jparam.getGenericDeclaration)
    owner.info match {
      case PolyType(tparams, _) => tparams.find(_.name.toString == jparam.getName).get
    }
  }

  /**
   * The Scala symbol that corresponds to a given Java generic declaration (class, method, or constructor)
   */
  def genericDeclarationToScala(jdecl: GenericDeclaration) = jdecl match {
    case jclazz: jClass[_]        => classToScala(jclazz)
    case jmeth: jMethod           => methodToScala(jmeth)
    case jconstr: jConstructor[_] => constrToScala(jconstr)
  }

  /**
   * Given some Java type arguments, a corresponding list of Scala types, plus potentially
   *  some existentially bound type variables that represent wildcard arguments.
   */
  private def targsToScala(owner: Symbol, args: List[jType]): (List[Type], List[Symbol]) = {
    val tparams = new ListBuffer[Symbol]
    def targToScala(arg: jType): Type = arg match {
      case jwild: WildcardType =>
        val tparam = owner.newExistential(newTypeName("T$" + tparams.length))
          .setInfo(TypeBounds(
            lub(jwild.getLowerBounds.toList map typeToScala),
            glb(jwild.getUpperBounds.toList map typeToScala map objToAny)))
        tparams += tparam
        typeRef(NoPrefix, tparam, List())
      case _ =>
        typeToScala(arg)
    }
    (args map targToScala, tparams.toList)
  }

  /**
   * The Scala type that corresponds to given Java type
   */
  def typeToScala(jtpe: jType): Type = jtpe match {
    case jclazz: jClass[_] =>
      if (jclazz.isArray)
        arrayType(typeToScala(jclazz.getComponentType))
      else {
        val clazz = classToScala(jclazz)
        rawToExistential(typeRef(clazz.owner.thisType, clazz, List()))
      }
    case japplied: ParameterizedType =>
      val (pre, sym) = typeToScala(japplied.getRawType) match {
        case ExistentialType(tparams, TypeRef(pre, sym, _)) => (pre, sym)
        case TypeRef(pre, sym, _)                           => (pre, sym)
      }
      val args0 = japplied.getActualTypeArguments
      val (args, bounds) = targsToScala(pre.typeSymbol, args0.toList)
      ExistentialType(bounds, typeRef(pre, sym, args))
    case jarr: GenericArrayType =>
      arrayType(typeToScala(jarr.getGenericComponentType))
    case jtvar: jTypeVariable[_] =>
      val tparam = tparamToScala(jtvar)
      typeRef(NoPrefix, tparam, List())
  }

  /**
   * The Scala class that corresponds to given Java class without taking
   *  Scala pickling info into account.
   *  @param jclazz  The Java class
   *  @return A Scala class symbol that wraps all reflection info of `jclazz`
   */
  private def jclassAsScala(jclazz: jClass[_]): Symbol = jclassAsScala(jclazz, sOwner(jclazz))

  private def jclassAsScala(jclazz: jClass[_], owner: Symbol): Symbol = {
    val name = scalaSimpleName(jclazz)
    val completer = (clazz: Symbol, module: Symbol) => new FromJavaClassCompleter(clazz, module, jclazz)
    val (clazz, module) = createClassModule(owner, name, completer)
    classCache enter (jclazz, clazz)
    clazz
  }

  /**
   * The Scala field that corresponds to given Java field without taking
   *  Scala pickling info into account.
   *  @param jfield  The Java field
   *  @return A Scala value symbol that wraps all reflection info of `jfield`
   */
  private def jfieldAsScala(jfield: jField): Symbol = fieldCache.toScala(jfield) {
    val field = (
      sOwner(jfield)
        newValue(newTermName(jfield.getName), NoPosition, toScalaFieldFlags(jfield.getModifiers))
        setInfo typeToScala(jfield.getGenericType)
    )
    fieldCache enter (jfield, field)
    copyAnnotations(field, jfield)
    field
  }

  private def setMethType(meth: Symbol, tparams: List[Symbol], paramtpes: List[Type], restpe: Type) = {
    meth setInfo GenPolyType(tparams, MethodType(meth.owner.newSyntheticValueParams(paramtpes map objToAny), restpe))
  }

  /**
   * The Scala method that corresponds to given Java method without taking
   *  Scala pickling info into account.
   *  @param jmeth  The Java method
   *  @return A Scala method symbol that wraps all reflection info of `jmethod`
   */
  private def jmethodAsScala(jmeth: jMethod): Symbol = methodCache.toScala(jmeth) {
    val clazz = sOwner(jmeth)
    val meth = clazz.newMethod(newTermName(jmeth.getName), NoPosition, toScalaMethodFlags(jmeth.getModifiers))
    methodCache enter (jmeth, meth)
    val tparams = jmeth.getTypeParameters.toList map createTypeParameter
    val paramtpes = jmeth.getGenericParameterTypes.toList map typeToScala
    val resulttpe = typeToScala(jmeth.getGenericReturnType)
    setMethType(meth, tparams, paramtpes, resulttpe)
    copyAnnotations(meth, jmeth)
    if ((jmeth.getModifiers & JAVA_ACC_VARARGS) != 0) {
      meth.setInfo(arrayToRepeated(meth.info))
    }
    meth
  }

  /**
   * The Scala constructor that corresponds to given Java constructor without taking
   *  Scala pickling info into account.
   *  @param jconstr  The Java constructor
   *  @return A Scala constructor symbol that wraps all reflection info of `jconstr`
   */
  private def jconstrAsScala(jconstr: jConstructor[_]): Symbol = {
    // [Martin] Note: I know there's a lot of duplication wrt jmethodAsScala, but don't think it's worth it to factor this out.
    val clazz = sOwner(jconstr)
    val constr = clazz.newConstructor(NoPosition, toScalaMethodFlags(jconstr.getModifiers))
    constructorCache enter (jconstr, constr)
    val tparams = jconstr.getTypeParameters.toList map createTypeParameter
    val paramtpes = jconstr.getGenericParameterTypes.toList map typeToScala
    setMethType(constr, tparams, paramtpes, clazz.tpe)
    constr setInfo GenPolyType(tparams, MethodType(clazz.newSyntheticValueParams(paramtpes), clazz.tpe))
    copyAnnotations(constr, jconstr)
    constr
  }
}

class ReflectError(msg: String) extends java.lang.Error(msg)
