package scala.reflect
package runtime

import java.lang.{ Class => jClass, Package => jPackage }
import java.lang.reflect.{
  Method => jMethod,
  Constructor => jConstructor,
  Modifier => jModifier,
  Field => jField,
  Member => jMember,
  Type => jType,
  TypeVariable => jTypeVariable,
  GenericDeclaration,
  ParameterizedType,
  WildcardType,
  AnnotatedElement
}
import internal.pickling.ByteCodecs
import internal.ClassfileConstants._
import internal.pickling.UnPickler
import collection.mutable.{ HashMap, ListBuffer }
import internal.Flags._

trait JavaToScala extends ConversionUtil { self: Universe =>

  import definitions._

  private object unpickler extends UnPickler {
    val global: JavaToScala.this.type = self
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
    //println("unpickling " + clazz + " " + module)//debug
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
        //println("long sig")//debug
        unpickler.unpickle(bytes, 0, clazz, module, jclazz.getName)
      } else { // class does not have a Scala signature; it's a Java class
        //println("no sig found for " + jclazz)//debug
        initClassModule(clazz, module, new FromJavaClassCompleter(clazz, module, jclazz))
      }
    }
  }

  /**
   * A fresh Scala type parameter that corresponds to a Java type variable.
   *  The association between Scala type parameter and Java type variable is entered in the cache.
   *  @param   jtvar   The Java type variable
   */
  private def createTypeParameter(jtvar: jTypeVariable[_ <: GenericDeclaration]): Symbol = {
    val tparam = sOwner(jtvar).newTypeParameter(NoPosition, newTypeName(jtvar.getName))
      .setInfo(new TypeParamCompleter(jtvar))
    tparamCache enter (jtvar, tparam)
    tparam
  }

  /**
   * A completer that fills in the type of a Scala type parameter from the bounds of a Java type variable.
   *  @param   jtvar   The Java type variable
   */
  private class TypeParamCompleter(jtvar: jTypeVariable[_ <: GenericDeclaration]) extends LazyType {
    override def complete(sym: Symbol) = {
      sym setInfo TypeBounds(NothingClass.tpe, lub(jtvar.getBounds.toList map typeToScala))
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
    override def complete(sym: Symbol) = {
      //println("completing from Java " + sym + "/" + clazz.fullName)//debug
      assert(sym == clazz || sym == module || sym == module.moduleClass, sym)
      val flags = toScalaFlags(jclazz.getModifiers, isClass = true)
      clazz setFlag (flags | JAVA)
      module setFlag (flags & PRIVATE | JAVA)
      module.moduleClass setFlag (flags & PRIVATE | JAVA)

      copyAnnotations(clazz, jclazz)
      // to do: annotations to set also for module?

      val tparams = jclazz.getTypeParameters.toList map createTypeParameter

      val jsuperclazz = jclazz.getGenericSuperclass
      val superclazz = if (jsuperclazz == null) AnyClass.tpe else typeToScala(jsuperclazz)
      val parents = superclazz :: (jclazz.getGenericInterfaces.toList map typeToScala)
      clazz setInfo polyType(tparams, new ClassInfoType(parents, newScope, clazz))
      module.moduleClass setInfo new ClassInfoType(List(), newScope, module.moduleClass)
      module setInfo module.moduleClass.tpe

      def enter(sym: Symbol) =
        (if (sym.isStatic) module.moduleClass else clazz).info.decls enter sym

      for (jfield <- jclazz.getDeclaredFields)
        enter(jfieldAsScala(jfield))

      for (jmeth <- jclazz.getDeclaredMethods)
        enter(jmethodAsScala(jmeth))

      for (jconstr <- jclazz.getConstructors)
        enter(jconstrAsScala(jconstr))
    }
  }

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
    if (jclazz.isMemberClass)
      followStatic(classToScala(jclazz.getEnclosingClass), jclazz.getModifiers)
    else if (jclazz.isLocalClass)
      methodToScala(jclazz.getEnclosingMethod) orElse constrToScala(jclazz.getEnclosingConstructor)
    else if (jclazz.isPrimitive || jclazz.isArray)
      ScalaPackageClass
    else {
      assert(jclazz.getPackage != null, jclazz)
      packageToScala(jclazz.getPackage)
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
      sym.isPrivate && nme.expandedName(sym.name, sym.owner).toString == jstr

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
    val owner = followStatic(classToScala(jmeth.getDeclaringClass), jmeth.getModifiers)
    lookup(owner, jmeth.getName) suchThat (erasesTo(_, jmeth)) orElse jmethodAsScala(jmeth)
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
  private def makeScalaPackage(fullname: String): Symbol = {
    val split = fullname lastIndexOf '.'
    val owner = if (split > 0) packageNameToScala(fullname take split) else RootClass
    assert(owner.isModuleClass)
    val name = fullname drop (split + 1)
    val pkg = owner.info decl newTermName(name)
    pkg.moduleClass
  }

  /**
   * The Scala class that corresponds to a given Java class.
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
    } else if (jclazz.isArray) {
      ArrayClass
    } else jclazz match {
      case java.lang.Void.TYPE      => UnitClass
      case java.lang.Byte.TYPE      => ByteClass
      case java.lang.Character.TYPE => CharClass
      case java.lang.Short.TYPE     => ShortClass
      case java.lang.Integer.TYPE   => IntClass
      case java.lang.Long.TYPE      => LongClass
      case java.lang.Float.TYPE     => FloatClass
      case java.lang.Double.TYPE    => DoubleClass
      case java.lang.Boolean.TYPE   => BooleanClass
      case _ =>
        // jclazz is top-level - get signature
        val (clazz, module) = createClassModule(
          sOwner(jclazz), newTypeName(jclazz.getSimpleName), new TopClassCompleter(_, _))
        clazz
    }
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
        val tparam = owner.newExistential(NoPosition, newTypeName("T$" + tparams.length))
          .setInfo(TypeBounds(
            lub(jwild.getLowerBounds.toList map typeToScala),
            glb(jwild.getUpperBounds.toList map typeToScala)))
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
  private def jclassAsScala(jclazz: jClass[_]): Symbol = {
    val (clazz, module) = createClassModule(
      sOwner(jclazz), newTypeName(jclazz.getSimpleName), new FromJavaClassCompleter(_, _, jclazz))
    clazz
  }

  /**
   * The Scala field that corresponds to given Java field without taking
   *  Scala pickling info into account.
   *  @param jfield  The Java field
   *  @return A Scala value symbol that wraps all reflection info of `jfield`
   */
  private def jfieldAsScala(jfield: jField): Symbol = fieldCache.toScala(jfield) {
    val field = sOwner(jfield).newValue(NoPosition, newTermName(jfield.getName))
      .setFlag(toScalaFlags(jfield.getModifiers, isClass = false) | JAVA)
      .setInfo(typeToScala(jfield.getGenericType))
    copyAnnotations(field, jfield)
    field
  }

  /**
   * The Scala method that corresponds to given Java method without taking
   *  Scala pickling info into account.
   *  @param jmeth  The Java method
   *  @return A Scala method symbol that wraps all reflection info of `jmethod`
   */
  private def jmethodAsScala(jmeth: jMethod): Symbol = methodCache.toScala(jmeth) {
    val clazz = sOwner(jmeth)
    val meth = clazz.newMethod(NoPosition, newTermName(jmeth.getName))
      .setFlag(toScalaFlags(jmeth.getModifiers, isClass = false) | JAVA)
    val tparams = jmeth.getTypeParameters.toList map createTypeParameter
    val paramtpes = jmeth.getGenericParameterTypes.toList map typeToScala
    val resulttpe = typeToScala(jmeth.getGenericReturnType)
    meth setInfo polyType(tparams, MethodType(clazz.newSyntheticValueParams(paramtpes), resulttpe))
    copyAnnotations(meth, jmeth)
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
    val meth = clazz.newMethod(NoPosition, nme.CONSTRUCTOR)
      .setFlag(toScalaFlags(jconstr.getModifiers, isClass = false) | JAVA)
    val tparams = jconstr.getTypeParameters.toList map createTypeParameter
    val paramtpes = jconstr.getGenericParameterTypes.toList map typeToScala
    meth setInfo polyType(tparams, MethodType(clazz.newSyntheticValueParams(paramtpes), clazz.tpe))
    copyAnnotations(meth, jconstr)
    meth
  }
}