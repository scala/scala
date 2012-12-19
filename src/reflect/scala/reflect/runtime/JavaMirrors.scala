package scala.reflect
package runtime

import scala.ref.WeakReference
import scala.collection.mutable.WeakHashMap

import java.lang.{Class => jClass, Package => jPackage}
import java.lang.reflect.{
  Method => jMethod, Constructor => jConstructor, Modifier => jModifier, Field => jField,
  Member => jMember, Type => jType, TypeVariable => jTypeVariable, Array => jArray,
  GenericDeclaration, GenericArrayType, ParameterizedType, WildcardType, AnnotatedElement }
import java.lang.annotation.{Annotation => jAnnotation}
import java.io.IOException
import internal.MissingRequirementError
import internal.pickling.ByteCodecs
import internal.ClassfileConstants._
import internal.pickling.UnPickler
import scala.collection.mutable.{ HashMap, ListBuffer }
import internal.Flags._
//import scala.tools.nsc.util.ScalaClassLoader
//import scala.tools.nsc.util.ScalaClassLoader._
import ReflectionUtils.{staticSingletonInstance, innerSingletonInstance}
import scala.language.existentials
import scala.runtime.{ScalaRunTime, BoxesRunTime}
import scala.reflect.internal.util.Collections._

private[reflect] trait JavaMirrors extends internal.SymbolTable with api.JavaUniverse { thisUniverse: SymbolTable =>

  private lazy val mirrors = new WeakHashMap[ClassLoader, WeakReference[JavaMirror]]()

  private def createMirror(owner: Symbol, cl: ClassLoader): Mirror = {
    val jm = new JavaMirror(owner, cl)
    mirrors(cl) = new WeakReference(jm)
    jm.init()
    jm
  }

  override type RuntimeClass = java.lang.Class[_]

  override type Mirror = JavaMirror

  override lazy val rootMirror: Mirror = createMirror(NoSymbol, rootClassLoader)

  // overriden by ReflectGlobal
  def rootClassLoader: ClassLoader = this.getClass.getClassLoader

  trait JavaClassCompleter extends FlagAssigningCompleter

  def init() = {
    definitions.AnyValClass // force it.

    // establish root association to avoid cyclic dependency errors later
    rootMirror.classToScala(classOf[java.lang.Object]).initialize

    // println("initializing definitions")
    definitions.init()
  }

  def runtimeMirror(cl: ClassLoader): Mirror = mirrors get cl match {
    case Some(WeakReference(m)) => m
    case _ => createMirror(rootMirror.RootClass, cl)
  }

  /** The API of a mirror for a reflective universe */
  class JavaMirror(owner: Symbol,
    /** Class loader that is a mastermind behind the reflexive mirror */
    val classLoader: ClassLoader
  ) extends Roots(owner) with super.JavaMirror { thisMirror =>

    val universe: thisUniverse.type = thisUniverse

    import definitions._

    /** The lazy type for root.
     */
    override lazy val rootLoader = new LazyType with FlagAgnosticCompleter {
      override def complete(sym: Symbol) = sym setInfo new LazyPackageType
    }

    // reflective mirrors can't know the exhaustive list of available packages
    // (that's because compiler mirrors are based on directories and reflective mirrors are based on classloaders,
    // and unlike directories classloaders might make up stuff on the fly)
    // hence we need to be optimistic and create packages out of thin air
    // the same thing is done by the `missingHook` below
    override def staticPackage(fullname: String): ModuleSymbol =
      try super.staticPackage(fullname)
      catch {
        case _: MissingRequirementError =>
          makeScalaPackage(fullname)
      }

// ----------- Caching ------------------------------------------------------------------

    private val classCache = new TwoWayCache[jClass[_], ClassSymbol]
    private val packageCache = new TwoWayCache[Package, ModuleSymbol]
    private val methodCache = new TwoWayCache[jMethod, MethodSymbol]
    private val constructorCache = new TwoWayCache[jConstructor[_], MethodSymbol]
    private val fieldCache = new TwoWayCache[jField, TermSymbol]
    private val tparamCache = new TwoWayCache[jTypeVariable[_ <: GenericDeclaration], TypeSymbol]

    private[runtime] def toScala[J: HasJavaClass, S](cache: TwoWayCache[J, S], key: J)(body: (JavaMirror, J) => S): S =
      cache.toScala(key){
        val jclazz = implicitly[HasJavaClass[J]] getClazz key
        body(mirrorDefining(jclazz), key)
      }

    private implicit val classHasJavaClass: HasJavaClass[jClass[_]] =
      new HasJavaClass(identity)
    private implicit val methHasJavaClass: HasJavaClass[jMethod]
      = new HasJavaClass(_.getDeclaringClass)
    private implicit val fieldHasJavaClass: HasJavaClass[jField] =
      new HasJavaClass(_.getDeclaringClass)
    private implicit val constrHasJavaClass: HasJavaClass[jConstructor[_]] =
      new HasJavaClass(_.getDeclaringClass)
    private implicit val tparamHasJavaClass: HasJavaClass[jTypeVariable[_ <: GenericDeclaration]] =
      new HasJavaClass ( (tparam: jTypeVariable[_ <: GenericDeclaration]) => {
        tparam.getGenericDeclaration match {
          case jclazz: jClass[_] => jclazz
          case jmeth: jMethod => jmeth.getDeclaringClass
          case jconstr: jConstructor[_] => jconstr.getDeclaringClass
        }
      })

// ----------- Implementations of mirror operations and classes  -------------------

    private def ErrorInnerClass(sym: Symbol) = throw new ScalaReflectionException(s"$sym is an inner class, use reflectClass on an InstanceMirror to obtain its ClassMirror")
    private def ErrorInnerModule(sym: Symbol) = throw new ScalaReflectionException(s"$sym is an inner module, use reflectModule on an InstanceMirror to obtain its ModuleMirror")
    private def ErrorStaticClass(sym: Symbol) = throw new ScalaReflectionException(s"$sym is a static class, use reflectClass on a RuntimeMirror to obtain its ClassMirror")
    private def ErrorStaticModule(sym: Symbol) = throw new ScalaReflectionException(s"$sym is a static module, use reflectModule on a RuntimeMirror to obtain its ModuleMirror")
    private def ErrorNotMember(sym: Symbol, owner: Symbol) = throw new ScalaReflectionException(s"expected a member of $owner, you provided ${sym.kindString} ${sym.fullName}")
    private def ErrorNotField(sym: Symbol) = throw new ScalaReflectionException(s"expected a field or an accessor method symbol, you provided $sym")
    private def ErrorNonExistentField(sym: Symbol) = throw new ScalaReflectionException(
      sm"""Scala field ${sym.name} isn't represented as a Java field, neither it has a Java accessor method
          |note that private parameters of class constructors don't get mapped onto fields and/or accessors,
          |unless they are used outside of their declaring constructors.""")
    private def ErrorSetImmutableField(sym: Symbol) = throw new ScalaReflectionException(s"cannot set an immutable field ${sym.name}")
    private def ErrorNotConstructor(sym: Symbol, owner: Symbol) = throw new ScalaReflectionException(s"expected a constructor of $owner, you provided $sym")
    private def ErrorFree(member: Symbol, freeType: Symbol) = throw new ScalaReflectionException(s"cannot reflect ${member.kindString} ${member.name}, because it's a member of a weak type ${freeType.name}")

    /** Helper functions for extracting typed values from a (Class[_], Any)
     *  representing an annotation argument.
     */
    private object toAnnotArg {
      val        StringClass = classOf[String]
      val         ClassClass = classOf[jClass[_]]
      object  PrimitiveClass { def unapply(x: jClass[_]) = x.isPrimitive }
      object       EnumClass { def unapply(x: jClass[_]) = x.isEnum }
      object      ArrayClass { def unapply(x: jClass[_]) = x.isArray }
      object AnnotationClass { def unapply(x: jClass[_]) = x.isAnnotation }

      object ConstantArg {
        def enumToSymbol(enum: Enum[_]): Symbol = {
          val staticPartOfEnum = classToScala(enum.getClass).companionSymbol
          staticPartOfEnum.typeSignature.declaration(enum.name: TermName)
        }

        def unapply(schemaAndValue: (jClass[_], Any)): Option[Any] = schemaAndValue match {
          case (StringClass | PrimitiveClass(), value) => Some(value)
          case (ClassClass, value: jClass[_])          => Some(classToScala(value).toType)
          case (EnumClass(), value: Enum[_])           => Some(enumToSymbol(value))
          case _                                       => None
        }
      }
      def apply(schemaAndValue: (jClass[_], Any)): ClassfileAnnotArg = schemaAndValue match {
        case ConstantArg(value)                      => LiteralAnnotArg(Constant(value))
        case (clazz @ ArrayClass(), value: Array[_]) => ArrayAnnotArg(value map (x => apply(ScalaRunTime.arrayElementClass(clazz) -> x)))
        case (AnnotationClass(), value: jAnnotation) => NestedAnnotArg(JavaAnnotationProxy(value))
        case _                                       => UnmappableAnnotArg
      }
    }
    private case class JavaAnnotationProxy(jann: jAnnotation) extends AnnotationInfo {
      override val atp: Type = classToScala(jann.annotationType).toType
      override val args: List[Tree] = Nil
      override def original: Tree = EmptyTree
      override def setOriginal(t: Tree): this.type = throw new Exception("setOriginal inapplicable for " + this)
      override def pos: Position = NoPosition
      override def setPos(pos: Position): this.type = throw new Exception("setPos inapplicable for " + this)
      override def toString = completeAnnotationToString(this)

      // todo. find out the exact order of assocs as they are written in the class file
      // currently I'm simply sorting the methods to guarantee stability of the output
      override lazy val assocs: List[(Name, ClassfileAnnotArg)] = (
        jann.annotationType.getDeclaredMethods.sortBy(_.getName).toList map (m =>
          (m.getName: TermName) -> toAnnotArg(m.getReturnType -> m.invoke(jann))
        )
      )
    }

    def reflect[T: ClassTag](obj: T): InstanceMirror = new JavaInstanceMirror(obj)

    def reflectClass(cls: ClassSymbol): ClassMirror = {
      if (!cls.isStatic) ErrorInnerClass(cls)
      new JavaClassMirror(null, cls)
    }

    def reflectModule(mod: ModuleSymbol): ModuleMirror = {
      if (!mod.isStatic) ErrorInnerModule(mod)
      new JavaModuleMirror(null, mod)
    }

    def runtimeClass(tpe: Type): RuntimeClass = typeToJavaClass(tpe)

    def runtimeClass(cls: ClassSymbol): RuntimeClass = classToJava(cls)

    def classSymbol(rtcls: RuntimeClass): ClassSymbol = classToScala(rtcls)

    def moduleSymbol(rtcls: RuntimeClass): ModuleSymbol = classToScala(rtcls).companionModule.asModule

    private def ensuringNotFree(sym: Symbol)(body: => Any) {
      val freeType = sym.ownerChain find (_.isFreeType)
      freeType match {
        case Some(freeType) => ErrorFree(sym, freeType)
        case _ => body
      }
    }

    private def checkMemberOf(sym: Symbol, owner: ClassSymbol) {
      if (sym.owner == AnyClass || sym.owner == AnyRefClass || sym.owner == ObjectClass) {
        // do nothing
      } else if (sym.owner == AnyValClass) {
        if (!owner.isPrimitiveValueClass && !owner.isDerivedValueClass) ErrorNotMember(sym, owner)
      } else {
        ensuringNotFree(sym) {
          if (!(owner.info.baseClasses contains sym.owner)) ErrorNotMember(sym, owner)
        }
      }
    }

    private def checkConstructorOf(sym: Symbol, owner: ClassSymbol) {
      if (!sym.isClassConstructor) ErrorNotConstructor(sym, owner)
      ensuringNotFree(sym) {
        if (!owner.info.decls.toList.contains(sym)) ErrorNotConstructor(sym, owner)
      }
    }

    private def preciseClass[T: ClassTag](instance: T) = {
      val staticClazz = classTag[T].runtimeClass
      val dynamicClazz = instance.getClass
      if (staticClazz.isPrimitive) staticClazz else dynamicClazz
    }

    private class JavaInstanceMirror[T: ClassTag](val instance: T)
            extends InstanceMirror {
      def symbol = thisMirror.classSymbol(preciseClass(instance))
      def reflectField(field: TermSymbol): FieldMirror = {
        checkMemberOf(field, symbol)
        if ((field.isMethod && !field.isAccessor) || field.isModule) ErrorNotField(field)
        val name =
          if (field.isGetter) nme.getterToLocal(field.name)
          else if (field.isSetter) nme.getterToLocal(nme.setterToGetter(field.name))
          else field.name
        val field1 = (field.owner.info decl name).asTerm
        try fieldToJava(field1)
        catch {
          case _: NoSuchFieldException => ErrorNonExistentField(field1)
        }
        new JavaFieldMirror(instance, field1)
      }
      def reflectMethod(method: MethodSymbol): MethodMirror = {
        checkMemberOf(method, symbol)
        mkJavaMethodMirror(instance, method)
      }
      def reflectClass(cls: ClassSymbol): ClassMirror = {
        if (cls.isStatic) ErrorStaticClass(cls)
        checkMemberOf(cls, symbol)
        new JavaClassMirror(instance.asInstanceOf[AnyRef], cls)
      }
      def reflectModule(mod: ModuleSymbol): ModuleMirror = {
        if (mod.isStatic) ErrorStaticModule(mod)
        checkMemberOf(mod, symbol)
        new JavaModuleMirror(instance.asInstanceOf[AnyRef], mod)
      }
      override def toString = s"instance mirror for $instance"
    }

    private class JavaFieldMirror(val receiver: Any, val symbol: TermSymbol)
            extends FieldMirror {
      lazy val jfield = {
        val jfield = fieldToJava(symbol)
        if (!jfield.isAccessible) jfield.setAccessible(true)
        jfield
      }
      def get = jfield.get(receiver)
      def set(value: Any) = {
        if (!symbol.isMutable) ErrorSetImmutableField(symbol)
        jfield.set(receiver, value)
      }
      override def toString = s"field mirror for ${symbol.fullName} (bound to $receiver)"
    }

    private def showMethodSig(symbol: MethodSymbol): String = {
      var sig = s"${symbol.fullName}"
      if (symbol.typeParams.nonEmpty) {
        def showTparam(tparam: Symbol) =
          tparam.typeSignature match {
            case tpe @ TypeBounds(_, _) => s"${tparam.name}$tpe"
            case _ => tparam.name
          }
        def showTparams(tparams: List[Symbol]) = "[" + (tparams map showTparam mkString ", ") + "]"
        sig += showTparams(symbol.typeParams)
      }
      if (symbol.paramss.nonEmpty) {
        def showParam(param: Symbol) = s"${param.name}: ${param.typeSignature}"
        def showParams(params: List[Symbol]) = {
          val s_mods = if (params.nonEmpty && params(0).hasFlag(IMPLICIT)) "implicit " else ""
          val s_params = params map showParam mkString ", "
          "(" + s_mods + s_params + ")"
        }
        def showParamss(paramss: List[List[Symbol]]) = paramss map showParams mkString ""
        sig += showParamss(symbol.paramss)
      }
      sig += s": ${symbol.returnType}"
      sig
    }

    // the "symbol == Any_getClass || symbol == Object_getClass" test doesn't cut it
    // because both AnyVal and its primitive descendants define their own getClass methods
    private def isGetClass(meth: MethodSymbol) = meth.name.toString == "getClass" && meth.paramss.flatten.isEmpty
    private def isStringConcat(meth: MethodSymbol) = meth == String_+ || (meth.owner.isPrimitiveValueClass && meth.returnType =:= StringClass.toType)
    lazy val bytecodelessMethodOwners = Set[Symbol](AnyClass, AnyValClass, AnyRefClass, ObjectClass, ArrayClass) ++ ScalaPrimitiveValueClasses
    lazy val bytecodefulObjectMethods = Set[Symbol](Object_clone, Object_equals, Object_finalize, Object_hashCode, Object_toString,
                                        Object_notify, Object_notifyAll) ++ ObjectClass.info.member(nme.wait_).asTerm.alternatives.map(_.asMethod)
    private def isBytecodelessMethod(meth: MethodSymbol): Boolean = {
      if (isGetClass(meth) || isStringConcat(meth) || meth.owner.isPrimitiveValueClass || meth == Predef_classOf || meth.isTermMacro) return true
      bytecodelessMethodOwners(meth.owner) && !bytecodefulObjectMethods(meth)
    }

    // unlike other mirrors, method mirrors are created by a factory
    // that's because we want to have decent performance
    // therefore we move special cases into separate subclasses
    // rather than have them on a hot path them in a unified implementation of the `apply` method
    private def mkJavaMethodMirror[T: ClassTag](receiver: T, symbol: MethodSymbol): JavaMethodMirror = {
      if (isBytecodelessMethod(symbol)) new JavaBytecodelessMethodMirror(receiver, symbol)
      else if (symbol.paramss.flatten exists (p => isByNameParamType(p.info))) new JavaByNameMethodMirror(receiver, symbol)
      else new JavaVanillaMethodMirror(receiver, symbol)
    }

    private abstract class JavaMethodMirror(val symbol: MethodSymbol)
            extends MethodMirror {
      lazy val jmeth = {
        val jmeth = methodToJava(symbol)
        if (!jmeth.isAccessible) jmeth.setAccessible(true)
        jmeth
      }

      def jinvoke(jmeth: jMethod, receiver: Any, args: Seq[Any]): Any = {
        val result = jmeth.invoke(receiver, args.asInstanceOf[Seq[AnyRef]]: _*)
        if (jmeth.getReturnType == java.lang.Void.TYPE) ()
        else result
      }

      override def toString = s"method mirror for ${showMethodSig(symbol)} (bound to $receiver)"
    }

    private class JavaVanillaMethodMirror(val receiver: Any, symbol: MethodSymbol)
            extends JavaMethodMirror(symbol) {
      def apply(args: Any*): Any = jinvoke(jmeth, receiver, args)
    }

    private class JavaByNameMethodMirror(val receiver: Any, symbol: MethodSymbol)
            extends JavaMethodMirror(symbol) {
      def apply(args: Any*): Any = {
        val transformed = map2(args.toList, symbol.paramss.flatten)((arg, param) => if (isByNameParamType(param.info)) () => arg else arg)
        jinvoke(jmeth, receiver, transformed)
      }
    }

    private class JavaBytecodelessMethodMirror[T: ClassTag](val receiver: T, symbol: MethodSymbol)
            extends JavaMethodMirror(symbol) {
       def apply(args: Any*): Any = {
        // checking type conformance is too much of a hassle, so we don't do it here
        // actually it's not even necessary, because we manually dispatch arguments below
        val params = symbol.paramss.flatten
        val perfectMatch = args.length == params.length
        // todo. this doesn't account for multiple vararg parameter lists
        // however those aren't supported by the mirror API: https://issues.scala-lang.org/browse/SI-6182
        // hence I leave this code as is, to be fixed when the corresponding bug is fixed
        val varargMatch = args.length >= params.length - 1 && isVarArgsList(params)
        if (!perfectMatch && !varargMatch) {
          val n_arguments = if (isVarArgsList(params)) s"${params.length - 1} or more" else s"${params.length}"
          var s_arguments = if (params.length == 1 && !isVarArgsList(params)) "argument" else "arguments"
          throw new ScalaReflectionException(s"${showMethodSig(symbol)} takes $n_arguments $s_arguments")
        }

        def objReceiver       = receiver.asInstanceOf[AnyRef]
        def objArg0           = args(0).asInstanceOf[AnyRef]
        def objArgs           = args.asInstanceOf[Seq[AnyRef]]
        def fail(msg: String) = throw new ScalaReflectionException(msg + ", it cannot be invoked with mirrors")

        def invokePrimitiveMethod = {
          val jmeths = classOf[BoxesRunTime].getDeclaredMethods.filter(_.getName == nme.primitiveMethodName(symbol.name).toString)
          assert(jmeths.length == 1, jmeths.toList)
          jinvoke(jmeths.head, null, objReceiver +: objArgs)
        }

        symbol match {
          case Any_== | Object_==                     => ScalaRunTime.inlinedEquals(objReceiver, objArg0)
          case Any_!= | Object_!=                     => !ScalaRunTime.inlinedEquals(objReceiver, objArg0)
          case Any_## | Object_##                     => ScalaRunTime.hash(objReceiver)
          case Any_equals                             => receiver.equals(objArg0)
          case Any_hashCode                           => receiver.hashCode
          case Any_toString                           => receiver.toString
          case Object_eq                              => objReceiver eq objArg0
          case Object_ne                              => objReceiver ne objArg0
          case Object_synchronized                    => objReceiver.synchronized(objArg0)
          case sym if isGetClass(sym)                 => preciseClass(receiver)
          case Any_asInstanceOf                       => fail("Any.asInstanceOf requires a type argument")
          case Any_isInstanceOf                       => fail("Any.isInstanceOf requires a type argument")
          case Object_asInstanceOf                    => fail("AnyRef.$asInstanceOf is an internal method")
          case Object_isInstanceOf                    => fail("AnyRef.$isInstanceOf is an internal method")
          case Array_length                           => ScalaRunTime.array_length(objReceiver)
          case Array_apply                            => ScalaRunTime.array_apply(objReceiver, args(0).asInstanceOf[Int])
          case Array_update                           => ScalaRunTime.array_update(objReceiver, args(0).asInstanceOf[Int], args(1))
          case Array_clone                            => ScalaRunTime.array_clone(objReceiver)
          case sym if isStringConcat(sym)             => receiver.toString + objArg0
          case sym if sym.owner.isPrimitiveValueClass => invokePrimitiveMethod
          case sym if sym == Predef_classOf           => fail("Predef.classOf is a compile-time function")
          case sym if sym.isTermMacro                 => fail(s"${symbol.fullName} is a macro, i.e. a compile-time function")
          case _                                      => abort(s"unsupported symbol $symbol when invoking $this")
        }
      }
    }

    private class JavaConstructorMirror(val outer: AnyRef, val symbol: MethodSymbol)
            extends MethodMirror {
      override val receiver = outer
      lazy val jconstr = {
        val jconstr = constructorToJava(symbol)
        if (!jconstr.isAccessible) jconstr.setAccessible(true)
        jconstr
      }
      def apply(args: Any*): Any = {
        if (symbol.owner == ArrayClass)
          throw new ScalaReflectionException("Cannot instantiate arrays with mirrors. Consider using `scala.reflect.ClassTag(<class of element>).newArray(<length>)` instead")

        val effectiveArgs =
          if (outer == null) args.asInstanceOf[Seq[AnyRef]]
          else outer +: args.asInstanceOf[Seq[AnyRef]]
        jconstr.newInstance(effectiveArgs: _*)
      }
      override def toString = s"constructor mirror for ${showMethodSig(symbol)} (bound to $outer)"
    }

    private abstract class JavaTemplateMirror
            extends TemplateMirror {
      def outer: AnyRef
      def erasure: ClassSymbol
      lazy val signature = typeToScala(classToJava(erasure))
    }

    private class JavaClassMirror(val outer: AnyRef, val symbol: ClassSymbol)
            extends JavaTemplateMirror with ClassMirror {
      def erasure = symbol
      def isStatic = false
      def reflectConstructor(constructor: MethodSymbol) = {
        checkConstructorOf(constructor, symbol)
        new JavaConstructorMirror(outer, constructor)
      }
      override def toString = s"class mirror for ${symbol.fullName} (bound to $outer)"
    }

    private class JavaModuleMirror(val outer: AnyRef, val symbol: ModuleSymbol)
            extends JavaTemplateMirror with ModuleMirror {
      def erasure = symbol.moduleClass.asClass
      def isStatic = true
      def instance = {
        if (symbol.owner.isPackageClass)
          staticSingletonInstance(classLoader, symbol.fullName)
        else
          if (outer == null) staticSingletonInstance(classToJava(symbol.moduleClass.asClass))
          else innerSingletonInstance(outer, symbol.name)
      }
      override def toString = s"module mirror for ${symbol.fullName} (bound to $outer)"
    }

// -------------------- Java to Scala  -----------------------------------

    /** Does method `meth` erase to Java method `jmeth`?
     *  This is true if the Java method type is the same as the Scala method type after performing
     *  all Scala-specific transformations in InfoTransformers. (to be done)
     */
    private def erasesTo(meth: Symbol, jmeth: jMethod): Boolean = {
      val mtpe = transformedType(meth)
      (mtpe.paramTypes map runtimeClass) == jmeth.getParameterTypes.toList &&
      runtimeClass(mtpe.resultType) == jmeth.getReturnType
    }

    private def erasesTo(meth: Symbol, jconstr: jConstructor[_]): Boolean = {
      val mtpe = transformedType(meth)
      (mtpe.paramTypes map runtimeClass) == jconstr.getParameterTypes.toList &&
      runtimeClass(mtpe.resultType) == jconstr.getDeclaringClass
    }

    def javaClass(path: String): jClass[_] =
      Class.forName(path, true, classLoader)

    /** Does `path` correspond to a Java class with that fully qualified name in the current class loader? */
    def tryJavaClass(path: String): Option[jClass[_]] =
      try {
        Some(javaClass(path))
      } catch {
        case (_: ClassNotFoundException) | (_: NoClassDefFoundError) | (_: IncompatibleClassChangeError) =>
          None
      }

    /** The mirror that corresponds to the classloader that original defined the given Java class */
    def mirrorDefining(jclazz: jClass[_]): JavaMirror = {
      val cl = jclazz.getClassLoader
      if (cl == this.classLoader) this else runtimeMirror(cl)
    }

    private object unpickler extends UnPickler {
      val global: thisUniverse.type = thisUniverse
    }

    /** how connected????
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
      // don't use classOf[scala.reflect.ScalaSignature] here, because it will use getClass.getClassLoader, not mirror's classLoader
      // don't use asInstanceOf either because of the same reason (lol, I cannot believe I fell for it)
      // don't use structural types to simplify reflective invocations because of the same reason
      def loadAnnotation(name: String): Option[java.lang.annotation.Annotation] =
        tryJavaClass(name) flatMap { annotClass =>
          val anns = jclazz.getAnnotations
          val result = anns find (_.annotationType == annotClass)
          if (result.isEmpty && (anns exists (_.annotationType.getName == name)))
            throw new ClassNotFoundException(
              sm"""Mirror classloader mismatch: $jclazz (loaded by ${ReflectionUtils.show(jclazz.getClassLoader)})
                  |is unrelated to the mirror's classloader: (${ReflectionUtils.show(classLoader)})""")
          result
        }
      def loadBytes[T: ClassTag](name: String): Option[T] =
        loadAnnotation(name) map { ssig =>
          val bytesMethod = ssig.annotationType.getMethod("bytes")
          bytesMethod.invoke(ssig).asInstanceOf[T]
        }

      try {
        markAbsent(NoType)
        loadBytes[String]("scala.reflect.ScalaSignature") match {
          case Some(ssig) =>
            info(s"unpickling Scala $clazz and $module, owner = ${clazz.owner}")
            val bytes = ssig.getBytes
            val len = ByteCodecs.decode(bytes)
            unpickler.unpickle(bytes take len, 0, clazz, module, jclazz.getName)
          case None =>
            loadBytes[Array[String]]("scala.reflect.ScalaLongSignature") match {
              case Some(slsig) =>
                info(s"unpickling Scala $clazz and $module with long Scala signature")
                val byteSegments = slsig map (_.getBytes)
                val lens = byteSegments map ByteCodecs.decode
                val bytes = Array.ofDim[Byte](lens.sum)
                var len = 0
                for ((bs, l) <- byteSegments zip lens) {
                  bs.copyToArray(bytes, len, l)
                  len += l
                }
                unpickler.unpickle(bytes, 0, clazz, module, jclazz.getName)
              case None =>
                // class does not have a Scala signature; it's a Java class
                info("translating reflection info for Java " + jclazz) //debug
                initClassAndModule(clazz, module, new FromJavaClassCompleter(clazz, module, jclazz))
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
    private def createTypeParameter(jtvar: jTypeVariable[_ <: GenericDeclaration]): TypeSymbol = {
      val tparam = sOwner(jtvar).newTypeParameter(newTypeName(jtvar.getName))
        .setInfo(new TypeParamCompleter(jtvar))
      tparamCache enter (jtvar, tparam)
      tparam
    }

    /**
     * A completer that fills in the type of a Scala type parameter from the bounds of a Java type variable.
     *  @param   jtvar   The Java type variable
     */
    private class TypeParamCompleter(jtvar: jTypeVariable[_ <: GenericDeclaration]) extends LazyType with FlagAgnosticCompleter {
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
      sym setAnnotations (jann.getAnnotations map JavaAnnotationProxy).toList
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
    private class FromJavaClassCompleter(clazz: Symbol, module: Symbol, jclazz: jClass[_]) extends LazyType with JavaClassCompleter with FlagAssigningCompleter {

      /** used to avoid cycles while initializing classes */
      private var parentsLevel = 0
      private var pendingLoadActions: List[() => Unit] = Nil

      override def load(sym: Symbol): Unit = {
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

      def completeRest(): Unit = thisUniverse.synchronized {
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

        for (jinner <- jclazz.getDeclaredClasses)
          jclassAsScala(jinner) // inner class is entered as a side-effect
                                // no need to call enter explicitly

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

      class LazyPolyType(override val typeParams: List[Symbol]) extends LazyType with FlagAgnosticCompleter {
        override def complete(sym: Symbol) {
          completeRest()
        }
      }
    }

    /**
     * If Java modifiers `mods` contain STATIC, return the module class
     *  of the companion module of `clazz`, otherwise the class `clazz` itself.
     */
    private def followStatic(clazz: Symbol, mods: Int) =
      if (jModifier.isStatic(mods)) clazz.companionModule.moduleClass else clazz

  /** Methods which need to be treated with care
   *  because they either are getSimpleName or call getSimpleName:
   *
   *    public String getSimpleName()
   *    public boolean isAnonymousClass()
   *    public boolean isLocalClass()
   *    public String getCanonicalName()
   *
   *  A typical manifestation:
   *
   *    // java.lang.Error: sOwner(class Test$A$1) has failed
   *    // Caused by: java.lang.InternalError: Malformed class name
   *    //        at java.lang.Class.getSimpleName(Class.java:1133)
   *    //        at java.lang.Class.isAnonymousClass(Class.java:1188)
   *    //        at java.lang.Class.isLocalClass(Class.java:1199)
   *    // (see t5256c.scala for more details)
   *
   *  TODO - find all such calls and wrap them.
   *  TODO - create mechanism to avoid the recurrence of unwrapped calls.
   */
   implicit class RichClass(jclazz: jClass[_]) {
      // `jclazz.isLocalClass` doesn't work because of problems with `getSimpleName`
      // hence we have to approximate by removing the `isAnonymousClass` check
//      def isLocalClass0: Boolean = jclazz.isLocalClass
      def isLocalClass0: Boolean = jclazz.getEnclosingMethod != null || jclazz.getEnclosingConstructor != null
    }

    /**
     * The Scala owner of the Scala class corresponding to the Java class `jclazz`
     */
    private def sOwner(jclazz: jClass[_]): Symbol =
      if (jclazz.isMemberClass) {
        val jEnclosingClass = jclazz.getEnclosingClass
        val sEnclosingClass = classToScala(jEnclosingClass)
        followStatic(sEnclosingClass, jclazz.getModifiers)
      } else if (jclazz.isLocalClass0) {
        val jEnclosingMethod = jclazz.getEnclosingMethod
        if (jEnclosingMethod != null) {
          methodToScala(jEnclosingMethod)
        } else {
          val jEnclosingConstructor = jclazz.getEnclosingConstructor
          constructorToScala(jEnclosingConstructor)
        }
      } else if (jclazz.isPrimitive || jclazz.isArray) {
        ScalaPackageClass
      } else if (jclazz.getPackage != null) {
        val jPackage = jclazz.getPackage
        packageToScala(jPackage).moduleClass
      } else {
        // @eb: a weird classloader might return a null package for something with a non-empty package name
        // for example, http://groups.google.com/group/scala-internals/browse_thread/thread/7be09ff8f67a1e5c
        // in that case we could invoke packageNameToScala(jPackageName) and, probably, be okay
        // however, I think, it's better to blow up, since weirdness of the class loader might bite us elsewhere
        // [martin] I think it's better to be forgiving here. Restoring packageNameToScala.
        val jPackageName = jclazz.getName take jclazz.getName.lastIndexOf('.')
        packageNameToScala(jPackageName).moduleClass
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
     * Find declarations or definition in class `clazz` that maps to a Java
     *  entity with name `jname`. Because of name-mangling, this is more difficult
     *  than a simple name-based lookup via `decl`. If `decl` fails, members
     *  that start with the given name are searched instead.
     */
    private def lookup(clazz: Symbol, jname: String): Symbol = {
      def approximateMatch(sym: Symbol, jstr: String): Boolean =
        (sym.name.toString == jstr) ||
        sym.isPrivate && nme.expandedName(sym.name.toTermName, sym.owner).toString == jstr

      clazz.info.decl(newTermName(jname)) orElse {
        (clazz.info.decls.iterator filter (approximateMatch(_, jname))).toList match {
          case List()    => NoSymbol
          case List(sym) => sym
          case alts      => clazz.newOverloaded(alts.head.tpe.prefix, alts)
        }
      }
    }

    /**
     * The Scala method corresponding to given Java method.
     *  @param  jmeth  The Java method
     *  @return A Scala method object that corresponds to `jmeth`.
     */
    def methodToScala(jmeth: jMethod): MethodSymbol =
      toScala(methodCache, jmeth)(_ methodToScala1 _)

    private def methodToScala1(jmeth: jMethod): MethodSymbol = {
      val jOwner = jmeth.getDeclaringClass
      val preOwner = classToScala(jOwner)
      val owner = followStatic(preOwner, jmeth.getModifiers)
      (lookup(owner, jmeth.getName) suchThat (erasesTo(_, jmeth)) orElse jmethodAsScala(jmeth))
        .asMethod
    }

    /**
     * The Scala constructor corresponding to given Java constructor.
     *  @param  jconstr  The Java constructor
     *  @return A Scala method object that corresponds to `jconstr`.
     */
    def constructorToScala(jconstr: jConstructor[_]): MethodSymbol =
      toScala(constructorCache, jconstr)(_ constructorToScala1 _)

    private def constructorToScala1(jconstr: jConstructor[_]): MethodSymbol = {
      val owner = followStatic(classToScala(jconstr.getDeclaringClass), jconstr.getModifiers)
      (lookup(owner, jconstr.getName) suchThat (erasesTo(_, jconstr)) orElse jconstrAsScala(jconstr))
        .asMethod
    }

    /**
     * The Scala field corresponding to given Java field.
     *  @param  jfield  The Java field
     *  @return A Scala field object that corresponds to `jfield`.
     *  // ??? should we return the getter instead?
     */
    def fieldToScala(jfield: jField): TermSymbol =
      toScala(fieldCache, jfield)(_ fieldToScala1 _)

    private def fieldToScala1(jfield: jField): TermSymbol = {
      val owner = followStatic(classToScala(jfield.getDeclaringClass), jfield.getModifiers)
      (lookup(owner, jfield.getName) suchThat (!_.isMethod) orElse jfieldAsScala(jfield)).asTerm
    }

    /**
     * The Scala package corresponding to given Java package
     */
    def packageToScala(jpkg: jPackage): ModuleSymbol = packageCache.toScala(jpkg) {
      makeScalaPackage(jpkg.getName)
    }

    /**
     * The Scala package with given fully qualified name.
     */
    def packageNameToScala(fullname: String): ModuleSymbol = {
      if (fullname == "") EmptyPackage
      else {
        val jpkg = jPackage.getPackage(fullname)
        if (jpkg != null) packageToScala(jpkg) else makeScalaPackage(fullname)
      }
    }

    /**
     * The Scala package with given fully qualified name. Unlike `packageNameToScala`,
     *  this one bypasses the cache.
     */
    private[JavaMirrors] def makeScalaPackage(fullname: String): ModuleSymbol = {
      val split = fullname lastIndexOf '.'
      val ownerModule: ModuleSymbol =
        if (split > 0) packageNameToScala(fullname take split) else this.RootPackage
      val owner = ownerModule.moduleClass
      val name = (fullname: TermName) drop split + 1
      val opkg = owner.info decl name
      if (opkg.isPackage)
        opkg.asModule
      else if (opkg == NoSymbol) {
        val pkg = owner.newPackage(name)
        pkg.moduleClass setInfo new LazyPackageType
        pkg setInfoAndEnter pkg.moduleClass.tpe
        info("made Scala "+pkg)
        pkg
      } else
        throw new ReflectError(opkg+" is not a package")
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
    def classToScala(jclazz: jClass[_]): ClassSymbol =
      toScala(classCache, jclazz)(_ classToScala1 _)

    private def classToScala1(jclazz: jClass[_]): ClassSymbol = {
      val jname = newTypeName(jclazz.getName)
      if (jname == fulltpnme.RuntimeNothing) NothingClass
      else if (jname == fulltpnme.RuntimeNull) NullClass
      else {
        val owner = sOwner(jclazz)
        val simpleName = scalaSimpleName(jclazz)

        def lookupClass = {
          def coreLookup(name: Name): Symbol =
            owner.info.decl(name) orElse {
              if (name.startsWith(nme.NAME_JOIN_STRING)) coreLookup(name drop 1) else NoSymbol
            }
          if (nme.isModuleName(simpleName))
            coreLookup(nme.stripModuleSuffix(simpleName).toTermName) map (_.moduleClass)
          else
            coreLookup(simpleName)
        }

        val cls =
          if (jclazz.isMemberClass && !nme.isImplClassName(jname))
            lookupClass
          else if (jclazz.isLocalClass0 || isInvalidClassName(jname))
            // local classes and implementation classes not preserved by unpickling - treat as Java
            //
            // upd. but only if they cannot be loaded as top-level classes
            // otherwise we may mistake mangled symbolic names for mangled nested names
            //
            // in case when a Java binary name can be treated both as a top-level class and as a nested class
            // (as described in http://groups.google.com/group/scala-internals/browse_thread/thread/10855403bbf04298)
            // we check for a top-level class first
            // this is totally correct, because a top-level class and a nested class with the same name cannot coexist
            // so it's either one or another, but not both - therefore we always load $-bearing classes correctly
            lookupClass orElse jclassAsScala(jclazz)
          else if (jclazz.isArray)
            ArrayClass
          else
            javaTypeToValueClass(jclazz) orElse lookupClass

        assert (cls.isType,
          sm"""${if (cls == NoSymbol) "not a type: symbol" else "no symbol could be"}
              | loaded from $jclazz in $owner with name $simpleName and classloader $classLoader""")

        cls.asClass
      }
    }

    /**
     * The Scala type parameter that corresponds to a given Java type parameter.
     *  @param jparam  The Java type parameter
     *  @return A Scala type parameter symbol that has the same owner and name as the Java type parameter
     */
    def typeParamToScala(jparam: jTypeVariable[_ <: GenericDeclaration]): TypeSymbol =
      toScala(tparamCache, jparam)(_ typeParamToScala1 _)

    private def typeParamToScala1(jparam: jTypeVariable[_ <: GenericDeclaration]): TypeSymbol = {
      val owner = genericDeclarationToScala(jparam.getGenericDeclaration)
      owner.info match {
        case PolyType(tparams, _) => tparams.find(_.name.toString == jparam.getName).get.asType
      }
    }

    /**
     * The Scala symbol that corresponds to a given Java generic declaration (class, method, or constructor)
     */
    def genericDeclarationToScala(jdecl: GenericDeclaration): Symbol = jdecl match {
      case jclazz: jClass[_]        => classToScala(jclazz)
      case jmeth: jMethod           => methodToScala(jmeth)
      case jconstr: jConstructor[_] => constructorToScala(jconstr)
    }

    /**
     * Given some Java type arguments, a corresponding list of Scala types, plus potentially
     *  some existentially bound type variables that represent wildcard arguments.
     */
    private def targsToScala(owner: Symbol, args: List[jType]): (List[Type], List[TypeSymbol]) = {
      val tparams = new ListBuffer[TypeSymbol]
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
        // http://stackoverflow.com/questions/5767122/parameterizedtype-getrawtype-returns-j-l-r-type-not-class
        val sym = classToScala(japplied.getRawType.asInstanceOf[jClass[_]])
        val pre = sym.owner.thisType
        val args0 = japplied.getActualTypeArguments
        val (args, bounds) = targsToScala(pre.typeSymbol, args0.toList)
        newExistentialType(bounds, typeRef(pre, sym, args))
      case jarr: GenericArrayType =>
        arrayType(typeToScala(jarr.getGenericComponentType))
      case jtvar: jTypeVariable[_] =>
        val tparam = typeParamToScala(jtvar)
        typeRef(NoPrefix, tparam, List())
    }

    /**
     * The Scala class that corresponds to given Java class without taking
     *  Scala pickling info into account.
     *  @param jclazz  The Java class
     *  @return A Scala class symbol that wraps all reflection info of `jclazz`
     */
    private def jclassAsScala(jclazz: jClass[_]): ClassSymbol =
      toScala(classCache, jclazz)(_ jclassAsScala1 _)

    private def jclassAsScala1(jclazz: jClass[_]): ClassSymbol = {
      val owner = sOwner(jclazz)
      val name = scalaSimpleName(jclazz)
      val completer = (clazz: Symbol, module: Symbol) => new FromJavaClassCompleter(clazz, module, jclazz)
      initAndEnterClassAndModule(owner, name, completer)._1
    }

    /**
     * The Scala field that corresponds to given Java field without taking
     *  Scala pickling info into account.
     *  @param jfield  The Java field
     *  @return A Scala value symbol that wraps all reflection info of `jfield`
     */
    private def jfieldAsScala(jfield: jField): TermSymbol =
      toScala(fieldCache, jfield)(_ jfieldAsScala1 _)

    private def jfieldAsScala1(jfield: jField): TermSymbol = {
      val field = sOwner(jfield)
          .newValue(newTermName(jfield.getName), NoPosition, toScalaFieldFlags(jfield.getModifiers))
          .setInfo(typeToScala(jfield.getGenericType))
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
    private def jmethodAsScala(jmeth: jMethod): MethodSymbol =
      toScala(methodCache, jmeth)(_ jmethodAsScala1 _)

    private def jmethodAsScala1(jmeth: jMethod): MethodSymbol = {
      val clazz = sOwner(jmeth)
      val meth = clazz.newMethod(newTermName(jmeth.getName), NoPosition, toScalaMethodFlags(jmeth.getModifiers))
      methodCache enter (jmeth, meth)
      val tparams = jmeth.getTypeParameters.toList map createTypeParameter
      val paramtpes = jmeth.getGenericParameterTypes.toList map typeToScala
      val resulttpe = typeToScala(jmeth.getGenericReturnType)
      setMethType(meth, tparams, paramtpes, resulttpe)
      copyAnnotations(meth, jmeth)
      if ((jmeth.getModifiers & JAVA_ACC_VARARGS) != 0) meth.setInfo(arrayToRepeated(meth.info))
      meth
    }

    /**
     * The Scala constructor that corresponds to given Java constructor without taking
     *  Scala pickling info into account.
     *  @param jconstr  The Java constructor
     *  @return A Scala constructor symbol that wraps all reflection info of `jconstr`
     */
    private def jconstrAsScala(jconstr: jConstructor[_]): MethodSymbol =
      toScala(constructorCache, jconstr)(_ jconstrAsScala1 _)

    private def jconstrAsScala1(jconstr: jConstructor[_]): MethodSymbol = {
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

// -------------------- Scala to Java  -----------------------------------

    /** Optionally, the Java package corresponding to a given Scala package, or None if no such Java package exists.
     *  @param   pkg The Scala package
     */
    def packageToJavaOption(pkg: ModuleSymbol): Option[jPackage] = packageCache.toJavaOption(pkg) {
      Option(jPackage.getPackage(pkg.fullName.toString))
    }

    /** The Java class corresponding to given Scala class.
     *  Note: This only works for
     *   - top-level classes
     *   - Scala classes that were generated via jclassToScala
     *   - classes that have a class owner that has a corresponding Java class
     *  @throws A `ClassNotFoundException` for all Scala classes not in one of these categories.
     */
    @throws(classOf[ClassNotFoundException])
    def classToJava(clazz: ClassSymbol): jClass[_] = classCache.toJava(clazz) {
      def noClass = throw new ClassNotFoundException("no Java class corresponding to "+clazz+" found")
      //println("classToJava "+clazz+" "+clazz.owner+" "+clazz.owner.isPackageClass)//debug
      if (clazz.isPrimitiveValueClass)
        valueClassToJavaType(clazz)
      else if (clazz == ArrayClass)
        noClass
      else if (clazz.owner.isPackageClass)
        javaClass(clazz.javaClassName)
      else if (clazz.owner.isClass) {
        val childOfClass = !clazz.owner.isModuleClass
        val childOfTopLevel = clazz.owner.owner.isPackageClass
        val childOfTopLevelObject = clazz.owner.isModuleClass && childOfTopLevel

        // suggested in https://issues.scala-lang.org/browse/SI-4023?focusedCommentId=54759#comment-54759
        var ownerClazz = classToJava(clazz.owner.asClass)
        if (childOfTopLevelObject) ownerClazz = Class.forName(ownerClazz.getName stripSuffix "$", true, ownerClazz.getClassLoader)
        val ownerChildren = ownerClazz.getDeclaredClasses

        var fullNameOfJavaClass = ownerClazz.getName
        if (childOfClass || childOfTopLevel) fullNameOfJavaClass += "$"
        fullNameOfJavaClass += clazz.name
        if (clazz.isModuleClass) fullNameOfJavaClass += "$"

        // println(s"ownerChildren = ${ownerChildren.toList}")
        // println(s"fullNameOfJavaClass = $fullNameOfJavaClass")
        ownerChildren.find(_.getName == fullNameOfJavaClass).getOrElse(noClass)
      } else
        noClass
    }

    private def expandedName(sym: Symbol): String =
      if (sym.isPrivate) nme.expandedName(sym.name.toTermName, sym.owner).toString
      else sym.name.toString

    /** The Java field corresponding to a given Scala field.
     *  @param   meth The Scala field.
     */
    def fieldToJava(fld: TermSymbol): jField = fieldCache.toJava(fld) {
      val jclazz = classToJava(fld.owner.asClass)
      val jname = nme.dropLocalSuffix(fld.name).toString
      try jclazz getDeclaredField jname
      catch {
        case ex: NoSuchFieldException => jclazz getDeclaredField expandedName(fld)
      }
    }

    /** The Java method corresponding to a given Scala method.
     *  @param   meth The Scala method
     */
    def methodToJava(meth: MethodSymbol): jMethod = methodCache.toJava(meth) {
      val jclazz = classToJava(meth.owner.asClass)
      val paramClasses = transformedType(meth).paramTypes map typeToJavaClass
      val jname = nme.dropLocalSuffix(meth.name).toString
      try jclazz getDeclaredMethod (jname, paramClasses: _*)
      catch {
        case ex: NoSuchMethodException =>
          jclazz getDeclaredMethod (expandedName(meth), paramClasses: _*)
      }
    }

    /** The Java constructor corresponding to a given Scala constructor.
     *  @param   constr The Scala constructor
     */
    def constructorToJava(constr: MethodSymbol): jConstructor[_] = constructorCache.toJava(constr) {
      val jclazz = classToJava(constr.owner.asClass)
      val paramClasses = transformedType(constr).paramTypes map typeToJavaClass
      val effectiveParamClasses =
        if (!constr.owner.owner.isStaticOwner) jclazz.getEnclosingClass +: paramClasses
        else paramClasses
      jclazz getConstructor (effectiveParamClasses: _*)
    }

    private def jArrayClass(elemClazz: jClass[_]): jClass[_] = {
      jArray.newInstance(elemClazz, 0).getClass
    }

    /** The Java class that corresponds to given Scala type.
     *  Pre: Scala type is already transformed to Java level.
     */
    def typeToJavaClass(tpe: Type): jClass[_] = tpe match {
      case ExistentialType(_, rtpe) => typeToJavaClass(rtpe)
      case TypeRef(_, ArrayClass, List(elemtpe)) => jArrayClass(typeToJavaClass(elemtpe))
      case TypeRef(_, sym: ClassSymbol, _) => classToJava(sym.asClass)
      case tpe @ TypeRef(_, sym: AliasTypeSymbol, _) => typeToJavaClass(tpe.dealias)
      case _ => throw new NoClassDefFoundError("no Java class corresponding to "+tpe+" found")
    }
  }

  /** Assert that packages have package scopes */
  override def validateClassInfo(tp: ClassInfoType) {
    assert(!tp.typeSymbol.isPackageClass || tp.decls.isInstanceOf[PackageScope])
  }

  override def newPackageScope(pkgClass: Symbol) = new PackageScope(pkgClass)

  override def scopeTransform(owner: Symbol)(op: => Scope): Scope =
    if (owner.isPackageClass) owner.info.decls else op

  override def mirrorThatLoaded(sym: Symbol): Mirror = sym.enclosingRootClass match {
    case root: RootSymbol => root.mirror
    case _ => abort(s"${sym}.enclosingRootClass = ${sym.enclosingRootClass}, which is not a RootSymbol")
  }

  private lazy val syntheticCoreClasses: Map[(String, Name), Symbol] = {
    def mapEntry(sym: Symbol): ((String, Name), Symbol) = (sym.owner.fullName, sym.name) -> sym
    Map() ++ (definitions.syntheticCoreClasses map mapEntry)
  }

  /** 1. If `owner` is a package class (but not the empty package) and `name` is a term name, make a new package
   *  <owner>.<name>, otherwise return NoSymbol.
   *  Exception: If owner is root and a java class with given name exists, create symbol in empty package instead
   *  2. If `owner` is the scala package and `name` designates a phantom class, return
   *     the corresponding class symbol and enter it into this mirror's ScalaPackage.
   */
  override def missingHook(owner: Symbol, name: Name): Symbol = {
    if (owner.hasPackageFlag) {
      val mirror = mirrorThatLoaded(owner)
      // todo. this makes toolbox tests pass, but it's a mere workaround for SI-5865
//      assert((owner.info decl name) == NoSymbol, s"already exists: $owner . $name")
      if (owner.isRootSymbol && mirror.tryJavaClass(name.toString).isDefined)
        return mirror.EmptyPackageClass.info decl name
      if (name.isTermName && !owner.isEmptyPackageClass)
        return mirror.makeScalaPackage(
          if (owner.isRootSymbol) name.toString else owner.fullName+"."+name)
      syntheticCoreClasses get (owner.fullName, name) match {
        case Some(tsym) =>
          // synthetic core classes are only present in root mirrors
          // because Definitions.scala, which initializes and enters them, only affects rootMirror
          // therefore we need to enter them manually for non-root mirrors
          if (mirror ne thisUniverse.rootMirror) owner.info.decls enter tsym
          return tsym
        case None =>
      }
    }
    info("*** missing: "+name+"/"+name.isTermName+"/"+owner+"/"+owner.hasPackageFlag+"/"+owner.info.decls.getClass)
    super.missingHook(owner, name)
  }
}

private[reflect] class ReflectError(msg: String) extends java.lang.Error(msg)

private[reflect] class HasJavaClass[J](val getClazz: J => java.lang.Class[_])
