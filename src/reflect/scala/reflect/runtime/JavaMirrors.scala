package scala
package reflect
package runtime

import scala.ref.WeakReference
import scala.collection.mutable.WeakHashMap

import java.lang.{Class => jClass, Package => jPackage}
import java.lang.reflect.{
  Method => jMethod, Constructor => jConstructor, Field => jField,
  Member => jMember, Type => jType, TypeVariable => jTypeVariable, Array => jArray,
  AccessibleObject => jAccessibleObject,
  GenericDeclaration, GenericArrayType, ParameterizedType, WildcardType, AnnotatedElement }
import java.lang.annotation.{Annotation => jAnnotation}
import java.io.IOException
import scala.reflect.internal.{ MissingRequirementError, JavaAccFlags, JMethodOrConstructor }
import internal.pickling.ByteCodecs
import internal.pickling.UnPickler
import scala.collection.mutable.{ HashMap, ListBuffer, ArrayBuffer }
import internal.Flags._
import ReflectionUtils._
import scala.language.existentials
import scala.runtime.{ScalaRunTime, BoxesRunTime}

private[scala] trait JavaMirrors extends internal.SymbolTable with api.JavaUniverse with TwoWayCaches { thisUniverse: SymbolTable =>

  private lazy val mirrors = new WeakHashMap[ClassLoader, WeakReference[JavaMirror]]()

  private def createMirror(owner: Symbol, cl: ClassLoader): Mirror = {
    val jm = new JavaMirror(owner, cl)
    mirrors(cl) = new WeakReference(jm)
    jm.init()
    jm
  }

  override type Mirror = JavaMirror
  implicit val MirrorTag: ClassTag[Mirror] = ClassTag[Mirror](classOf[JavaMirror])

  override lazy val rootMirror: Mirror = createMirror(NoSymbol, rootClassLoader)

  // overridden by ReflectGlobal
  def rootClassLoader: ClassLoader = this.getClass.getClassLoader

  trait JavaClassCompleter

  def runtimeMirror(cl: ClassLoader): Mirror = gilSynchronized {
    mirrors get cl match {
      case Some(WeakReference(m)) => m
      case _ => createMirror(rootMirror.RootClass, cl)
    }
  }

  /** The API of a mirror for a reflective universe */
  class JavaMirror(owner: Symbol,
    /* Class loader that is a mastermind behind the reflexive mirror */
    val classLoader: ClassLoader
  ) extends Roots(owner) with super.JavaMirror { thisMirror =>

    val universe: thisUniverse.type = thisUniverse

    import definitions._
    private[reflect] lazy val runDefinitions = new definitions.RunDefinitions // only one "run" in the reflection universe
    import runDefinitions._

    override lazy val RootPackage = (new RootPackage with SynchronizedTermSymbol).markFlagsCompleted(mask = AllFlags)
    override lazy val RootClass = (new RootClass with SynchronizedModuleClassSymbol).markFlagsCompleted(mask = AllFlags)
    override lazy val EmptyPackage = (new EmptyPackage with SynchronizedTermSymbol).markFlagsCompleted(mask = AllFlags)
    override lazy val EmptyPackageClass = (new EmptyPackageClass with SynchronizedModuleClassSymbol).markFlagsCompleted(mask = AllFlags)

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
      catch { case _: ScalaReflectionException => makeScalaPackage(fullname) }

// ----------- Caching ------------------------------------------------------------------

    private val classCache       = new TwoWayCache[jClass[_], ClassSymbol]
    private val packageCache     = new TwoWayCache[Package, ModuleSymbol]
    private val methodCache      = new TwoWayCache[jMethod, MethodSymbol]
    private val constructorCache = new TwoWayCache[jConstructor[_], MethodSymbol]
    private val fieldCache       = new TwoWayCache[jField, TermSymbol]
    private val tparamCache      = new TwoWayCache[jTypeVariable[_ <: GenericDeclaration], TypeSymbol]

    private[runtime] def toScala[J: HasJavaClass, S](cache: TwoWayCache[J, S], key: J)(body: (JavaMirror, J) => S): S =
      cache.toScala(key){
        val jclazz = implicitly[HasJavaClass[J]] getClazz key
        body(mirrorDefining(jclazz), key)
      }

    private implicit val classHasJavaClass: HasJavaClass[jClass[_]]        = new HasJavaClass(identity)
    private implicit val methHasJavaClass: HasJavaClass[jMethod]           = new HasJavaClass(_.getDeclaringClass)
    private implicit val fieldHasJavaClass: HasJavaClass[jField]           = new HasJavaClass(_.getDeclaringClass)
    private implicit val constrHasJavaClass: HasJavaClass[jConstructor[_]] = new HasJavaClass(_.getDeclaringClass)
    private implicit val tparamHasJavaClass: HasJavaClass[jTypeVariable[_ <: GenericDeclaration]] =
      new HasJavaClass ( (tparam: jTypeVariable[_ <: GenericDeclaration]) => {
        tparam.getGenericDeclaration match {
          case jclazz: jClass[_]        => jclazz
          case jmeth: jMethod           => jmeth.getDeclaringClass
          case jconstr: jConstructor[_] => jconstr.getDeclaringClass
        }
      })

// ----------- Implementations of mirror operations and classes  -------------------

    private def abort(msg: String) = throw new ScalaReflectionException(msg)

    private def ErrorInnerClass(sym: Symbol)                      = abort(s"$sym is an inner class, use reflectClass on an InstanceMirror to obtain its ClassMirror")
    private def ErrorInnerModule(sym: Symbol)                     = abort(s"$sym is an inner module, use reflectModule on an InstanceMirror to obtain its ModuleMirror")
    private def ErrorStaticClass(sym: Symbol)                     = abort(s"$sym is a static class, use reflectClass on a RuntimeMirror to obtain its ClassMirror")
    private def ErrorStaticModule(sym: Symbol)                    = abort(s"$sym is a static module, use reflectModule on a RuntimeMirror to obtain its ModuleMirror")
    private def ErrorNotMember(sym: Symbol, owner: Symbol)        = abort(s"expected a member of $owner, you provided ${sym.kindString} ${sym.fullName}")
    private def ErrorNotField(sym: Symbol)                        = abort(s"expected a field or an accessor method symbol, you provided $sym")
    private def ErrorNotConstructor(sym: Symbol, owner: Symbol)   = abort(s"expected a constructor of $owner, you provided $sym")
    private def ErrorArrayConstructor(sym: Symbol, owner: Symbol) = abort(s"Cannot instantiate arrays with mirrors. Consider using `scala.reflect.ClassTag(<class of element>).newArray(<length>)` instead")
    private def ErrorFree(member: Symbol, freeType: Symbol)       = abort(s"cannot reflect ${member.kindString} ${member.name}, because it's a member of a weak type ${freeType.name}")
    private def ErrorNonExistentField(sym: Symbol)                = abort(
      sm"""Scala field ${sym.name} of ${sym.owner} isn't represented as a Java field, nor does it have a
          |Java accessor method. One common reason for this is that it may be a private class parameter
          |not used outside the primary constructor.""")

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
          staticPartOfEnum.info.declaration(TermName(enum.name))
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
          TermName(m.getName) -> toAnnotArg(m.getReturnType -> m.invoke(jann))
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
      if (owner == ArrayClass) ErrorArrayConstructor(sym, owner)
      ensuringNotFree(sym) {
        if (!owner.info.decls.toList.contains(sym)) ErrorNotConstructor(sym, owner)
      }
    }

    private def preciseClass[T: ClassTag](instance: T) = {
      val staticClazz = classTag[T].runtimeClass
      val dynamicClazz = instance.getClass
      if (staticClazz.isPrimitive) staticClazz else dynamicClazz
    }

    private class JavaInstanceMirror[T: ClassTag](val instance: T) extends InstanceMirror {
      def symbol = thisMirror.classSymbol(preciseClass(instance))
      def reflectField(field: TermSymbol): FieldMirror = {
        checkMemberOf(field, symbol)
        if ((field.isMethod && !field.isAccessor) || field.isModule) ErrorNotField(field)
        val name = if (field.isAccessor) field.localName else field.name
        val field1 = (field.owner.info decl name).asTerm
        try fieldToJava(field1)
        catch {
          case _: NoSuchFieldException => ErrorNonExistentField(field1)
        }
        new JavaFieldMirror(instance, field1)
      }
      def reflectMethod(method: MethodSymbol): MethodMirror = {
        checkMemberOf(method, symbol)
        mkMethodMirror(instance, method)
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

    // caches value class metadata, so that we minimize the work that needs to be done during Mirror.apply
    private class DerivedValueClassMetadata(info: Type) {
      val symbol = info.typeSymbol
      val isDerivedValueClass = symbol.isDerivedValueClass
      lazy val boxer = runtimeClass(symbol.toType).getDeclaredConstructors().head
      lazy val unboxer = {
        val fields @ (field :: _) = symbol.toType.decls.collect{ case ts: TermSymbol if ts.isParamAccessor && ts.isMethod => ts }.toList
        assert(fields.length == 1, s"$symbol: $fields")
        runtimeClass(symbol.asClass).getDeclaredMethod(field.name.toString)
      }
    }

    private class JavaFieldMirror(val receiver: Any, val symbol: TermSymbol, metadata: DerivedValueClassMetadata)
            extends FieldMirror {
      def this(receiver: Any, symbol: TermSymbol) = this(receiver, symbol, new DerivedValueClassMetadata(symbol.info))
      def bind(newReceiver: Any) = new JavaFieldMirror(newReceiver, symbol, metadata)
      import metadata._

      lazy val jfield = ensureAccessible(fieldToJava(symbol))
      def get = {
        val value = jfield get receiver
        if (isDerivedValueClass) boxer.newInstance(value) else value
      }
      def set(value: Any) = {
        // it appears useful to be able to set values of vals, therefore I'm disabling this check
        // if (!symbol.isMutable) ErrorSetImmutableField(symbol)
        jfield.set(receiver, if (isDerivedValueClass) unboxer.invoke(value) else value)
      }

      override def toString = s"field mirror for ${showDecl(symbol)} (bound to $receiver)"
    }

    // the "symbol == Any_getClass || symbol == Object_getClass" test doesn't cut it
    // because both AnyVal and its primitive descendants define their own getClass methods
    private def isGetClass(meth: MethodSymbol) = (meth.name string_== "getClass") && meth.paramss.flatten.isEmpty
    private def isStringConcat(meth: MethodSymbol) = meth == String_+ || (meth.owner.isPrimitiveValueClass && meth.returnType =:= StringClass.toType)
    lazy val bytecodelessMethodOwners = Set[Symbol](AnyClass, AnyValClass, AnyRefClass, ObjectClass, ArrayClass) ++ ScalaPrimitiveValueClasses
    lazy val bytecodefulObjectMethods = Set[Symbol](Object_clone, Object_equals, Object_finalize, Object_hashCode, Object_toString,
                                        Object_notify, Object_notifyAll) ++ ObjectClass.info.member(nme.wait_).asTerm.alternatives.map(_.asMethod)
    private def isBytecodelessMethod(meth: MethodSymbol): Boolean = {
      if (isGetClass(meth) || isStringConcat(meth) || meth.owner.isPrimitiveValueClass || meth == runDefinitions.Predef_classOf || meth.isMacro) return true
      bytecodelessMethodOwners(meth.owner) && !bytecodefulObjectMethods(meth)
    }

    private def isByNameParam(p: Type) = isByNameParamType(p)
    private def isValueClassParam(p: Type) = p.typeSymbol.isDerivedValueClass

    // unlike other mirrors, method mirrors are created by a factory
    // that's because we want to have decent performance
    // therefore we move special cases into separate subclasses
    // rather than have them on a hot path them in a unified implementation of the `apply` method
    private def mkMethodMirror[T: ClassTag](receiver: T, symbol: MethodSymbol): MethodMirror = {
      def existsParam(pred: Type => Boolean) = symbol.paramss.flatten.map(_.info).exists(pred)
      if (isBytecodelessMethod(symbol)) new BytecodelessMethodMirror(receiver, symbol)
      else if (existsParam(isByNameParam) || existsParam(isValueClassParam)) new JavaTransformingMethodMirror(receiver, symbol)
      else {
        symbol.paramss.flatten.length match {
          case 0 => new JavaVanillaMethodMirror0(receiver, symbol)
          case 1 => new JavaVanillaMethodMirror1(receiver, symbol)
          case 2 => new JavaVanillaMethodMirror2(receiver, symbol)
          case 3 => new JavaVanillaMethodMirror3(receiver, symbol)
          case 4 => new JavaVanillaMethodMirror4(receiver, symbol)
          case _ => new JavaVanillaMethodMirror(receiver, symbol)
        }
      }
    }

    private abstract class JavaMethodMirror(val symbol: MethodSymbol, protected val ret: DerivedValueClassMetadata) extends MethodMirror {
      lazy val jmeth = ensureAccessible(methodToJava(symbol))
      lazy val jconstr = ensureAccessible(constructorToJava(symbol))

      def jinvokeraw(args: Seq[Any]) =
        if (!symbol.isConstructor) jmeth.invoke(receiver, args.asInstanceOf[Seq[AnyRef]]: _*)
        else if (receiver == null) jconstr.newInstance(args.asInstanceOf[Seq[AnyRef]]: _*)
        else jconstr.newInstance((receiver +: args).asInstanceOf[Seq[AnyRef]]: _*)
      def jinvoke(args: Seq[Any]): Any = {
        val result = jinvokeraw(args)
        if (!symbol.isConstructor && jmeth.getReturnType == java.lang.Void.TYPE) ()
        else if (!symbol.isConstructor && ret.isDerivedValueClass) ret.boxer.newInstance(result.asInstanceOf[AnyRef])
        else result
      }

      override def toString = {
        val what = if (symbol.isConstructor) "constructor mirror" else "method mirror"
        s"$what for ${showDecl(symbol)} (bound to $receiver)"
      }
    }

    private class JavaVanillaMethodMirror(val receiver: Any, symbol: MethodSymbol, ret: DerivedValueClassMetadata)
            extends JavaMethodMirror(symbol, ret) {
      def this(receiver: Any, symbol: MethodSymbol) = this(receiver, symbol, new DerivedValueClassMetadata(symbol.returnType))
      def bind(newReceiver: Any) = new JavaVanillaMethodMirror(newReceiver, symbol, ret)
      def apply(args: Any*): Any = jinvoke(args)
    }

    private class JavaVanillaMethodMirror0(receiver: Any, symbol: MethodSymbol, ret: DerivedValueClassMetadata)
            extends JavaVanillaMethodMirror(receiver, symbol, ret) {
      def this(receiver: Any, symbol: MethodSymbol) = this(receiver, symbol, new DerivedValueClassMetadata(symbol.returnType))
      override def bind(newReceiver: Any) = new JavaVanillaMethodMirror0(newReceiver, symbol, ret)
      override def jinvokeraw(args: Seq[Any]) =
        if (!symbol.isConstructor) jmeth.invoke(receiver)
        else if (receiver == null) jconstr.newInstance()
        else jconstr.newInstance(receiver.asInstanceOf[AnyRef])
    }

    private class JavaVanillaMethodMirror1(receiver: Any, symbol: MethodSymbol, ret: DerivedValueClassMetadata)
            extends JavaVanillaMethodMirror(receiver, symbol, ret) {
      def this(receiver: Any, symbol: MethodSymbol) = this(receiver, symbol, new DerivedValueClassMetadata(symbol.returnType))
      override def bind(newReceiver: Any) = new JavaVanillaMethodMirror1(newReceiver, symbol, ret)
      override def jinvokeraw(args: Seq[Any]) =
        if (!symbol.isConstructor) jmeth.invoke(receiver, args(0).asInstanceOf[AnyRef])
        else if (receiver == null) jconstr.newInstance(args(0).asInstanceOf[AnyRef])
        else jconstr.newInstance(receiver.asInstanceOf[AnyRef], args(0).asInstanceOf[AnyRef])
    }

    private class JavaVanillaMethodMirror2(receiver: Any, symbol: MethodSymbol, ret: DerivedValueClassMetadata)
            extends JavaVanillaMethodMirror(receiver, symbol, ret) {
      def this(receiver: Any, symbol: MethodSymbol) = this(receiver, symbol, new DerivedValueClassMetadata(symbol.returnType))
      override def bind(newReceiver: Any) = new JavaVanillaMethodMirror2(newReceiver, symbol, ret)
      override def jinvokeraw(args: Seq[Any]) =
        if (!symbol.isConstructor) jmeth.invoke(receiver, args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef])
        else if (receiver == null) jconstr.newInstance(args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef])
        else jconstr.newInstance(receiver.asInstanceOf[AnyRef], args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef])
    }

    private class JavaVanillaMethodMirror3(receiver: Any, symbol: MethodSymbol, ret: DerivedValueClassMetadata)
            extends JavaVanillaMethodMirror(receiver, symbol, ret) {
      def this(receiver: Any, symbol: MethodSymbol) = this(receiver, symbol, new DerivedValueClassMetadata(symbol.returnType))
      override def bind(newReceiver: Any) = new JavaVanillaMethodMirror3(newReceiver, symbol, ret)
      override def jinvokeraw(args: Seq[Any]) =
        if (!symbol.isConstructor) jmeth.invoke(receiver, args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef], args(2).asInstanceOf[AnyRef])
        else if (receiver == null) jconstr.newInstance(args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef], args(2).asInstanceOf[AnyRef])
        else jconstr.newInstance(receiver.asInstanceOf[AnyRef], args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef], args(2).asInstanceOf[AnyRef])
    }

    private class JavaVanillaMethodMirror4(receiver: Any, symbol: MethodSymbol, ret: DerivedValueClassMetadata)
            extends JavaVanillaMethodMirror(receiver, symbol, ret) {
      def this(receiver: Any, symbol: MethodSymbol) = this(receiver, symbol, new DerivedValueClassMetadata(symbol.returnType))
      override def bind(newReceiver: Any) = new JavaVanillaMethodMirror4(newReceiver, symbol, ret)
      override def jinvokeraw(args: Seq[Any]) =
        if (!symbol.isConstructor) jmeth.invoke(receiver, args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef], args(2).asInstanceOf[AnyRef], args(3).asInstanceOf[AnyRef])
        else if (receiver == null) jconstr.newInstance(args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef], args(2).asInstanceOf[AnyRef], args(3).asInstanceOf[AnyRef])
        else jconstr.newInstance(receiver.asInstanceOf[AnyRef], args(0).asInstanceOf[AnyRef], args(1).asInstanceOf[AnyRef], args(2).asInstanceOf[AnyRef], args(3).asInstanceOf[AnyRef])
    }

    // caches MethodSymbol metadata, so that we minimize the work that needs to be done during Mirror.apply
    // TODO: vararg is only supported in the last parameter list (SI-6182), so we don't need to worry about the rest for now
    private class MethodMetadata(symbol: MethodSymbol) {
      private val params = symbol.paramss.flatten.toArray
      private val vcMetadata = params.map(p => new DerivedValueClassMetadata(p.info))
      val isByName = params.map(p => isByNameParam(p.info))
      def isDerivedValueClass(i: Int) = vcMetadata(i).isDerivedValueClass
      def paramUnboxers(i: Int) = vcMetadata(i).unboxer
      val paramCount = params.length
      val ret = new DerivedValueClassMetadata(symbol.returnType)
    }

    private class JavaTransformingMethodMirror(val receiver: Any, symbol: MethodSymbol, metadata: MethodMetadata)
            extends JavaMethodMirror(symbol, metadata.ret) {
      def this(receiver: Any, symbol: MethodSymbol) = this(receiver, symbol, new MethodMetadata(symbol))
      override def bind(newReceiver: Any) = new JavaTransformingMethodMirror(newReceiver, symbol, metadata)
      import metadata._

      def apply(args: Any*): Any = {
        val args1 = new Array[Any](args.length)
        var i = 0
        while (i < args1.length) {
          val arg = args(i)
          args1(i) = (
            if (i >= paramCount)             arg                           // don't transform varargs
            else if (isByName(i))            () => arg                     // don't transform by-name value class params
            else if (isDerivedValueClass(i)) paramUnboxers(i).invoke(arg)  // do get the underlying value
            else                             arg                           // don't molest anything else
          )
          i += 1
        }
        jinvoke(args1)
      }
    }

    private class BytecodelessMethodMirror[T: ClassTag](val receiver: T, val symbol: MethodSymbol)
            extends MethodMirror {
      def bind(newReceiver: Any) = new BytecodelessMethodMirror(newReceiver.asInstanceOf[T], symbol)
      override def toString = s"bytecodeless method mirror for ${showDecl(symbol)} (bound to $receiver)"

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
          val s_arguments = if (params.length == 1 && !isVarArgsList(params)) "argument" else "arguments"
          abort(s"${showDecl(symbol)} takes $n_arguments $s_arguments")
        }

        def objReceiver       = receiver.asInstanceOf[AnyRef]
        def objArg0           = args(0).asInstanceOf[AnyRef]
        def objArgs           = args.asInstanceOf[Seq[AnyRef]]
        def fail(msg: String) = abort(msg + ", it cannot be invoked with mirrors")

        def invokePrimitiveMethod = {
          val jmeths = classOf[BoxesRunTime].getDeclaredMethods.filter(_.getName == nme.primitiveMethodName(symbol.name).toString)
          assert(jmeths.length == 1, jmeths.toList)
          val jmeth = jmeths.head
          val result = jmeth.invoke(null, (objReceiver +: objArgs).asInstanceOf[Seq[AnyRef]]: _*)
          if (jmeth.getReturnType == java.lang.Void.TYPE) ()
          else result
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
          case Object_asInstanceOf                    => fail("AnyRef.%s is an internal method" format symbol.name)
          case Object_isInstanceOf                    => fail("AnyRef.%s is an internal method" format symbol.name)
          case Array_length                           => ScalaRunTime.array_length(objReceiver)
          case Array_apply                            => ScalaRunTime.array_apply(objReceiver, args(0).asInstanceOf[Int])
          case Array_update                           => ScalaRunTime.array_update(objReceiver, args(0).asInstanceOf[Int], args(1))
          case Array_clone                            => ScalaRunTime.array_clone(objReceiver)
          case sym if isStringConcat(sym)             => receiver.toString + objArg0
          case sym if sym.owner.isPrimitiveValueClass => invokePrimitiveMethod
          case sym if sym == Predef_classOf           => fail("Predef.classOf is a compile-time function")
          case sym if sym.isMacro                     => fail(s"${symbol.fullName} is a macro, i.e. a compile-time function")
          case _                                      => abort(s"unsupported symbol $symbol when invoking $this")
        }
      }
    }

    private abstract class JavaTemplateMirror
            extends TemplateMirror {
      def outer: AnyRef
      def erasure: ClassSymbol
    }

    private class JavaClassMirror(val outer: AnyRef, val symbol: ClassSymbol)
            extends JavaTemplateMirror with ClassMirror {
      def erasure = symbol
      def isStatic = false
      def reflectConstructor(constructor: MethodSymbol) = {
        checkConstructorOf(constructor, symbol)
        mkMethodMirror(outer, constructor)
      }
      override def toString = s"class mirror for ${symbol.fullName} (bound to $outer)"
    }

    private class JavaModuleMirror(val outer: AnyRef, val symbol: ModuleSymbol)
            extends JavaTemplateMirror with ModuleMirror {
      def erasure = symbol.moduleClass.asClass
      def isStatic = true
      def instance = {
        if (symbol.isTopLevel)
          staticSingletonInstance(classLoader, symbol.fullName)
        else
          if (outer == null) staticSingletonInstance(classToJava(symbol.moduleClass.asClass))
          else innerSingletonInstance(outer, symbol.name.toString)
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
      jClass.forName(path, true, classLoader)

    /** Does `path` correspond to a Java class with that fully qualified name in the current class loader? */
    def tryJavaClass(path: String): Option[jClass[_]] = (
      try Some(javaClass(path))
      catch { case ex @ (_: LinkageError | _: ClassNotFoundException) => None } // TODO - log
    )

    /** The mirror that corresponds to the classloader that original defined the given Java class */
    def mirrorDefining(jclazz: jClass[_]): JavaMirror = {
      val cl = jclazz.getClassLoader
      if (cl == this.classLoader) this else runtimeMirror(cl)
    }

    private object unpickler extends UnPickler {
      val symbolTable: thisUniverse.type = thisUniverse
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
        if (settings.debug) ex.printStackTrace()
        val msg = ex.getMessage()
        MissingRequirementError.signal(
          (if (msg eq null) "reflection error while loading " + clazz.name
           else "error while loading " + clazz.name) + ", " + msg)
      }
      // don't use classOf[scala.reflect.ScalaSignature] here, because it will use getClass.getClassLoader, not mirror's classLoader
      // don't use asInstanceOf either because of the same reason (lol, I cannot believe I fell for it)
      // don't use structural types to simplify reflective invocations because of the same reason
      // TODO SI-9296 duplicated code, refactor
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
            assignAssociatedFile(clazz, module, jclazz)
            unpickler.unpickle(bytes take len, 0, clazz, module, jclazz.getName)
            markAllCompleted(clazz, module)
          case None =>
            loadBytes[Array[String]]("scala.reflect.ScalaLongSignature") match {
              case Some(slsig) =>
                info(s"unpickling Scala $clazz and $module with long Scala signature")
                val encoded = slsig flatMap (_.getBytes)
                val len = ByteCodecs.decode(encoded)
                val decoded = encoded.take(len)
                assignAssociatedFile(clazz, module, jclazz)
                unpickler.unpickle(decoded, 0, clazz, module, jclazz.getName)
                markAllCompleted(clazz, module)
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
      markFlagsCompleted(tparam)(mask = AllFlags)
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
        markAllCompleted(sym)
      }
    }

    private def assignAssociatedFile(clazz: Symbol, module: Symbol, jclazz: jClass[_]): Unit = {
      val associatedFile = ReflectionUtils.associatedFile(jclazz)
      clazz.associatedFile = associatedFile
      if (module != NoSymbol) module.associatedFile = associatedFile
    }

    /**
     * Copy all annotations of Java annotated element `jann` over to Scala symbol `sym`.
     * Also creates `@throws` annotations if necessary.
     *  Pre: `sym` is already initialized with a concrete type.
     *  Note: If `sym` is a method or constructor, its parameter annotations are copied as well.
     */
    private def copyAnnotations(sym: Symbol, jann: AnnotatedElement) {
      sym setAnnotations (jann.getAnnotations map JavaAnnotationProxy).toList
      // SI-7065: we're not using getGenericExceptionTypes here to be consistent with ClassfileParser
      val jexTpes = jann match {
        case jm: jMethod              => jm.getExceptionTypes.toList
        case jconstr: jConstructor[_] => jconstr.getExceptionTypes.toList
        case _                        => Nil
      }
      jexTpes foreach (jexTpe => sym.addThrowsAnnotation(classSymbol(jexTpe)))
    }

    private implicit class jClassOps(val clazz: jClass[_]) {
      def javaFlags: JavaAccFlags = JavaAccFlags(clazz)
      def scalaFlags: Long        = javaFlags.toScalaFlags
    }
    private implicit class jMemberOps(val member: jMember) {
      def javaFlags: JavaAccFlags = JavaAccFlags(member)
      def scalaFlags: Long        = javaFlags.toScalaFlags
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
    private class FromJavaClassCompleter(clazz: Symbol, module: Symbol, jclazz: jClass[_]) extends LazyType with JavaClassCompleter with FlagAgnosticCompleter {
      // one doesn't need to do non-trivial computations to assign flags for Java-based reflection artifacts
      // therefore I'm moving flag-assigning logic from completion to construction
      val flags = jclazz.scalaFlags
      clazz setFlag (flags | JAVA)
      if (module != NoSymbol) {
        module setFlag (flags & PRIVATE | JAVA)
        module.moduleClass setFlag (flags & PRIVATE | JAVA)
      }
      markFlagsCompleted(clazz, module)(mask = AllFlags)

      /** used to avoid cycles while initializing classes */
      private var parentsLevel = 0
      private var pendingLoadActions: List[() => Unit] = Nil
      private val relatedSymbols = clazz +: (if (module != NoSymbol) List(module, module.moduleClass) else Nil)

      override def load(sym: Symbol): Unit = {
        debugInfo("completing from Java " + sym + "/" + clazz.fullName)//debug
        assert(sym == clazz || (module != NoSymbol && (sym == module || sym == module.moduleClass)), sym)

        assignAssociatedFile(clazz, module, jclazz)
        propagatePackageBoundary(jclazz, relatedSymbols: _*)
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
        markAllCompleted(clazz, module)
      }

      def completeRest(): Unit = gilSynchronized {
        val tparams = clazz.rawInfo.typeParams

        val parents = try {
          parentsLevel += 1
          val jsuperclazz = jclazz.getGenericSuperclass
          val ifaces = jclazz.getGenericInterfaces.toList map typeToScala
          val isAnnotation = JavaAccFlags(jclazz).isAnnotation
          if (isAnnotation) AnnotationClass.tpe :: ClassfileAnnotationClass.tpe :: ifaces
          else if (jclazz.isInterface) ObjectTpe :: ifaces // interfaces have Object as superclass in the classfile (see jvm spec), but getGenericSuperclass seems to return null
          else (if (jsuperclazz == null) AnyTpe else typeToScala(jsuperclazz)) :: ifaces
        } finally {
          parentsLevel -= 1
        }
        clazz setInfo GenPolyType(tparams, new ClassInfoType(parents, newScope, clazz))
        if (module != NoSymbol) {
          module.moduleClass setInfo new ClassInfoType(List(), newScope, module.moduleClass)
        }

        def enter(sym: Symbol, mods: JavaAccFlags) = followStatic(clazz, module, mods).info.decls enter sym

        def enterEmptyCtorIfNecessary(): Unit = {
          if (jclazz.getConstructors.isEmpty)
            clazz.info.decls.enter(clazz.newClassConstructor(NoPosition))
        }

        for (jinner <- jclazz.getDeclaredClasses) {
          jclassAsScala(jinner) // inner class is entered as a side-effect
                                // no need to call enter explicitly
        }

        pendingLoadActions ::= { () =>
          jclazz.getDeclaredFields  foreach (f => enter(jfieldAsScala(f),  f.javaFlags))
          jclazz.getDeclaredMethods foreach (m => enter(jmethodAsScala(m), m.javaFlags))
          jclazz.getConstructors    foreach (c => enter(jconstrAsScala(c), c.javaFlags))
          enterEmptyCtorIfNecessary()
        }

        if (parentsLevel == 0) {
          while (pendingLoadActions.nonEmpty) {
            val item = pendingLoadActions.head
            pendingLoadActions = pendingLoadActions.tail
            item()
          }
        }
      }

      class LazyPolyType(override val typeParams: List[Symbol]) extends LazyType with FlagAgnosticCompleter {
        override def complete(sym: Symbol) {
          completeRest()
          markAllCompleted(clazz, module)
        }
      }
    }

    /**
     * If Java modifiers `mods` contain STATIC, return the module class
     *  of the companion module of `clazz`, otherwise the class `clazz` itself.
     */
    private def followStatic(clazz: Symbol, mods: JavaAccFlags): Symbol = followStatic(clazz, clazz.companionModule, mods)

    private def followStatic(clazz: Symbol, module: Symbol, mods: JavaAccFlags): Symbol =
      // SI-8196 `orElse(clazz)` needed for implementation details of the backend, such as the static
      //         field containing the cache for structural calls.
      if (mods.isStatic) module.moduleClass.orElse(clazz) else clazz

  /**
   * Certain method of the Java reflection api cannot be used on classfiles created by Scala.
   * See the comment in test/files/jvm/javaReflection/Test.scala. The methods are
   *
   *    public String getSimpleName()
   *    public boolean isAnonymousClass()
   *    public boolean isLocalClass()
   *    public String getCanonicalName()
   *    public boolean isSynthetic()
   *
   *  TODO - find all such calls and wrap them.
   *  TODO - create mechanism to avoid the recurrence of unwrapped calls.
   */
   implicit class RichClass(jclazz: jClass[_]) {
      // As explained in the javaReflection test, Class.isLocalClass is true for all non-member
      // nested classes in Scala. This is fine per se, however the implementation may throw an
      // InternalError. We therefore re-implement it here.
      // TODO: this method should be renamed to `isLocalOrAnonymousClass`.
      // due to bin compat that's only possible in 2.12, we cannot introduce a new alias in 2.11.
      def isLocalClass0: Boolean = jclazz.getEnclosingClass != null && !jclazz.isMemberClass
    }

    /**
     * The Scala owner of the Scala class corresponding to the Java class `jclazz`
     */
    // @eb: a weird classloader might return a null package for something with a non-empty package name
    // for example, http://groups.google.com/group/scala-internals/browse_thread/thread/7be09ff8f67a1e5c
    // in that case we could invoke packageNameToScala(jPackageName) and, probably, be okay
    // however, I think, it's better to blow up, since weirdness of the class loader might bite us elsewhere
    // [martin] I think it's better to be forgiving here. Restoring packageNameToScala.
    private def sOwner(jclazz: jClass[_]): Symbol = jclazz match {
      case PrimitiveOrArray()            => ScalaPackageClass
      case EnclosedInMethod(jowner)      => methodToScala(jowner)
      case EnclosedInConstructor(jowner) => constructorToScala(jowner)
      case EnclosedInClass(jowner)       => followStatic(classToScala(jowner), jclazz.javaFlags)
      case EnclosedInPackage(jowner)     => packageToScala(jowner).moduleClass
      case _                             => packageNameToScala(jclazz.getName take jclazz.getName.lastIndexOf('.')).moduleClass
    }

    /**
     * The Scala owner of the Scala symbol corresponding to the Java member `jmember`
     */
    private def sOwner(jmember: jMember): Symbol = {
      followStatic(classToScala(jmember.getDeclaringClass), jmember.javaFlags)
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
      def approximateMatch(sym: Symbol, jstr: String): Boolean = (
           (sym.name string_== jstr)
        || sym.isPrivate && (nme.expandedName(sym.name.toTermName, sym.owner) string_== jstr)
      )

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
      val owner = followStatic(preOwner, jmeth.javaFlags)
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
      val owner = followStatic(classToScala(jconstr.getDeclaringClass), jconstr.javaFlags)
      (lookup(owner, jconstr.getName) suchThat (erasesTo(_, jconstr)) orElse jconstrAsScala(jconstr))
        .asMethod
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
    private[JavaMirrors] def makeScalaPackage(fullname: String): ModuleSymbol = gilSynchronized {
      val split = fullname lastIndexOf '.'
      val ownerModule: ModuleSymbol =
        if (split > 0) packageNameToScala(fullname take split) else this.RootPackage
      val owner = ownerModule.moduleClass
      val name = TermName(fullname) drop split + 1
      val opkg = owner.info decl name
      if (opkg.hasPackageFlag)
        opkg.asModule
      else if (opkg == NoSymbol) {
        val pkg = owner.newPackage(name)
        pkg.moduleClass setInfo new LazyPackageType
        pkg setInfoAndEnter pkg.moduleClass.tpe
        markFlagsCompleted(pkg)(mask = AllFlags)
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
            coreLookup(simpleName.dropModule.toTermName) map (_.moduleClass)
          else
            coreLookup(simpleName)
        }

        val cls =
          if (jclazz.isMemberClass && !nme.isImplClassName(jname))
            lookupClass
          else if (jclazz.isLocalClass0 || scalacShouldntLoadClass(jname))
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
          (if (cls != NoSymbol) s"not a type: symbol $cls" else "no symbol could be") +
          s" loaded from $jclazz in $owner with name $simpleName and classloader $classLoader")

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
        case PolyType(tparams, _) => tparams.find(_.name string_== jparam.getName).get.asType
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
    def reflectMemberToScala(m: jMember): Symbol = m match {
      case x: GenericDeclaration => genericDeclarationToScala(x)
      case x: jField             => jfieldAsScala(x)
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
          .newValue(newTermName(jfield.getName), NoPosition, jfield.scalaFlags)
          .setInfo(typeToScala(jfield.getGenericType))

      fieldCache.enter(jfield, field)
      propagatePackageBoundary(jfield, field)
      copyAnnotations(field, jfield)
      markAllCompleted(field)
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
      val meth = clazz.newMethod(newTermName(jmeth.getName), NoPosition, jmeth.scalaFlags)
      methodCache enter (jmeth, meth)
      val tparams = jmeth.getTypeParameters.toList map createTypeParameter
      val paramtpes = jmeth.getGenericParameterTypes.toList map typeToScala
      val resulttpe = typeToScala(jmeth.getGenericReturnType)
      setMethType(meth, tparams, paramtpes, resulttpe)
      propagatePackageBoundary(jmeth.javaFlags, meth)
      copyAnnotations(meth, jmeth)
      if (jmeth.javaFlags.isVarargs) meth modifyInfo arrayToRepeated
      markAllCompleted(meth)
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
      val constr = clazz.newConstructor(NoPosition, jconstr.scalaFlags)
      constructorCache enter (jconstr, constr)
      val tparams = jconstr.getTypeParameters.toList map createTypeParameter
      val paramtpes = jconstr.getGenericParameterTypes.toList map typeToScala
      setMethType(constr, tparams, paramtpes, clazz.tpe_*)
      constr setInfo GenPolyType(tparams, MethodType(clazz.newSyntheticValueParams(paramtpes), clazz.tpe))
      propagatePackageBoundary(jconstr.javaFlags, constr)
      copyAnnotations(constr, jconstr)
      if (jconstr.javaFlags.isVarargs) constr modifyInfo arrayToRepeated
      markAllCompleted(constr)
      constr
    }

// -------------------- Scala to Java  -----------------------------------

    /** The Java class corresponding to given Scala class.
     *  Note: This only works for
     *   - top-level classes
     *   - Scala classes that were generated via jclassToScala
     *   - classes that have a class owner that has a corresponding Java class
     *  @throws ClassNotFoundException for all Scala classes not in one of these categories.
     */
    @throws(classOf[ClassNotFoundException])
    def classToJava(clazz: ClassSymbol): jClass[_] = classCache.toJava(clazz) {
      def noClass = throw new ClassNotFoundException("no Java class corresponding to "+clazz+" found")
      //println("classToJava "+clazz+" "+clazz.owner+" "+clazz.owner.isPackageClass)//debug
      if (clazz.isPrimitiveValueClass)
        valueClassToJavaType(clazz)
      else if (clazz == ArrayClass)
        noClass
      else if (clazz.isTopLevel)
        javaClass(clazz.javaClassName)
      else if (clazz.owner.isClass) {
        val childOfClass          = !clazz.owner.isModuleClass
        val childOfTopLevel       = clazz.owner.isTopLevel
        val childOfTopLevelObject = clazz.owner.isModuleClass && childOfTopLevel

        // suggested in https://issues.scala-lang.org/browse/SI-4023?focusedCommentId=54759#comment-54759
        var ownerClazz = classToJava(clazz.owner.asClass)
        if (childOfTopLevelObject)
          ownerClazz = jClass.forName(ownerClazz.getName stripSuffix "$", true, ownerClazz.getClassLoader)

        val ownerChildren = ownerClazz.getDeclaredClasses

        var fullNameOfJavaClass = ownerClazz.getName
        if (childOfClass || childOfTopLevel) fullNameOfJavaClass += "$"
        fullNameOfJavaClass += clazz.name

        // compactify (see SI-7779)
        fullNameOfJavaClass = fullNameOfJavaClass match {
          case PackageAndClassPattern(pack, clazzName) =>
            // in a package
            pack + compactifyName(clazzName)
          case _ =>
            // in the empty package
            compactifyName(fullNameOfJavaClass)
        }

        if (clazz.isModuleClass) fullNameOfJavaClass += "$"

        // println(s"ownerChildren = ${ownerChildren.toList}")
        // println(s"fullNameOfJavaClass = $fullNameOfJavaClass")
        ownerChildren.find(_.getName == fullNameOfJavaClass).getOrElse(noClass)
      } else
        noClass
    }

    private val PackageAndClassPattern = """(.*\.)(.*)$""".r

    private def expandedName(sym: Symbol): String =
      if (sym.isPrivate) nme.expandedName(sym.name.toTermName, sym.owner).toString
      else sym.name.toString

    /** The Java field corresponding to a given Scala field.
     *  @param   fld The Scala field.
     */
    def fieldToJava(fld: TermSymbol): jField = fieldCache.toJava(fld) {
      val jclazz = classToJava(fld.owner.asClass)
      val jname = fld.name.dropLocal.toString
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
      val jname = meth.name.dropLocal.toString
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
      jclazz getDeclaredConstructor (effectiveParamClasses: _*)
    }

    /** The Java class that corresponds to given Scala type.
     *  Pre: Scala type is already transformed to Java level.
     */
    def typeToJavaClass(tpe: Type): jClass[_] = tpe match {
      case ExistentialType(_, rtpe)                  => typeToJavaClass(rtpe)
      case TypeRef(_, ArrayClass, List(elemtpe))     => ScalaRunTime.arrayClass(typeToJavaClass(elemtpe))
      case TypeRef(_, sym: ClassSymbol, _)           => classToJava(sym.asClass)
      case tpe @ TypeRef(_, sym: AliasTypeSymbol, _) => typeToJavaClass(tpe.dealias)
      case SingleType(_, sym: ModuleSymbol)          => classToJava(sym.moduleClass.asClass)
      case _                                         => throw new NoClassDefFoundError("no Java class corresponding to "+tpe+" found")
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

  /** 1. If `owner` is a package class (but not the empty package) and `name` is a term name, make a new package
   *  <owner>.<name>, otherwise return NoSymbol.
   *  Exception: If owner is root and a java class with given name exists, create symbol in empty package instead
   *  2. If `owner` is the scala package and `name` designates a phantom class, return
   *     the corresponding class symbol and enter it into this mirror's ScalaPackage.
   */
  override def missingHook(owner: Symbol, name: Name): Symbol = {
    if (owner.hasPackageFlag) {
      val mirror = mirrorThatLoaded(owner)
      if (owner.isRootSymbol && mirror.tryJavaClass(name.toString).isDefined)
        return mirror.EmptyPackageClass.info decl name
      if (name.isTermName && !owner.isEmptyPackageClass)
        return mirror.makeScalaPackage(
          if (owner.isRootSymbol) name.toString else owner.fullName+"."+name)
      if (name == tpnme.AnyRef && owner.owner.isRoot && owner.name == tpnme.scala_)
        // when we synthesize the scala.AnyRef symbol, we need to add it to the scope of the scala package
        // the problem is that adding to the scope implies doing something like `owner.info.decls enter anyRef`
        // which entails running a completer for the scala package
        // which will try to unpickle the stuff in scala/package.class
        // which will transitively load scala.AnyRef
        // which doesn't exist yet, because it hasn't been added to the scope yet
        // this missing hook ties the knot without introducing synchronization problems like before
        return definitions.AnyRefClass
    }
    info("*** missing: "+name+"/"+name.isTermName+"/"+owner+"/"+owner.hasPackageFlag+"/"+owner.info.decls.getClass)
    super.missingHook(owner, name)
  }
}

private[reflect] class ReflectError(msg: String) extends java.lang.Error(msg)

private[reflect] class HasJavaClass[J](val getClazz: J => java.lang.Class[_])
