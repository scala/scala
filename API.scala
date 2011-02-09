/* sbt -- Simple Build Tool
 * Copyright 2008, 2009, 2010 Mark Harrah
 */
package xsbt

import java.io.File
import scala.tools.nsc.{io, plugins, symtab, Global, Phase}
import io.{AbstractFile, PlainFile, ZipArchive}
import plugins.{Plugin, PluginComponent}
import symtab.Flags
import scala.collection.mutable.{HashMap, HashSet, ListBuffer}
import xsbti.api.{ClassLike, DefinitionType, PathComponent, SimpleType}

object API
{
	val name = "xsbt-api"
	// for 2.7 compatibility: this class was removed in 2.8
	type ImplicitMethodType = AnyRef
}
 // imports ImplicitMethodType, which will preserve source compatibility in 2.7 for defDef
import API._

final class API(val global: Global, val callback: xsbti.AnalysisCallback) extends Compat
{
	import global._
	def error(msg: String) = throw new RuntimeException(msg)

	def newPhase(prev: Phase) = new ApiPhase(prev)
	class ApiPhase(prev: Phase) extends Phase(prev)
	{
		override def description = "Extracts the public API from source files."
		def name = API.name
		def run: Unit =
		{
			val start = System.currentTimeMillis
			currentRun.units.foreach(processUnit)
			val stop = System.currentTimeMillis
			println("API phase took : " + ((stop - start)/1000.0) + " s")
		}
		def processUnit(unit: CompilationUnit)
		{
			val sourceFile = unit.source.file.file
			println("Traversing " + sourceFile)
			val traverser = new TopLevelHandler(sourceFile)
			traverser.apply(unit.body)
			val packages = traverser.packages.toArray[String].map(p => new xsbti.api.Package(p))
			val source = new xsbti.api.Source(packages, traverser.definitions.toArray[xsbti.api.Definition])
			forceStructures()
			clearCaches()
			callback.api(sourceFile, source)
		}
	}

	// this cache reduces duplicate work both here and when persisting
	//   caches on other structures had minimal effect on time and cache size
	//   (tried: Definition, Modifier, Path, Id, String)
	private[this] val typeCache = new HashMap[(Symbol,Type), xsbti.api.Type]
	// these caches are necessary for correctness
	private[this] val structureCache = new HashMap[Symbol, xsbti.api.Structure]
	private[this] val classLikeCache = new HashMap[(Symbol,Symbol), xsbti.api.ClassLike]
	private[this] val pending = new HashSet[xsbti.api.Lazy[_]]

	// to mitigate "temporary leaks" like that caused by NoPhase in 2.8.0,
	//   this ensures this class is not retaining objects
	private def clearCaches()
	{
		typeCache.clear()
		structureCache.clear()
		classLikeCache.clear()
	}

	// call back to the xsbti.SafeLazy class in main sbt code to construct a SafeLazy instance
	//   we pass a thunk, whose class is loaded by the interface class loader (this class's loader)
	//   SafeLazy ensures that once the value is forced, the thunk is nulled out and so 
	//   references to the thunk's classes are not retained.  Specifically, it allows the interface classes
	//   (those in this subproject) can be garbage collected after compilation. 
	private[this] val safeLazy = Class.forName("xsbti.SafeLazy").getMethod("apply", classOf[xsbti.F0[_]])
	private def lzy[S <: AnyRef](s: => S): xsbti.api.Lazy[S] =
	{
		val z = safeLazy.invoke(null, Message(s)).asInstanceOf[xsbti.api.Lazy[S]]
		pending += z
		z
	}

	// force all lazy structures.  This is necessary so that we see the symbols/types at this phase and
	//   so that we don't hold on to compiler objects and classes
	private def forceStructures(): Unit =
		if(pending.isEmpty)
			structureCache.clear()
		else
		{
			val toProcess = pending.toList
			pending.clear()
			toProcess foreach { _.get() }
			forceStructures()
		}

	private def thisPath(sym: Symbol) = path(pathComponents(sym, Constants.thisPath :: Nil))
	private def path(components: List[PathComponent]) = new xsbti.api.Path(components.toArray[PathComponent])
	private def pathComponents(sym: Symbol, postfix: List[PathComponent]): List[PathComponent] =
	{
		if(sym == NoSymbol || sym.isRoot || sym.isEmptyPackageClass || sym.isRootPackage) postfix
		else pathComponents(sym.owner, new xsbti.api.Id(simpleName(sym)) :: postfix)
	}
	private def simpleType(in: Symbol, t: Type): SimpleType =
		processType(in, t) match
		{
			case s: SimpleType => s
			case x => error("Not a simple type:\n\tType: " + t + " (class " + t.getClass + ")\n\tTransformed: " + x.getClass)
		}
	private def types(in: Symbol, t: List[Type]): Array[xsbti.api.Type] = t.toArray[Type].map(processType(in, _))
	private def projectionType(in: Symbol, pre: Type, sym: Symbol) =
	{
		if(pre == NoPrefix)
		{
			if(sym.isLocalClass || sym.isRoot || sym.isRootPackage) Constants.emptyType
			else if(sym.isTypeParameterOrSkolem || isExistential(sym)) reference(sym)
			else {
				// this appears to come from an existential type in an inherited member- not sure why isExistential is false here
				/*println("Warning: Unknown prefixless type: " + sym + " in " + sym.owner + " in " + sym.enclClass)
				println("\tFlags: " + sym.flags + ", istype: " + sym.isType + ", absT: " + sym.isAbstractType + ", alias: " + sym.isAliasType + ", nonclass: " + isNonClassType(sym))*/
				reference(sym)
			}
		}
		else if(sym.isRoot || sym.isRootPackage) Constants.emptyType
		else new xsbti.api.Projection(simpleType(in, pre), sym.nameString)
	}

	private def reference(sym: Symbol): xsbti.api.ParameterRef = new xsbti.api.ParameterRef(sym.id)


	private def annotations(in: Symbol, as: List[AnnotationInfo]): Array[xsbti.api.Annotation] = as.toArray[AnnotationInfo].map(annotation(in,_))
	private def annotation(in: Symbol, a: AnnotationInfo) =
		new xsbti.api.Annotation(simpleType(in, a.atp),
			if(a.assocs.isEmpty) Array(new xsbti.api.AnnotationArgument("", a.args.mkString("(", ",", ")"))) // what else to do with a Tree?
			else a.assocs.map { case (name, value) => new xsbti.api.AnnotationArgument(name.toString, value.toString) }.toArray[xsbti.api.AnnotationArgument]
		)
	private def annotated(in: Symbol, as: List[AnnotationInfo], tpe: Type) = new xsbti.api.Annotated(simpleType(in, tpe), annotations(in, as))

	private def viewer(s: Symbol) = (if(s.isModule) s.moduleClass else s).thisType
	private def printMember(label: String, in: Symbol, t: Type) = println(label + " in " + in + " : " + t + " (debug: " + debugString(t) + " )")
	private def defDef(in: Symbol, s: Symbol) =
	{
		def build(t: Type, typeParams: Array[xsbti.api.TypeParameter], valueParameters: List[xsbti.api.ParameterList]): xsbti.api.Def =
		{
			// 2.8 compatibility
			implicit def symbolsToParameters(syms: List[Symbol]): xsbti.api.ParameterList =
			{
				val isImplicitList = syms match { case head :: _ => isImplicit(head); case _ => false }
				new xsbti.api.ParameterList(syms.map(parameterS).toArray, isImplicitList)
			}
			// 2.7 compatibility
			implicit def typesToParameters(syms: List[Type]): xsbti.api.ParameterList =
			{
				val isImplicitList = t.isInstanceOf[ImplicitMethodType]
				new xsbti.api.ParameterList(syms.map(parameterT).toArray, isImplicitList)
			}
			t match
			{
				case PolyType(typeParams0, base) =>
					assert(typeParams.isEmpty)
					assert(valueParameters.isEmpty)
					build(base, typeParameters(in, typeParams0), Nil)
				case MethodType(params, resultType) => // in 2.7, params is of type List[Type], in 2.8 it is List[Symbol]
					build(resultType, typeParams, (params: xsbti.api.ParameterList) :: valueParameters)
				case Nullary(resultType) => // 2.9 and later
					build(resultType, typeParams, valueParameters)
				case returnType =>
					new xsbti.api.Def(valueParameters.reverse.toArray, processType(in, returnType), typeParams, simpleName(s), getAccess(s), getModifiers(s), annotations(in,s))
			}
		}
		def parameterS(s: Symbol): xsbti.api.MethodParameter =
			makeParameter(s.nameString, s.info, s.info.typeSymbol, s)

		def parameterT(t: Type): xsbti.api.MethodParameter =
			makeParameter("", t, t.typeSymbol, NoSymbol)

		// paramSym is only for 2.8 and is to determine if the parameter has a default
		def makeParameter(name: String, tpe: Type, ts: Symbol, paramSym: Symbol): xsbti.api.MethodParameter =
		{
			import xsbti.api.ParameterModifier._
			val (t, special) =
				if(ts == definitions.RepeatedParamClass)// || s == definitions.JavaRepeatedParamClass)
					(tpe.typeArgs(0), Repeated)
				else if(ts == definitions.ByNameParamClass)
					(tpe.typeArgs(0), ByName)
				else
					(tpe, Plain)
			new xsbti.api.MethodParameter(name, processType(in, t), hasDefault(paramSym), special)
		}
		val t = viewer(in).memberInfo(s)
		build(t, Array(), Nil)
	}
	private def hasDefault(s: Symbol) =
	{
		// 2.7 compatibility
		implicit def flagsWithDefault(f: AnyRef): WithDefault = new WithDefault
		class WithDefault { val DEFAULTPARAM = 0x00000000 }
		s != NoSymbol && s.hasFlag(Flags.DEFAULTPARAM)
	}
	private def fieldDef[T](in: Symbol, s: Symbol, create: (xsbti.api.Type, String, xsbti.api.Access, xsbti.api.Modifiers, Array[xsbti.api.Annotation]) => T): T =
	{
		val t = dropNullary(viewer(in).memberType(s))
		create(processType(in, t), simpleName(s), getAccess(s), getModifiers(s), annotations(in, s))
	}
	private def dropNullary(t: Type): Type = t match {
		case Nullary(un) => un
		case _ => t
	}
		
	private def typeDef(in: Symbol, s: Symbol): xsbti.api.TypeMember =
	{
		val (typeParams, tpe) =
			viewer(in).memberInfo(s) match
			{
				case PolyType(typeParams0, base) => (typeParameters(in, typeParams0), base)
				case t => (Array[xsbti.api.TypeParameter](), t)
			}
		val name = simpleName(s)
		val access = getAccess(s)
		val modifiers = getModifiers(s)
		val as = annotations(in, s)

		if(s.isAliasType)
			new xsbti.api.TypeAlias(processType(in, tpe), typeParams, name, access, modifiers, as)
		else if(s.isAbstractType)
		{
			val bounds = tpe.bounds
			new xsbti.api.TypeDeclaration(processType(in, bounds.lo), processType(in, bounds.hi), typeParams, name, access, modifiers, as)
		}
		else
			error("Unknown type member" + s)
	}

	private def structure(in: Symbol, s: Symbol): xsbti.api.Structure = structure(viewer(in).memberInfo(s), s, true)
	private def structure(info: Type): xsbti.api.Structure = structure(info, info.typeSymbol, false)
	private def structure(info: Type, s: Symbol, inherit: Boolean): xsbti.api.Structure =
		structureCache.getOrElseUpdate( s, mkStructure(info, s, inherit))

	private def removeConstructors(ds: List[Symbol]): List[Symbol] = ds filter { !_.isConstructor}

	private def mkStructure(info: Type, s: Symbol, inherit: Boolean): xsbti.api.Structure =
	{
		val (declared, inherited) = info.members.reverse.partition(_.owner == s)
		val baseTypes = info.baseClasses.tail.map(info.baseType)
		val ds = if(s.isModuleClass) removeConstructors(declared) else declared
		val is = if(inherit) removeConstructors(inherited) else Nil
		mkStructure(s, baseTypes, ds, is)
	}

	private def mkStructure(s: Symbol, bases: List[Type], declared: List[Symbol], inherited: List[Symbol]): xsbti.api.Structure =
		new xsbti.api.Structure(lzy(types(s, bases)), lzy(processDefinitions(s, declared)), lzy(processDefinitions(s, inherited)))
	private def processDefinitions(in: Symbol, defs: List[Symbol]): Array[xsbti.api.Definition] =
		defs.toArray.flatMap( (d: Symbol) => definition(in, d))
	private def definition(in: Symbol, sym: Symbol): Option[xsbti.api.Definition] =
	{
		def mkVar = Some(fieldDef(in, sym, new xsbti.api.Var(_,_,_,_,_)))
		def mkVal = Some(fieldDef(in, sym, new xsbti.api.Val(_,_,_,_,_)))
		if(sym.isClass || sym.isModule)
			if(ignoreClass(sym)) None else Some(classLike(in, sym))
		else if(isNonClassType(sym))
			Some(typeDef(in, sym))
		else if(sym.isVariable)
			if(isSourceField(sym)) mkVar else None
		else if(sym.isStable)
			if(isSourceField(sym)) mkVal else None
		else if(sym.isSourceMethod && !sym.isSetter)
			if(sym.isGetter) mkVar else Some(defDef(in, sym))
		else
			None
	}
	private def ignoreClass(sym: Symbol): Boolean =
		sym.isLocalClass || sym.isAnonymousClass || fullName(sym).endsWith(LocalChild)

	// This filters private[this] vals/vars that were not in the original source.
	//  The getter will be used for processing instead.
	private def isSourceField(sym: Symbol): Boolean =
	{
		val getter = sym.getter(sym.enclClass)
		// the check `getter eq sym` is a precaution against infinite recursion
		// `isParamAccessor` does not exist in all supported versions of Scala, so the flag check is done directly
		(getter == NoSymbol && !sym.hasFlag(Flags.PARAMACCESSOR)) || (getter eq sym)
	}
	private def getModifiers(s: Symbol): xsbti.api.Modifiers =
	{
		import Flags._
		new xsbti.api.Modifiers(s.hasFlag(ABSTRACT) || s.hasFlag(DEFERRED), s.hasFlag(OVERRIDE),
			s.isFinal, s.hasFlag(SEALED), isImplicit(s), s.hasFlag(LAZY))
	}

	private def isImplicit(s: Symbol) = s.hasFlag(Flags.IMPLICIT)
	private def getAccess(c: Symbol): xsbti.api.Access =
	{
		if(c.isPublic) Constants.public
		else if(c.isPrivateLocal) Constants.privateLocal
		else if(c.isProtectedLocal) Constants.protectedLocal
		else
		{
			val within = c.privateWithin
			val qualifier = if(within == NoSymbol) Constants.unqualified else new xsbti.api.IdQualifier(fullName(within))
			if(c.hasFlag(Flags.PROTECTED)) new xsbti.api.Protected(qualifier)
			else new xsbti.api.Private(qualifier)
		}
	}
	private def processType(in: Symbol, t: Type): xsbti.api.Type = typeCache.getOrElseUpdate((in, t), makeType(in, t))
	private def makeType(in: Symbol, t: Type): xsbti.api.Type =
	{
		def dealias(t: Type) = t match { case TypeRef(_, sym, _) if sym.isAliasType => t.normalize; case _ => t }

		dealias(t) match
		{
			case NoPrefix => Constants.emptyType
			case ThisType(sym) => new xsbti.api.Singleton(thisPath(sym))
			case SingleType(pre, sym) => projectionType(in, pre, sym)
			case ConstantType(value) => error("Constant type (not implemented)")
			case TypeRef(pre, sym, args) =>
				val base = projectionType(in, pre, sym)
				if(args.isEmpty) base else new xsbti.api.Parameterized(base, types(in, args))
			case SuperType(thistpe: Type, supertpe: Type) => error("Super type (not implemented)")
			case at: AnnotatedType => annotatedType(in, at)
			case rt: CompoundType => structure(rt)
			case ExistentialType(tparams, result) => new xsbti.api.Existential(processType(in, result), typeParameters(in, tparams))
			case NoType => error("NoType")
			case PolyType(typeParams, resultType) => new xsbti.api.Polymorphic(processType(in, resultType), typeParameters(in, typeParams))
			case Nullary(resultType) => error("Unexpected nullary method type " + in + " in " + in.owner)
			case _ => error("Unhandled type " + t.getClass + " : " + t)
		}
	}
	private def typeParameters(in: Symbol, s: Symbol): Array[xsbti.api.TypeParameter] = typeParameters(in, s.typeParams)
	private def typeParameters(in: Symbol, s: List[Symbol]): Array[xsbti.api.TypeParameter] = s.map(typeParameter(in,_)).toArray[xsbti.api.TypeParameter]
	private def typeParameter(in: Symbol, s: Symbol): xsbti.api.TypeParameter =
	{
		val varianceInt = s.variance
		import xsbti.api.Variance._
		val annots = annotations(in, s)
		val variance = if(varianceInt < 0) Contravariant else if(varianceInt > 0) Covariant else Invariant
		viewer(in).memberInfo(s) match
		{
			case TypeBounds(low, high) => new xsbti.api.TypeParameter( s.id, annots, typeParameters(in, s), variance, processType(in, low), processType(in, high) )
			case PolyType(typeParams, base) => new xsbti.api.TypeParameter( s.id, annots, typeParameters(in, typeParams), variance, processType(in, base.bounds.lo),  processType(in, base.bounds.hi))
			case x => error("Unknown type parameter info: " + x.getClass)
		}
	}
	private def selfType(in: Symbol, s: Symbol): xsbti.api.Type =
		if(s.thisSym eq s) Constants.normalSelf else processType(in, s.thisSym.typeOfThis)

	private def classLike(in: Symbol, c: Symbol): ClassLike = classLikeCache.getOrElseUpdate( (in,c), mkClassLike(in, c))
	private def mkClassLike(in: Symbol, c: Symbol): ClassLike =
	{
		val name = fullName(c)
		val isModule = c.isModuleClass || c.isModule
		val struct = if(isModule) c.moduleClass else c
		val defType =
			if(c.isTrait) DefinitionType.Trait
			else if(isModule)
			{
				if(c.isPackage) DefinitionType.PackageModule
				else DefinitionType.Module
			}
			else DefinitionType.ClassDef
		new xsbti.api.ClassLike(defType, lzy(selfType(in, c)), lzy(structure(in, struct)), typeParameters(in, c), name, getAccess(c), getModifiers(c), annotations(in, c))
	}
	private final class TopLevelHandler(sourceFile: File) extends TopLevelTraverser
	{
		val packages = new HashSet[String]
		val definitions = new ListBuffer[xsbti.api.Definition]
		def `class`(c: Symbol): Unit = definitions += classLike(c.owner, c)
		/** Record packages declared in the source file*/
		def `package`(p: Symbol)
		{
			if( (p eq null) || p == NoSymbol || p.isRoot || p.isRootPackage || p.isEmptyPackageClass || p.isEmptyPackage)
				()
			else
			{
				packages += fullName(p)
				`package`(p.enclosingPackage)
			}
		}
	}
	private object Constants
	{
		val local = new xsbti.api.ThisQualifier
		val public = new xsbti.api.Public
		val privateLocal = new xsbti.api.Private(local)
		val protectedLocal = new xsbti.api.Protected(local)
		val unqualified = new xsbti.api.Unqualified
		val emptyPath = new xsbti.api.Path(Array())
		val thisPath = new xsbti.api.This
		val emptyType = new xsbti.api.EmptyType
		val normalSelf = emptyType
	}
	private abstract class TopLevelTraverser extends Traverser
	{
		def `class`(s: Symbol)
		def `package`(s: Symbol)
		override def traverse(tree: Tree)
		{
			tree match
			{
				case (_: ClassDef | _ : ModuleDef) if isTopLevel(tree.symbol) => `class`(tree.symbol)
				case p: PackageDef =>
					`package`(p.symbol)
					super.traverse(tree)
				case _ =>
			}
		}
		def isTopLevel(sym: Symbol): Boolean =
			(sym ne null) && (sym != NoSymbol) && !sym.isImplClass && !sym.isNestedClass && sym.isStatic &&
			!sym.hasFlag(Flags.SYNTHETIC) && !sym.hasFlag(Flags.JAVA)
	}
	
		// In 2.8, attributes is renamed to annotations
		implicit def compat(a: AnyRef): WithAnnotations = new WithAnnotations(a)
		class WithAnnotations(a: AnyRef) { def attributes = a.getClass.getMethod("annotations").invoke(a).asInstanceOf[List[AnnotationInfo]] }

	private def annotations(in: Symbol, s: Symbol): Array[xsbti.api.Annotation] =
		atPhase(currentRun.typerPhase) {
			val base = if(s.hasFlag(Flags.ACCESSOR)) s.accessed else NoSymbol
			val b = if(base == NoSymbol) s else base
			// annotations from bean methods are not handled because:
			//  a) they are recorded as normal source methods anyway
			//  b) there is no way to distinguish them from user-defined methods
			val associated = List(b, b.getter(b.enclClass), b.setter(b.enclClass)).filter(_ != NoSymbol)
			associated.flatMap( ss => annotations(in, ss.attributes) ).removeDuplicates.toArray ;
		}
	private def annotatedType(in: Symbol, at: AnnotatedType): xsbti.api.Type =
	{
		val annots = at.attributes
		if(annots.isEmpty) processType(in, at.underlying) else annotated(in, annots, at.underlying)
	}
	private def fullName(s: Symbol): String = nameString(s)
	private def simpleName(s: Symbol): String = s.simpleName.toString.trim
}
