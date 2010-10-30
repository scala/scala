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
import API._ // imports ImplicitMethodType, which will preserve source compatibility in 2.7 for defDef
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
	private[this] val typeCache = new HashMap[Type, xsbti.api.Type]
	// these caches are necessary for correctness
	private[this] val structureCache = new HashMap[Symbol, xsbti.api.Structure]
	private[this] val classLikeCache = new HashMap[Symbol, xsbti.api.ClassLike]
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
	private def simpleType(t: Type): SimpleType =
		processType(t) match
		{
			case s: SimpleType => s
			case x => error("Not a simple type:\n\tType: " + t + " (class " + t.getClass + ")\n\tTransformed: " + x.getClass)
		}
	private def types(t: List[Type]): Array[xsbti.api.Type] = t.toArray[Type].map(processType)
	private def projectionType(pre: Type, sym: Symbol) =
	{
		if(pre == NoPrefix)
		{
			if(sym.isLocalClass || sym.isRoot || sym.isRootPackage) Constants.emptyType
			else if(sym.isTypeParameterOrSkolem || isExistential(sym)) reference(sym)
			else {
				println("Warning: Unknown prefixless type: " + sym + " in " + sym.owner + " in " + sym.enclClass)
				println("\tFlags: " + sym.flags + ", istype: " + sym.isType + ", absT: " + sym.isAbstractType + ", alias: " + sym.isAliasType + ", nonclass: " + isNonClassType(sym))
				reference(sym)
			}
		}
		else if(sym.isRoot || sym.isRootPackage) Constants.emptyType
		else new xsbti.api.Projection(simpleType(pre), sym.nameString)
	}

	private def reference(sym: Symbol): xsbti.api.ParameterRef = new xsbti.api.ParameterRef(sym.id)


	private def annotations(as: List[AnnotationInfo]): Array[xsbti.api.Annotation] = as.toArray[AnnotationInfo].map(annotation)
	private def annotation(a: AnnotationInfo) =
		new xsbti.api.Annotation(simpleType(a.atp),
			if(a.assocs.isEmpty) Array(new xsbti.api.AnnotationArgument("", a.args.mkString("(", ",", ")"))) // what else to do with a Tree?
			else a.assocs.map { case (name, value) => new xsbti.api.AnnotationArgument(name.toString, value.toString) }.toArray[xsbti.api.AnnotationArgument]
		)
	private def annotated(as: List[AnnotationInfo], tpe: Type) = new xsbti.api.Annotated(simpleType(tpe), annotations(as))

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
					build(base, typeParameters(typeParams0), Nil)
				case MethodType(params, resultType) => // in 2.7, params is of type List[Type], in 2.8 it is List[Symbol]
					build(resultType, typeParams, (params: xsbti.api.ParameterList) :: valueParameters)
				case returnType =>
					new xsbti.api.Def(valueParameters.reverse.toArray, processType(returnType), typeParams, simpleName(s), getAccess(s), getModifiers(s), annotations(s))
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
			new xsbti.api.MethodParameter(name, processType(t), hasDefault(paramSym), special)
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
		val t = viewer(in).memberType(s)
		create(processType(t), simpleName(s), getAccess(s), getModifiers(s), annotations(s))
	}
		
	private def typeDef(in: Symbol, s: Symbol): xsbti.api.TypeMember =
	{
		val (typeParams, tpe) =
			viewer(in).memberInfo(s) match
			{
				case PolyType(typeParams0, base) => (typeParameters(typeParams0), base)
				case t => (Array[xsbti.api.TypeParameter](), t)
			}
		val name = simpleName(s)
		val access = getAccess(s)
		val modifiers = getModifiers(s)
		val as = annotations(s)

		if(s.isAliasType)
			new xsbti.api.TypeAlias(processType(tpe), typeParams, name, access, modifiers, as)
		else if(s.isAbstractType)
		{
			val bounds = tpe.bounds
			new xsbti.api.TypeDeclaration(processType(bounds.lo), processType(bounds.hi), typeParams, name, access, modifiers, as)
		}
		else
			error("Unknown type member" + s)
	}

	private def structure(s: Symbol): xsbti.api.Structure = structure(s.info, s, true)
	private def structure(info: Type): xsbti.api.Structure = structure(info, info.typeSymbol, false)
	private def structure(info: Type, s: Symbol, inherit: Boolean): xsbti.api.Structure =
		structureCache.getOrElseUpdate( s, mkStructure(info, s, inherit))

	private def mkStructure(info: Type, s: Symbol, inherit: Boolean): xsbti.api.Structure =
	{
		val (declared, inherited) = info.members.reverse.partition(_.owner == s)
		val baseTypes = info.baseClasses.tail.map(info.baseType)
		mkStructure(s, baseTypes, declared, if(inherit) inherited filter { !_.isConstructor} else Nil)
	}

	private def mkStructure(s: Symbol, bases: List[Type], declared: List[Symbol], inherited: List[Symbol]): xsbti.api.Structure =
		new xsbti.api.Structure(lzy(types(bases)), lzy(processDefinitions(s, declared)), lzy(processDefinitions(s, inherited)))
	private def processDefinitions(in: Symbol, defs: List[Symbol]): Array[xsbti.api.Definition] =
		defs.toArray.flatMap( (d: Symbol) => definition(in, d))
	private def definition(in: Symbol, sym: Symbol): Option[xsbti.api.Definition] =
	{
		def mkVar = Some(fieldDef(in, sym, new xsbti.api.Var(_,_,_,_,_)))
		def mkVal = Some(fieldDef(in, sym, new xsbti.api.Val(_,_,_,_,_)))
		if(sym.isClass)
			Some(classLike(in, sym))
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
	private def processType(t: Type): xsbti.api.Type = typeCache.getOrElseUpdate(t, makeType(t))
	private def makeType(t: Type): xsbti.api.Type =
	{
		def dealias(t: Type) = t match { case TypeRef(_, sym, _) if sym.isAliasType => t.normalize; case _ => t }

		dealias(t) match
		{
			case NoPrefix => Constants.emptyType
			case ThisType(sym) => new xsbti.api.Singleton(thisPath(sym))
			case SingleType(pre, sym) => projectionType(pre, sym)
			case ConstantType(value) => error("Constant type (not implemented)")
			case TypeRef(pre, sym, args) =>
				val base = projectionType(pre, sym)
				if(args.isEmpty) base else new xsbti.api.Parameterized(base, types(args))
			case SuperType(thistpe: Type, supertpe: Type) => error("Super type (not implemented)")
			case at: AnnotatedType => annotatedType(at)
			case rt: CompoundType => structure(rt)
			case ExistentialType(tparams, result) => new xsbti.api.Existential(processType(result), typeParameters(tparams))
			case NoType => error("NoType")
			case PolyType(typeParams, resultType) => new xsbti.api.Polymorphic(processType(resultType), typeParameters(typeParams))
			case _ => error("Unhandled type " + t.getClass + " : " + t)
		}
	}
	private def typeParameters(s: Symbol): Array[xsbti.api.TypeParameter] = typeParameters(s.typeParams)
	private def typeParameters(s: List[Symbol]): Array[xsbti.api.TypeParameter] = s.map(typeParameter).toArray[xsbti.api.TypeParameter]
	private def typeParameter(s: Symbol): xsbti.api.TypeParameter =
	{
		val varianceInt = s.variance
		import xsbti.api.Variance._
		val annots = annotations(s)
		val variance = if(varianceInt < 0) Contravariant else if(varianceInt > 0) Covariant else Invariant
		s.info match
		{
			case TypeBounds(low, high) => new xsbti.api.TypeParameter( s.id, annots, typeParameters(s), variance, processType(low), processType(high) )
			case PolyType(typeParams, base) => new xsbti.api.TypeParameter( s.id, annots, typeParameters(typeParams), variance, processType(base.bounds.lo),  processType(base.bounds.hi))
			case x => error("Unknown type parameter info: " + x.getClass)
		}
	}
	private def selfType(s: Symbol): xsbti.api.Type =
		if(s.thisSym eq s) Constants.normalSelf else processType(s.thisSym.typeOfThis)

	private def classLike(in: Symbol, c: Symbol): ClassLike = classLikeCache.getOrElseUpdate(c, mkClassLike(in, c))
	private def mkClassLike(in: Symbol, c: Symbol): ClassLike =
	{
		val name = fullName(c)
		val isModule = c.isModuleClass || c.isModule
		val defType =
			if(c.isTrait) DefinitionType.Trait
			else if(isModule)
			{
				if(c.isPackage) DefinitionType.PackageModule
				else DefinitionType.Module
			}
			else DefinitionType.ClassDef
		new xsbti.api.ClassLike(defType, lzy(selfType(c)), lzy(structure(c)), typeParameters(c), name, getAccess(c), getModifiers(c), annotations(c))
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

	private def annotations(s: Symbol): Array[xsbti.api.Annotation] =
		atPhase(currentRun.typerPhase) {
			val base = if(s.hasFlag(Flags.ACCESSOR)) s.accessed else NoSymbol
			val b = if(base == NoSymbol) s else base
			// annotations from bean methods are not handled because:
			//  a) they are recorded as normal source methods anyway
			//  b) there is no way to distinguish them from user-defined methods
			val associated = List(b, b.getter(b.enclClass), b.setter(b.enclClass)).filter(_ != NoSymbol)
			associated.flatMap( ss => annotations(ss.attributes) ).removeDuplicates.toArray ;
		}
	private def annotatedType(at: AnnotatedType): xsbti.api.Type =
	{
		val annots = at.attributes
		if(annots.isEmpty) processType(at.underlying) else annotated(annots, at.underlying)
	}
	private def fullName(s: Symbol): String = nameString(s)
	private def simpleName(s: Symbol): String = s.simpleName.toString.trim
}
