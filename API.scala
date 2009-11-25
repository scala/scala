/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
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
}
final class API(val global: Global, val callback: xsbti.AnalysisCallback) extends NotNull
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
			if(java.lang.Boolean.getBoolean("sbt.api.enable"))
				currentRun.units.foreach(processUnit)
			val stop = System.currentTimeMillis
			println("API phase took : " + ((stop - start)/1000.0) + " s")
		}
		def processUnit(unit: CompilationUnit)
		{
			val sourceFile = unit.source.file.file
			val traverser = new TopLevelHandler(sourceFile)
			traverser.apply(unit.body)
			val packages = traverser.packages.toArray[String].map(p => new xsbti.api.Package(p))
			val source = new xsbti.api.Source(packages, traverser.definitions.toArray[xsbti.api.Definition])
			callback.api(sourceFile, source)
		}
	}
	private def thisPath(sym: Symbol) = path(pathComponents(sym, Constants.thisPath :: Nil))
	private def path(components: List[PathComponent]) = new xsbti.api.Path(components.toArray[PathComponent])
	private def pathComponents(sym: Symbol, postfix: List[PathComponent]): List[PathComponent] =
	{
		if(sym == NoSymbol || sym.isRoot || sym.isRootPackage) postfix
		else pathComponents(sym.owner, new xsbti.api.Id(sym.simpleName.toString) :: postfix)
	}
	private def simpleType(t: Type): SimpleType =
		processType(t) match
		{
			case s: SimpleType => s
			case _ => error("Expected simple type: " + t)
		}
	private def types(t: List[Type]): Array[xsbti.api.Type] = t.toArray[Type].map(processType)
	private def projectionType(pre: Type, sym: Symbol) =
	{
		if(pre == NoPrefix) new xsbti.api.ParameterRef(sym.id)
		else if(sym.isRoot || sym.isRootPackage) Constants.emptyType
		else new xsbti.api.Projection(simpleType(pre), sym.nameString)
	}

	private def annotations(as: List[AnnotationInfo]): Array[xsbti.api.Annotation] = as.toArray[AnnotationInfo].map(annotation)
	private def annotation(a: AnnotationInfo) = new xsbti.api.Annotation(simpleType(a.atp), a.args.map(_.hashCode.toString).toArray[String])
	private def annotated(as: List[AnnotationInfo], tpe: Type) = new xsbti.api.Annotated(simpleType(tpe), annotations(as))

	private def defDef(s: Symbol) =
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
					new xsbti.api.Def(valueParameters.toArray, processType(returnType), typeParams, s.fullNameString, getAccess(s), getModifiers(s), annotations(s))
			}
		}
		def parameterS(s: Symbol): xsbti.api.MethodParameter = makeParameter(s.nameString, s.info, s.info.typeSymbol)
		def parameterT(t: Type): xsbti.api.MethodParameter = makeParameter("", t, t.typeSymbol)
		def makeParameter(name: String, tpe: Type, ts: Symbol): xsbti.api.MethodParameter =
		{
			import xsbti.api.ParameterModifier._
			val (t, special) =
				if(ts == definitions.RepeatedParamClass)// || s == definitions.JavaRepeatedParamClass)
					(tpe.typeArgs(0), Repeated)
				else if(ts == definitions.ByNameParamClass)
					(tpe.typeArgs(0), ByName)
				else
					(tpe, Plain)
			new xsbti.api.MethodParameter(name, processType(t), hasDefault(s), special)
		}
		build(s.info, Array(), Nil)
	}
	private def hasDefault(s: Symbol) =
	{
		// 2.7 compatibility
		implicit def flagsWithDefault(f: AnyRef): WithDefault = new WithDefault
		class WithDefault { val DEFAULTPARAM = 0x02000000 }
		s.hasFlag(Flags.DEFAULTPARAM)
	}
	private def fieldDef[T](s: Symbol, create: (xsbti.api.Type, String, xsbti.api.Access, xsbti.api.Modifiers, Array[xsbti.api.Annotation]) => T): T =
		create(processType(s.tpe), s.fullNameString, getAccess(s), getModifiers(s), annotations(s))
	private def typeDef(s: Symbol): xsbti.api.TypeMember =
	{
		val (typeParams, tpe) =
			s.info match
			{
				case PolyType(typeParams0, base) => (typeParameters(typeParams0), base)
				case t => (Array[xsbti.api.TypeParameter](), t)
			}
		val name = s.fullNameString
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

	private def structure(s: Symbol): xsbti.api.Structure = structure(s.info)
	private def structure(info: Type): xsbti.api.Structure =
	{
		val s = info.typeSymbol
		val (declared, inherited) = info.members.partition(_.owner == s)
		structure(info.baseClasses.map(_.tpe), declared, inherited) // linearization instead of parents
	}
	private def structure(parents: List[Type], declared: List[Symbol], inherited: List[Symbol]): xsbti.api.Structure =
		new xsbti.api.Structure(types(parents), processDefinitions(declared), processDefinitions(inherited))
	private def processDefinitions(defs: List[Symbol]): Array[xsbti.api.Definition] = defs.toArray.map(definition)
	private def definition(sym: Symbol): xsbti.api.Definition =
	{
		if(sym.isClass) classLike(sym)
		else if(sym.isMethod) defDef(sym)
		else if(sym.isTypeMember) typeDef(sym)
		else if(sym.isVariable) fieldDef(sym, new xsbti.api.Var(_,_,_,_,_))
		else fieldDef(sym, new xsbti.api.Val(_,_,_,_,_))
	}
	private def getModifiers(s: Symbol): xsbti.api.Modifiers =
	{
		import Flags._
		new xsbti.api.Modifiers(s.hasFlag(ABSTRACT), s.hasFlag(DEFERRED), s.hasFlag(OVERRIDE),
			s.isFinal, s.hasFlag(SEALED), isImplicit(s), s.hasFlag(LAZY), s.hasFlag(SYNTHETIC))
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
			val qualifier = if(within == NoSymbol) Constants.unqualified else new xsbti.api.IdQualifier(c.fullNameString)
			if(c.hasFlag(Flags.PRIVATE)) new xsbti.api.Private(qualifier)
			else if(c.hasFlag(Flags.PROTECTED)) new xsbti.api.Protected(qualifier)
			else new xsbti.api.Pkg(qualifier)
		}
	}
	
	private def processType(t: Type): xsbti.api.Type =
	{
		t match
		{
			case NoPrefix => Constants.emptyType
			case ThisType(sym) => new xsbti.api.Singleton(thisPath(sym))
			case SingleType(pre, sym) => projectionType(pre, sym)
			case ConstantType(value) => error("Constant type (not implemented)")
			case TypeRef(pre, sym, args) =>
				val base = projectionType(pre, sym)
				if(args.isEmpty) base else new xsbti.api.Parameterized(base, args.map(simpleType).toArray[SimpleType])
			case SuperType(thistpe: Type, supertpe: Type) => error("Super type (not implemented)")
			case at: AnnotatedType => annotatedType(at)
			case rt: RefinedType => structure(rt)
			case ExistentialType(tparams, result) => new xsbti.api.Existential(processType(result), typeParameters(tparams))
			case NoType => error("NoType")
			case PolyType(typeParams, resultType) => println("polyType(" + typeParams + " , " + resultType + ")"); error("polyType")
			case _ => error("Unhandled type " + t.getClass + " : " + t)
		}
	}
	private def typeParameters(s: Symbol): Array[xsbti.api.TypeParameter] = typeParameters(s.typeParams)
	private def typeParameters(s: List[Symbol]): Array[xsbti.api.TypeParameter] = s.map(typeParameter).toArray[xsbti.api.TypeParameter]
	private def typeParameter(s: Symbol): xsbti.api.TypeParameter =
	{
		val varianceInt = s.variance
		import xsbti.api.Variance._
		val variance = if(varianceInt < 0) Contravariant else if(varianceInt > 0) Covariant else Invariant
		s.info match
		{
			case TypeBounds(low, high) => new xsbti.api.TypeParameter( s.id, typeParameters(s), variance, processType(low), processType(high) )
			case PolyType(typeParams, base) => new xsbti.api.TypeParameter( s.id, typeParameters(typeParams), variance, processType(base.bounds.lo),  processType(base.bounds.hi))
			case x => error("Unknown type parameter info: " + x.getClass)
		}
	}
	private def selfType(s: Symbol): xsbti.api.Type = if(s.thisSym eq s) Constants.normalSelf else processType(s.typeOfThis)
	private def classLike(c: Symbol): ClassLike =
	{
		val name = c.fullNameString
		val isModule = c.isModuleClass || c.isModule
		val defType =
			if(c.isTrait) DefinitionType.Trait
			else if(isModule)
			{
				if(c.isPackage) DefinitionType.PackageModule
				else DefinitionType.Module
			}
			else DefinitionType.ClassDef
		new xsbti.api.ClassLike(defType, selfType(c), structure(c), typeParameters(c), name, getAccess(c), getModifiers(c), annotations(c))
	}
	private final class TopLevelHandler(sourceFile: File) extends TopLevelTraverser
	{
		val packages = new HashSet[String]
		val definitions = new ListBuffer[xsbti.api.Definition]
		def `class`(c: Symbol): Unit = definitions += classLike(c)
		/** Record packages declared in the source file*/
		def `package`(p: Symbol)
		{
			if( (p eq null) || p == NoSymbol || p.isRoot || p.isRootPackage || p.isEmptyPackageClass || p.isEmptyPackage)
				()
			else
			{
				packages += p.fullNameString
				`package`(p.enclosingPackage)
			}
		}
	}
	private object Constants
	{
		val public = new xsbti.api.Public
		val privateLocal = new xsbti.api.Private(local)
		val protectedLocal = new xsbti.api.Protected(local)
		val unqualified = new xsbti.api.Unqualified
		val local = new xsbti.api.ThisQualifier
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

	private def annotations(s: Symbol): Array[xsbti.api.Annotation] = annotations(s.attributes)
	private def annotatedType(at: AnnotatedType): xsbti.api.Type =
		if(at.attributes.isEmpty) processType(at.underlying) else annotated(at.attributes, at.underlying)
}