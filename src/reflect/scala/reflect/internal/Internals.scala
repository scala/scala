/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal

import scala.language.implicitConversions

import scala.reflect.api.Universe
import scala.reflect.macros.Attachments

trait Internals extends api.Internals {
  self: SymbolTable =>

  type Internal = MacroInternalApi
  lazy val internal: Internal = new SymbolTableInternal {}

  @deprecated("compatibility with Scala 2.10 EOL", "2.13.0")
  type Compat = MacroCompatApi
  @deprecated("compatibility with Scala 2.10 EOL", "2.13.0")
  lazy val compat: Compat = new Compat {}

  trait SymbolTableInternal extends MacroInternalApi {
    lazy val reificationSupport: ReificationSupportApi = self.build

    def createImporter(from0: Universe): Importer { val from: from0.type } = self.mkImporter(from0)

    def newScopeWith(elems: Symbol*): Scope = self.newScopeWith(elems: _*)
    def enter(scope: Scope, sym: Symbol): scope.type = { scope.enter(sym); scope }
    def unlink(scope: Scope, sym: Symbol): scope.type = { scope.unlink(sym); scope }

    def freeTerms(tree: Tree): List[FreeTermSymbol] = tree.freeTerms
    def freeTypes(tree: Tree): List[FreeTypeSymbol] = tree.freeTypes
    def substituteSymbols(tree: Tree, from: List[Symbol], to: List[Symbol]): Tree = tree.substituteSymbols(from, to)
    def substituteTypes(tree: Tree, from: List[Symbol], to: List[Type]): Tree = tree.substituteTypes(from, to)
    def substituteThis(tree: Tree, clazz: Symbol, to: => Tree): Tree = tree.substituteThis(clazz, to)
    def attachments(tree: Tree): Attachments { type Pos = Position } = tree.attachments
    def updateAttachment[T: ClassTag](tree: Tree, attachment: T): tree.type = tree.updateAttachment(attachment)
    def removeAttachment[T: ClassTag](tree: Tree): tree.type = tree.removeAttachment[T]
    def setPos(tree: Tree, newpos: Position): tree.type = tree.setPos(newpos)
    def setType(tree: Tree, tp: Type): tree.type = tree.setType(tp)
    def defineType(tree: Tree, tp: Type): tree.type = tree.defineType(tp)
    def setSymbol(tree: Tree, sym: Symbol): tree.type = tree.setSymbol(sym)
    def setOriginal(tt: TypeTree, tree: Tree): TypeTree = tt.setOriginal(tree)

    def captureVariable(vble: Symbol): Unit = self.captureVariable(vble)
    def referenceCapturedVariable(vble: Symbol): Tree = self.referenceCapturedVariable(vble)
    def capturedVariableType(vble: Symbol): Type = self.capturedVariableType(vble)

    def classDef(sym: Symbol, impl: Template): ClassDef = self.ClassDef(sym, impl)
    def moduleDef(sym: Symbol, impl: Template): ModuleDef = self.ModuleDef(sym, impl)
    def valDef(sym: Symbol, rhs: Tree): ValDef = self.ValDef(sym, rhs)
    def valDef(sym: Symbol): ValDef = self.ValDef(sym)
    def defDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef = self.DefDef(sym, mods, vparamss, rhs)
    def defDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef = self.DefDef(sym, vparamss, rhs)
    def defDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef = self.DefDef(sym, mods, rhs)
    def defDef(sym: Symbol, rhs: Tree): DefDef = self.DefDef(sym, rhs)
    def defDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef = self.DefDef(sym, rhs)
    def typeDef(sym: Symbol, rhs: Tree): TypeDef = self.TypeDef(sym, rhs)
    def typeDef(sym: Symbol): TypeDef = self.TypeDef(sym)
    def labelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef = self.LabelDef(sym, params, rhs)

    def changeOwner(tree: Tree, prev: Symbol, next: Symbol): tree.type = { new ChangeOwnerTraverser(prev, next).traverse(tree); tree }

    lazy val gen = self.treeBuild

    def isFreeTerm(symbol: Symbol): Boolean = symbol.isFreeTerm
    def asFreeTerm(symbol: Symbol): FreeTermSymbol = symbol.asFreeTerm
    def isFreeType(symbol: Symbol): Boolean = symbol.isFreeType
    def asFreeType(symbol: Symbol): FreeTypeSymbol = symbol.asFreeType
    def newTermSymbol(symbol: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol = symbol.newTermSymbol(name, pos, flags)
    def newModuleAndClassSymbol(symbol: Symbol, name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol) = symbol.newModuleAndClassSymbol(name, pos, flags)
    def newMethodSymbol(symbol: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol = symbol.newMethodSymbol(name, pos, flags)
    def newTypeSymbol(symbol: Symbol, name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol = symbol.newTypeSymbol(name, pos, flags)
    def newClassSymbol(symbol: Symbol, name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol = symbol.newClassSymbol(name, pos, flags)
    def newFreeTerm(name: String, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTermSymbol = reificationSupport.newFreeTerm(name, value, flags, origin)
    def newFreeType(name: String, flags: FlagSet = NoFlags, origin: String = null): FreeTypeSymbol = reificationSupport.newFreeType(name, flags, origin)
    def isErroneous(symbol: Symbol): Boolean = symbol.isErroneous
    def isSkolem(symbol: Symbol): Boolean = symbol.isSkolem
    def deSkolemize(symbol: Symbol): Symbol = symbol.deSkolemize
    def initialize(symbol: Symbol): symbol.type = symbol.initialize
    def fullyInitialize(symbol: Symbol): symbol.type = definitions.fullyInitializeSymbol(symbol).asInstanceOf[symbol.type]
    def fullyInitialize(tp: Type): tp.type = definitions.fullyInitializeType(tp).asInstanceOf[tp.type]
    def fullyInitialize(scope: Scope): scope.type = definitions.fullyInitializeScope(scope).asInstanceOf[scope.type]
    def flags(symbol: Symbol): FlagSet = symbol.flags
    def attachments(symbol: Symbol): Attachments { type Pos = Position } = symbol.attachments
    def updateAttachment[T: ClassTag](symbol: Symbol, attachment: T): symbol.type = symbol.updateAttachment(attachment)
    def removeAttachment[T: ClassTag](symbol: Symbol): symbol.type = symbol.removeAttachment[T]
    def setOwner(symbol: Symbol, newowner: Symbol): symbol.type = { symbol.owner = newowner; symbol }
    def setInfo(symbol: Symbol, tpe: Type): symbol.type = symbol.setInfo(tpe)
    def setAnnotations(symbol: Symbol, annots: Annotation*): symbol.type = symbol.setAnnotations(annots: _*)
    def setName(symbol: Symbol, name: Name): symbol.type = symbol.setName(name)
    def setPrivateWithin(symbol: Symbol, sym: Symbol): symbol.type = symbol.setPrivateWithin(sym)
    def setFlag(symbol: Symbol, flags: FlagSet): symbol.type = symbol.setFlag(flags)
    def resetFlag(symbol: Symbol, flags: FlagSet): symbol.type = symbol.resetFlag(flags)

    def thisType(sym: Symbol): Type = self.ThisType(sym)
    def singleType(pre: Type, sym: Symbol): Type = self.SingleType(pre, sym)
    def superType(thistpe: Type, supertpe: Type): Type = self.SuperType(thistpe, supertpe)
    def constantType(value: Constant): ConstantType = self.ConstantType(value)
    def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = self.TypeRef(pre, sym, args)
    def refinedType(parents: List[Type], decls: Scope): RefinedType = self.RefinedType(parents, decls)
    def refinedType(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType = self.RefinedType(parents, decls, clazz)
    def refinedType(parents: List[Type], owner: Symbol): Type = self.refinedType(parents, owner)
    def refinedType(parents: List[Type], owner: Symbol, decls: Scope): Type = self.RefinedType(parents, decls, owner)
    def refinedType(parents: List[Type], owner: Symbol, decls: Scope, pos: Position): Type = self.refinedType(parents, owner, decls, pos)
    def intersectionType(tps: List[Type]): Type = self.intersectionType(tps)
    def intersectionType(tps: List[Type], owner: Symbol): Type = self.intersectionType(tps, owner)
    def classInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType = self.ClassInfoType(parents, decls, typeSymbol)
    def methodType(params: List[Symbol], resultType: Type): MethodType = self.MethodType(params, resultType)
    def nullaryMethodType(resultType: Type): NullaryMethodType = self.NullaryMethodType(resultType)
    def polyType(typeParams: List[Symbol], resultType: Type): PolyType = self.PolyType(typeParams, resultType)
    def existentialType(quantified: List[Symbol], underlying: Type): ExistentialType = self.ExistentialType(quantified, underlying)
    def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type = self.existentialAbstraction(tparams, tpe0)
    def annotatedType(annotations: List[Annotation], underlying: Type): AnnotatedType = self.AnnotatedType(annotations, underlying)
    def typeBounds(lo: Type, hi: Type): TypeBounds = self.TypeBounds(lo, hi)
    def boundedWildcardType(bounds: TypeBounds): BoundedWildcardType = self.BoundedWildcardType(bounds)

    def subpatterns(tree: Tree): Option[List[Tree]] = tree.attachments.get[SubpatternsAttachment].map(_.patterns.map(duplicateAndKeepPositions))

    type Decorators = MacroDecoratorApi
    lazy val decorators: Decorators = new MacroDecoratorApi {
      override type ScopeDecorator[T <: Scope] = MacroScopeDecoratorApi[T]
      override implicit def scopeDecorator[T <: Scope](scope: T): ScopeDecorator[T] = new MacroScopeDecoratorApi[T](scope)
      override type TreeDecorator[T <: Tree] = MacroTreeDecoratorApi[T]
      override implicit def treeDecorator[T <: Tree](tree: T): TreeDecorator[T] = new MacroTreeDecoratorApi[T](tree)
      override type TypeTreeDecorator[T <: TypeTree] = MacroTypeTreeDecoratorApi[T]
      override implicit def typeTreeDecorator[T <: TypeTree](tt: T): TypeTreeDecorator[T] = new MacroTypeTreeDecoratorApi[T](tt)
      override type SymbolDecorator[T <: Symbol] = MacroSymbolDecoratorApi[T]
      override implicit def symbolDecorator[T <: Symbol](symbol: T): SymbolDecorator[T] = new MacroSymbolDecoratorApi[T](symbol)
      override type TypeDecorator[T <: Type] = TypeDecoratorApi[T]
      override implicit def typeDecorator[T <: Type](tp: T): TypeDecorator[T] = new TypeDecoratorApi[T](tp)
    }
  }

  lazy val treeBuild = new self.TreeGen {
    def mkAttributedQualifier(tpe: Type): Tree = self.gen.mkAttributedQualifier(tpe)
    def mkAttributedQualifier(tpe: Type, termSym: Symbol): Tree = self.gen.mkAttributedQualifier(tpe, termSym)
    def mkAttributedRef(pre: Type, sym: Symbol): RefTree = self.gen.mkAttributedRef(pre, sym)
    def mkAttributedRef(sym: Symbol): RefTree = self.gen.mkAttributedRef(sym)
    def stabilize(tree: Tree): Tree = self.gen.stabilize(tree)
    def mkAttributedStableRef(pre: Type, sym: Symbol): Tree = self.gen.mkAttributedStableRef(pre, sym)
    def mkAttributedStableRef(sym: Symbol): Tree = self.gen.mkAttributedStableRef(sym)
    def mkUnattributedRef(sym: Symbol): RefTree = self.gen.mkUnattributedRef(sym)
    def mkUnattributedRef(fullName: Name): RefTree = self.gen.mkUnattributedRef(fullName)
    def mkAttributedThis(sym: Symbol): This = self.gen.mkAttributedThis(sym)
    def mkAttributedIdent(sym: Symbol): RefTree = self.gen.mkAttributedIdent(sym)
    def mkAttributedSelect(qual: Tree, sym: Symbol): RefTree = self.gen.mkAttributedSelect(qual, sym)
    def mkMethodCall(receiver: Symbol, methodName: Name, targs: List[Type], args: List[Tree]): Tree = self.gen.mkMethodCall(receiver, methodName, targs, args)
    def mkMethodCall(method: Symbol, targs: List[Type], args: List[Tree]): Tree = self.gen.mkMethodCall(method, targs, args)
    def mkMethodCall(method: Symbol, args: List[Tree]): Tree = self.gen.mkMethodCall(method, args)
    def mkMethodCall(target: Tree, args: List[Tree]): Tree = self.gen.mkMethodCall(target, args)
    def mkMethodCall(receiver: Symbol, methodName: Name, args: List[Tree]): Tree = self.gen.mkMethodCall(receiver, methodName, args)
    def mkMethodCall(receiver: Tree, method: Symbol, targs: List[Type], args: List[Tree]): Tree = self.gen.mkMethodCall(receiver, method, targs, args)
    def mkMethodCall(target: Tree, targs: List[Type], args: List[Tree]): Tree = self.gen.mkMethodCall(target, targs, args)
    def mkNullaryCall(method: Symbol, targs: List[Type]): Tree = self.gen.mkNullaryCall(method, targs)
    def mkRuntimeUniverseRef: Tree = self.gen.mkRuntimeUniverseRef
    def mkZero(tp: Type): Tree = self.gen.mkZero(tp)
    def mkCast(tree: Tree, pt: Type): Tree = self.gen.mkCast(tree, pt)
  }
}
