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
package macros

import scala.language.implicitConversions

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  The refinement of [[scala.reflect.api.Universe]] for the use by macro writers.
 *
 *  This universe provides mutability for reflection artifacts (e.g. macros can change types of compiler trees,
 *  add annotation to symbols representing definitions, etc) and exposes some internal compiler functionality
 *  such as `Symbol.deSkolemize` or `Tree.attachments`.
 *  @groupname Macros Macro Specific Additions
 *  @groupprio Macros -1
 *
 *  @contentDiagram hideNodes "*Api"
 */
abstract class Universe extends scala.reflect.api.Universe {

  /** @see [[MacroInternalApi]] */
  override type Internal <: MacroInternalApi

  /** @see [[InternalApi]] */
  trait MacroInternalApi extends InternalApi { internal =>

    /** Adds a given symbol to the given scope.
     */
    def enter(scope: Scope, sym: Symbol): scope.type

    /** Removes a given symbol to the given scope.
     */
    def unlink(scope: Scope, sym: Symbol): scope.type

    /** Collects all the symbols defined by subtrees of `tree` that are owned by `prev`,
     *  and then changes their owner to point to `next`.
     *
     *  This is an essential tool to battle owner chain corruption when moving trees
     *  from one lexical context to another. Whenever you take an attributed tree that
     *  has been typechecked under the Context owned by some symbol (let's call it `x`)
     *  and splice it elsewhere, into the Context owned by another symbol (let's call it `y`),
     *  it is imperative that you either call `untypecheck` or do `changeOwner(tree, x, y)`.
     *
     *  Since at the moment `untypecheck` has fundamental problem that can sometimes lead to tree corruption,
     *  `changeOwner` becomes an indispensable tool in building 100% robust macros.
     *  Future versions of the reflection API might obviate the need in taking care of
     *  these low-level details, but at the moment this is what we've got.
     */
    def changeOwner(tree: Tree, prev: Symbol, next: Symbol): tree.type

    /** Advanced tree factories */
    val gen: TreeGen

    /** The attachment of the symbol. */
    def attachments(symbol: Symbol): Attachments { type Pos = Position }

    /** Updates the attachment with the payload slot of T added/updated with the provided value.
     *  Replaces an existing payload of the same type, if exists.
     *  Returns the symbol itself.
     */
    def updateAttachment[T: ClassTag](symbol: Symbol, attachment: T): symbol.type

    /** Update the attachment with the payload of the given class type `T` removed.
     *  Returns the symbol itself.
     */
    def removeAttachment[T: ClassTag](symbol: Symbol): symbol.type

    /** Sets the `owner` of the symbol. */
    def setOwner(symbol: Symbol, newowner: Symbol): symbol.type

    /** Sets the `info` of the symbol. */
    def setInfo(symbol: Symbol, tpe: Type): symbol.type

    /** Sets the `annotations` of the symbol. */
    def setAnnotations(symbol: Symbol, annots: Annotation*): symbol.type

    /** Sets the `name` of the symbol. */
    def setName(symbol: Symbol, name: Name): symbol.type

    /** Sets the `privateWithin` of the symbol. */
    def setPrivateWithin(symbol: Symbol, sym: Symbol): symbol.type

    /** Enables `flags` on the symbol. */
    def setFlag(symbol: Symbol, flags: FlagSet): symbol.type

    /** Disables `flags` on the symbol. */
    def resetFlag(symbol: Symbol, flags: FlagSet): symbol.type

    /** The attachment of the tree. */
    def attachments(tree: Tree): Attachments { type Pos = Position }

    /** Updates the attachment with the payload slot of T added/updated with the provided value.
     *  Replaces an existing payload of the same type, if exists.
     *  Returns the tree itself.
     */
    def updateAttachment[T: ClassTag](tree: Tree, attachment: T): tree.type

    /** Update the attachment with the payload of the given class type `T` removed.
     *  Returns the tree itself.
     */
    def removeAttachment[T: ClassTag](tree: Tree): tree.type

    /** Sets the `pos` of the tree. Returns the tree itself. */
    def setPos(tree: Tree, newpos: Position): tree.type

    /** Sets the `tpe` of the tree. Returns the tree itself. */
    def setType(tree: Tree, tp: Type): tree.type

    /** Like `setType`, but if this is a previously empty TypeTree that
     *  fact is remembered so that `untypecheck` will snap back.
     *
     *  \@PP: Attempting to elaborate on the above, I find: If defineType
     *  is called on a TypeTree whose type field is null or NoType,
     *  this is recorded as "wasEmpty = true". That value is used in
     *  ResetAttrsTraverser, which nulls out the type field of TypeTrees
     *  for which wasEmpty is true, leaving the others alone.
     *
     *  `untypecheck` (or `resetAttrs` in compiler parlance) is used
     *  in situations where some speculative
     *  typing of a tree takes place, fails, and the tree needs to be
     *  returned to its former state to try again. So according to me:
     *  using `defineType` instead of `setType` is how you communicate
     *  that the type being set does not depend on any previous state,
     *  and therefore should be abandoned if the current line of type
     *  inquiry doesn't work out.
     */
    def defineType(tree: Tree, tp: Type): tree.type

    /** Sets the `symbol` of the tree. Returns the tree itself. */
    def setSymbol(tree: Tree, sym: Symbol): tree.type

    /** Sets the `original` field of the type tree. */
    def setOriginal(tt: TypeTree, original: Tree): TypeTree

    /** Mark a variable as captured; i.e. force boxing in a *Ref type.
     *  @group Macros
     */
    def captureVariable(vble: Symbol): Unit

    /** Mark given identifier as a reference to a captured variable itself
     *  suppressing dereferencing with the `elem` field.
     *  @group Macros
     */
    def referenceCapturedVariable(vble: Symbol): Tree

    /** Convert type of a captured variable to *Ref type.
     *  @group Macros
     */
    def capturedVariableType(vble: Symbol): Type

    /** Retrieves the untyped list of subpatterns attached to selector dummy of an UnApply node.
     *  Useful in writing quasiquoting macros that do pattern matching.
     */
    def subpatterns(tree: Tree): Option[List[Tree]]

    /** @see MacroDecoratorApi */
    override type Decorators <: MacroDecoratorApi

    /** @see DecoratorApi */
    trait MacroDecoratorApi extends DecoratorApi {
      /** Extension methods for scopes */
      type ScopeDecorator[T <: Scope] <: MacroScopeDecoratorApi[T]

      /** @see [[ScopeDecorator]] */
      implicit def scopeDecorator[T <: Scope](tree: T): ScopeDecorator[T]

      /** @see [[ScopeDecorator]] */
      class MacroScopeDecoratorApi[T <: Scope](val scope: T) {
        /** @see [[MacroInternalApi.enter]] */
        def enter(sym: Symbol): T = internal.enter(scope, sym)

        /** @see [[MacroInternalApi.unlink]] */
        def unlink(sym: Symbol): T = internal.unlink(scope, sym)
      }

      /** @inheritdoc */
      override type TreeDecorator[T <: Tree] <: MacroTreeDecoratorApi[T]

      /** @see [[TreeDecorator]] */
      class MacroTreeDecoratorApi[T <: Tree](override val tree: T) extends TreeDecoratorApi[T](tree) {
        /** @see [[MacroInternalApi.changeOwner]] */
        def changeOwner(prev: Symbol, next: Symbol): tree.type = internal.changeOwner(tree, prev, next)

        /** @see [[MacroInternalApi.attachments]] */
        def attachments: Attachments { type Pos = Position } = internal.attachments(tree)

        /** @see [[MacroInternalApi.updateAttachment]] */
        def updateAttachment[A: ClassTag](attachment: A): tree.type = internal.updateAttachment(tree, attachment)

        /** @see [[MacroInternalApi.removeAttachment]] */
        def removeAttachment[A: ClassTag]: T = internal.removeAttachment[A](tree)

        /** @see [[MacroInternalApi.setPos]] */
        def setPos(newpos: Position): T = internal.setPos(tree, newpos)

        /** @see [[MacroInternalApi.setType]] */
        def setType(tp: Type): T = internal.setType(tree, tp)

        /** @see [[MacroInternalApi.defineType]] */
        def defineType(tp: Type): T = internal.defineType(tree, tp)

        /** @see [[MacroInternalApi.setSymbol]] */
        def setSymbol(sym: Symbol): T = internal.setSymbol(tree, sym)
      }

      /** Extension methods for typetrees */
      type TypeTreeDecorator[T <: TypeTree] <: MacroTypeTreeDecoratorApi[T]

      /** @see [[TypeTreeDecorator]] */
      implicit def typeTreeDecorator[T <: TypeTree](tt: T): TypeTreeDecorator[T]

      /** @see [[TypeTreeDecorator]] */
      class MacroTypeTreeDecoratorApi[T <: TypeTree](val tt: T) {
        /** @see [[MacroInternalApi.setOriginal]] */
        def setOriginal(tree: Tree): TypeTree = internal.setOriginal(tt, tree)
      }

      /** @inheritdoc */
      override type SymbolDecorator[T <: Symbol] <: MacroSymbolDecoratorApi[T]

      /** @see [[TreeDecorator]] */
      class MacroSymbolDecoratorApi[T <: Symbol](override val symbol: T) extends SymbolDecoratorApi[T](symbol) {
        /** @see [[MacroInternalApi.attachments]] */
        def attachments: Attachments { type Pos = Position } = internal.attachments(symbol)

        /** @see [[MacroInternalApi.updateAttachment]] */
        def updateAttachment[A: ClassTag](attachment: A): T = internal.updateAttachment(symbol, attachment)

        /** @see [[MacroInternalApi.removeAttachment]] */
        def removeAttachment[A: ClassTag]: T = internal.removeAttachment[A](symbol)

        /** @see [[MacroInternalApi.setOwner]] */
        def setOwner(newowner: Symbol): T = internal.setOwner(symbol, newowner)

        /** @see [[MacroInternalApi.setInfo]] */
        def setInfo(tpe: Type): T = internal.setInfo(symbol, tpe)

        /** @see [[MacroInternalApi.setAnnotations]] */
        def setAnnotations(annots: Annotation*): T = internal.setAnnotations(symbol, annots: _*)

        /** @see [[MacroInternalApi.setName]] */
        def setName(name: Name): T = internal.setName(symbol, name)

        /** @see [[MacroInternalApi.setPrivateWithin]] */
        def setPrivateWithin(sym: Symbol): T = internal.setPrivateWithin(symbol, sym)

        /** @see [[MacroInternalApi.setFlag]] */
        def setFlag(flags: FlagSet): T = internal.setFlag(symbol, flags)

        /** @see [[MacroInternalApi.setFlag]] */
        def resetFlag(flags: FlagSet): T = internal.resetFlag(symbol, flags)
      }
    }
  }

  /** @group Internal */
  trait TreeGen {
    /** Builds a reference to value whose type is given stable prefix.
     *  The type must be suitable for this.  For example, it
     *  must not be a TypeRef pointing to an abstract type variable.
     */
    def mkAttributedQualifier(tpe: Type): Tree

    /** Builds a reference to value whose type is given stable prefix.
     *  If the type is unsuitable, e.g. it is a TypeRef for an
     *  abstract type variable, then an Ident will be made using
     *  termSym as the Ident's symbol.  In that case, termSym must
     *  not be NoSymbol.
     */
    def mkAttributedQualifier(tpe: Type, termSym: Symbol): Tree

    /** Builds a typed reference to given symbol with given stable prefix. */
    def mkAttributedRef(pre: Type, sym: Symbol): RefTree

    /** Builds a typed reference to given symbol. */
    def mkAttributedRef(sym: Symbol): RefTree

    def stabilize(tree: Tree): Tree

    def mkAttributedStableRef(pre: Type, sym: Symbol): Tree

    def mkAttributedStableRef(sym: Symbol): Tree

    /** Builds an untyped reference to given symbol. Requires the symbol to be static. */
    def mkUnattributedRef(sym: Symbol): RefTree

    /** Builds an untyped reference to symbol with given name. Requires the symbol to be static. */
    def mkUnattributedRef(fullName: Name): RefTree

    /** Builds a typed This reference to given symbol. */
    def mkAttributedThis(sym: Symbol): This

    /** Builds a typed Ident with an underlying symbol. */
    def mkAttributedIdent(sym: Symbol): RefTree

    /** Builds a typed Select with an underlying symbol. */
    def mkAttributedSelect(qual: Tree, sym: Symbol): RefTree

    /** A creator for method calls, e.g. fn[T1, T2, ...](v1, v2, ...)
     *  There are a number of variations.
     *
     *  @param    receiver    symbol of the method receiver
     *  @param    methodName  name of the method to call
     *  @param    targs       type arguments (if Nil, no TypeApply node will be generated)
     *  @param    args        value arguments
     *  @return               the newly created trees.
     */
    def mkMethodCall(receiver: Symbol, methodName: Name, targs: List[Type], args: List[Tree]): Tree

    def mkMethodCall(method: Symbol, targs: List[Type], args: List[Tree]): Tree

    def mkMethodCall(method: Symbol, args: List[Tree]): Tree

    def mkMethodCall(target: Tree, args: List[Tree]): Tree

    def mkMethodCall(receiver: Symbol, methodName: Name, args: List[Tree]): Tree

    def mkMethodCall(receiver: Tree, method: Symbol, targs: List[Type], args: List[Tree]): Tree

    def mkMethodCall(target: Tree, targs: List[Type], args: List[Tree]): Tree

    def mkNullaryCall(method: Symbol, targs: List[Type]): Tree

    /** A tree that refers to the runtime reflexive universe, `scala.reflect.runtime.universe`. */
    def mkRuntimeUniverseRef: Tree

    def mkZero(tp: Type): Tree

    def mkCast(tree: Tree, pt: Type): Tree
  }

  @deprecated("use `internal.gen` instead", "2.11.0")
  val treeBuild: TreeGen

  /** @inheritdoc */
  @deprecated("compatibility with Scala 2.10 EOL", "2.13.0")
  type Compat <: MacroCompatApi

  /** @see [[compat]]
   *  @group Internal
   */
  @deprecated("compatibility with Scala 2.10 EOL", "2.13.0")
  trait MacroCompatApi extends CompatApi {
    /** Scala 2.10 compatibility enrichments for Symbol. */
    @deprecated("compatibility with Scala 2.10 EOL", "2.13.0")
    implicit class MacroCompatibleSymbol(symbol: Symbol)

    /** Scala 2.10 compatibility enrichments for TypeTree. */
    @deprecated("compatibility with Scala 2.10 EOL", "2.13.0")
    implicit class MacroCompatibleTree(tree: Tree)

    /** Scala 2.10 compatibility enrichments for TypeTree. */
    @deprecated("compatibility with Scala 2.10 EOL", "2.13.0")
    implicit class CompatibleTypeTree(tt: TypeTree)
  }

  /** The type of compilation runs.
   *  @see [[scala.reflect.macros.Enclosures]]
   *  @template
   *  @group Macros
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  type Run <: RunContextApi

  /** Compilation run uniquely identifies current invocation of the compiler
   *  (e.g. can be used to implement per-run caches for macros) and provides access to units of work
   *  of the invocation (currently processed unit of work and the list of all units).
   *  @see [[scala.reflect.macros.Enclosures]]
   *  @group API
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  trait RunContextApi {
    /** Currently processed unit of work (a real or a virtual file). */
    @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
    def currentUnit: CompilationUnit

    /** All units of work comprising this compilation run. */
    @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
    def units: Iterator[CompilationUnit]
  }

  /** The type of compilation units.
   *  @see [[scala.reflect.macros.Enclosures]]
   *  @template
   *  @group Macros
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  type CompilationUnit <: CompilationUnitContextApi

  /** Compilation unit describes a unit of work of the compilation run.
   *  It provides such information as file name, textual representation of the unit and the underlying AST.
   *  @see [[scala.reflect.macros.Enclosures]]
   *  @group API
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  trait CompilationUnitContextApi {
    /** Source file corresponding to this compilation unit.
     *
     *  Exposes information about the file as a part of a real or virtual file system
     *  along with the contents of that file.
     *
     *  The return type is `scala.reflect.io.AbstractFile`, which belongs to an experimental part of Scala reflection.
     *  It should not be used unless you know what you are doing. In subsequent releases, this API will be refined
     *  and exposed as a part of scala.reflect.api.
     */
    @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
    def source: scala.reflect.internal.util.SourceFile

    /** The AST that corresponds to this compilation unit. */
    @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
    def body: Tree
  }
}
