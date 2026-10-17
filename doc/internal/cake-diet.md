# Cake diet: implementation traits out of the compiler cake

## The cake today

The compiler's universe is one object. `reflect.internal.SymbolTable` is an abstract class that mixes in about 40 traits (`Names`, `Symbols`, `Types`, `Trees`, `Definitions`, `Kinds`, `Variances`, ...), each declared with `self: SymbolTable =>`. `nsc.Global` extends it and mixes in another 11, and `interactive.Global`, `ReflectGlobal` and the runtime `JavaUniverse` add more. Every member of every trait is a member of the universe, so code anywhere in the compiler can call `lub(ts)` or `varianceInType(tp)(tparam)` unqualified, and plugins and macros can call them as `global.lub(ts)`.

On the JVM each of those traits is an interface implemented by the universe class. `Global` implements 70 interfaces, `ReflectGlobal` 81. HotSpot's interface type checks degrade once a class has more than 63 secondary supers, and megamorphic interface calls scan the itable linearly; the PR description and the survey in retronym/scala#149 have the details and measurements. The upshot is that every trait mixed into the universe has a cost even if nothing outside it cares that it is a trait.

The traits fall into three groups:

- **Public API**: the 20 `scala.reflect.api` traits behind `scala.reflect.api.Universe`. Under MiMa; they stay.
- **Universe-defining traits**: `Names`, `Symbols`, `Types`, `Trees`, `Scopes`, `Definitions`, `StdNames`, `StdAttachments`, `TypeMaps`, ... They declare the classes and objects that the rest of the compiler names as `global.Symbol`, `global.TypeRef`, `global.nme`. Taking them out would mean a type alias and companion forwarder per class, or merging very large files. They stay.
- **Implementation traits**: the rest. These are the subject of this work.

## Implementation traits

An implementation trait is a trait mixed into the universe for no other reason than to break up what would otherwise be a very long file. It is not designed to be extended or mixed in by third parties, and it is not part of the published API. It has a handful of entry points used by the rest of the compiler, and its other members are used only by itself. If it declares classes, few of them escape, and those that do are either extended by phases and plugins (`InfoTransformer`, `AnnotationChecker`) or used as data (`KindErrors`, `DocComment`).

A rule of thumb: an implementation trait could have been written as a separate object with an `import` in the files that use it, if only that object could see the universe's path-dependent types. Making it a class with a `self` reference to the universe provides exactly that.

This branch takes out: `StdCreators`, `ReificationSupport`, `TypeDebugging`, `CapturedVariables`, `PrivateWithin`, `Variances`, `ExistentialsAndSkolems`, `pickling.Translations`, `Kinds`, `InfoTransformers`, `AnnotationCheckers`, `FreshNames` from `SymbolTable`; `tpe.GlbLubs`, `tpe.CommonOwners`, `tpe.TypeToStrings`, `tpe.FindMembers` from `Types`; `PhaseAssembly` and `ast.DocComments` from `Global`.

## Worked example: `Variances`

Before, `Variances` is a trait of the universe:

```scala
trait Variances { self: SymbolTable =>
  import definitions.uncheckedVarianceClass

  class VarianceValidator extends InternalTraverser { ... }   // extended by RefChecks

  def varianceInTypes(tps: List[Type])(tparam: Symbol): Variance = ...
  def varianceInType(tp: Type, considerUnchecked: Boolean = false)(tparam: Symbol): Variance = ...

  private[this] val varianceInTypeCache =
    ReusableInstance[varianceInType](new varianceInType, enabled = isCompilerUniverse)
  ...
}

abstract class SymbolTable extends ... with Variances with ...
```

Callers in `Infer`, `Implicits` and `Validators` call `varianceInType(...)` unqualified, and `RefChecks` declares `object varianceValidator extends VarianceValidator`.

The first step is the same in both options below. The trait becomes a class that takes the universe as a constructor parameter and imports its members, and the universe creates one instance:

```scala
class Variances[U <: SymbolTable with Singleton](val self: U) {
  import self._
  ... // body unchanged
}

abstract class SymbolTable extends ... {   // Variances no longer mixed in
  val variances: Variances[SymbolTable.this.type] = new Variances[SymbolTable.this.type](this)
}
```

Typing `self` as the universe's singleton type keeps the link between the instance and its universe, so `variances.varianceInType(tp)(tparam)` accepts `tp: global.Type` and `variances.VarianceValidator` sees `global.Tree`. The body of the class is unchanged; `git blame` survives.

### Option A: composition

Call sites go through the new prefix:

```scala
// Infer.scala
- try instantiateToBound(tvar, varianceInTypes(formals)(tparam))
+ try instantiateToBound(tvar, variances.varianceInTypes(formals)(tparam))
```

or, where a file uses a member often, import it once (`Typers` has `import kinds.Kind`).

The universe gains one member (`variances`) and nothing else. Code outside the repository that called `global.varianceInType` no longer compiles and, if already compiled, no longer links; it can call `global.variances.varianceInType` instead.

### Option B: composition with forwarders

The universe keeps the old members as forwarders and aliases, so call sites and existing binaries are unchanged:

```scala
abstract class SymbolTable extends ... {
  val variances: Variances[SymbolTable.this.type] = ...
  type VarianceValidator = variances.VarianceValidator
  final def varianceInType(tp: Type, considerUnchecked: Boolean = false)(tparam: Symbol): Variance =
    variances.varianceInType(tp, considerUnchecked)(tparam)
}
```

A forwarder on a class is a vtable method and adds no interface. A call `global.varianceInType(...)` compiled against `Global` was already `invokevirtual Global.varianceInType`, so plugins and macros keep linking. `object varianceValidator extends VarianceValidator` in `RefChecks` still compiles through the alias.

The forwarders do not have to be `final`. Where a subclass of the universe overrides a member, the member stays on the universe as an ordinary method and the extracted class calls back through `self`. Runtime reflection is the case in point: `SynchronizedTypes` replaces the caches of `GlbLubs`, `TypeToStrings` and `CommonOwners` with thread-local versions:

```scala
trait Types ... {
  private[this] val _lubResults = new mutable.HashMap[(Depth, List[Type]), Type]
  def lubResults = _lubResults                               // state stays on the universe
  final def lub(ts: List[Type]): Type = glbLubs.lub(ts)      // behaviour moves out
}

private[reflect] trait SynchronizedTypes extends internal.Types { self: SymbolTable =>
  override def lubResults = _lubResults.get                  // unchanged
}

class GlbLubs[U <: SymbolTable with Singleton](val self: U) {
  import self._
  ... lubResults.get(...) ...                                // resolves to self.lubResults
}
```

The first version of this branch used option B throughout. That kept each commit to a few lines per trait but left about 100 forwarders, aliases and companion vals on `SymbolTable`, `Types` and `Global`. The census below decided which of them to keep; the rest were replaced by option A.

## Initialization order

Two things can go wrong, and both show up as a `NullPointerException` while the universe is being constructed.

### The component reads `self` in its constructor

```scala
abstract class Universe {
  val definitions = new Definitions
  class Definitions { val AnyClass = "Any" }
  val kinds: Kinds { val self: Universe.this.type }
}

abstract class Kinds {
  val self: Universe
  import self._
  val TopKind = definitions.AnyClass   // runs in Kinds' constructor
}

class Late extends Universe {
  val kinds = new Kinds { val self: Late.this.type = Late.this }
}

class Early extends Universe {
  val kinds = new { val self: Early.this.type = Early.this } with Kinds
}
```

```
early: Any
Exception in thread "main" java.lang.NullPointerException: Cannot invoke "Universe.definitions()" because the return value of "Kinds.self()" is null
	at Kinds.<init>(InitOrder.scala:11)
	at Late$$anon$1.<init>(InitOrder.scala:15)
	at Late.<init>(InitOrder.scala:15)
```

With `new Kinds { val self = ... }`, the anonymous subclass assigns `self` after `Kinds`' constructor has run, and any statement in `Kinds`' body that touches the universe sees `null`. The early initializer `new { val self = ... } with Kinds` assigns `self` first.

The same mistake in the real code: with `variances` created as `new Variances { val self = ... }`, compiling hello world gives

```
Exception in thread "main" java.lang.NullPointerException: Cannot invoke "scala.reflect.internal.SymbolTable.isCompilerUniverse()" because the return value of "scala.reflect.internal.Variances.self()" is null
	at scala.reflect.internal.Variances.<init>(Variances.scala:273)
	at scala.reflect.internal.SymbolTable$$anon$6.<init>(SymbolTable.scala:81)
	at scala.reflect.internal.SymbolTable.<init>(SymbolTable.scala:81)
	at scala.tools.nsc.symtab.SymbolTable.<init>(SymbolTable.scala:16)
	at scala.tools.nsc.Global.<init>(Global.scala:46)
```

because `Variances` builds `varianceInTypeCache` in its constructor with `enabled = isCompilerUniverse`.

The first version of this branch used the late form for six components whose constructors happened not to touch `self`, and early initializers for the rest. Early initializers are deprecated (a migration warning under `-Xsource:3`). All components now take the universe as a constructor parameter, `class Kinds[U <: SymbolTable with Singleton](val self: U)`, which is assigned before the body runs, needs no deprecated syntax, and is shorter at the instantiation site. The singleton bound keeps `kinds.self` typed as the universe's singleton type, so path-dependent types line up as they did with the refinement `{ val self: SymbolTable.this.type }`.

### The universe uses a component before its field is assigned

A trait's body is initialized before the body of the class that mixes it in. A component held in a `val` in `SymbolTable`'s body is therefore still `null` while `Types`, `Definitions` and the other traits initialize:

```scala
trait Definitions { self: Universe =>
  val AnyClass = "Any"
  val AnyKind = kind(AnyClass)   // trait body runs before Universe's body
}

abstract class Universe extends Definitions {
  val kinds = new { val self: Universe.this.type = Universe.this } with Kinds
  final def kind(s: String): String = kinds.kind(s)
}
```

```
Exception in thread "main" java.lang.NullPointerException: Cannot invoke "Kinds.kind(String)" because the return value of "Universe.kinds()" is null
	at Universe.kind(HostOrder.scala:9)
	at Definitions.$init$(HostOrder.scala:4)
	at Universe.<init>(HostOrder.scala:7)
```

How `self` is passed does not matter here: the problem is in the host. The 2025 `cake-diet` branch fixed these by making the component vals lazy, which adds an initialization check to every access, on paths as warm as `lub` and `varianceInType`. On this branch the components are plain vals in `SymbolTable`'s body, or in `Types`' body for the four `tpe` components, and none of them is used before its field is assigned. A component that is needed while an earlier trait initializes has to be declared in that trait or one initialized before it.

## Census of the forwarders

### Method

To find every use of a forwarder, mark it and let the compiler report the uses. `@compileTimeOnly` is the obvious marker, but a use in `reflect` is then an error, `reflect` emits no classes, and the uses in `compiler`, `interactive`, `repl` and `scaladoc` are never reported. `@deprecated` does the same job as a warning:

1. Add `@deprecated("CAKEDIET", "")` to each member the branch added to `SymbolTable`, `Types` and `Global` (a short script over the added lines).
2. For every project, `scalacOptions ++= Seq("-deprecation", "-Xmaxwarns", "100000", "-Wconf:msg=CAKEDIET:w")`, clean, and compile `scaladoc`, `replFrontend`, `sbtBridge`, `scalap`, `partest` and `junit/Test`.
3. Group the warnings by member.

This gives a lower bound. A deprecation warning is not issued inside a deprecated definition, and is silenced by an enclosing `@nowarn`. That hides the forwarders' own references to the component vals, which is useful, but also some real uses: `JavaMirrors` calls `propagatePackageBoundary` from inside the deprecated class `JavaMirror`, and several of `Typers`' uses of `Kind` were not reported. The census is good for deciding what to keep; for the removals themselves, deleting the forwarder and fixing what fails to compile is the authoritative check, and that is how each removal commit was made.

### Results

Reported uses per member across `reflect`, `compiler`, `interactive`, `repl`, `scaladoc` and the JUnit tests.

| component | member | uses | where |
|---|---|---:|---|
| `StdCreators` | `FixedMirrorTreeCreator`, `FixedMirrorTypeCreator` | 1, 2 | macro `contexts/Aliases` |
| `ReificationSupport` | `build` | 60 | quasiquotes, reify, `Internals`, `ToolBoxFactory`, repl |
| | `ReificationSupportImpl` | 0 | |
| `TypeDebugging` | `typeDebug` | 20 | `Typers`, `Infer`, `Implicits`, `TypersTracking` |
| | `noPrint` | 1 | `TypersTracking` |
| | `paramString`, `typeParamsString`, `debugString` | 1, 4, 1 | `Types`, `Symbols` |
| `CapturedVariables` | `captureVariable`, `referenceCapturedVariable`, `capturedVariableType` | 2, 2, 4 | `Internals` (implements the macro API), reify, `LambdaLift`, `Typers` |
| `PrivateWithin` | `propagatePackageBoundary` | 3 (7) | `ClassfileParser`, and `JavaMirrors` (not reported) |
| | `setPackageAccessBoundary` | 0 | |
| `Variances` | `varianceInType`, `varianceInTypes` | 7, 3 | `Infer`, `Implicits`, `Validators` |
| | `VarianceValidator` | 1 | `RefChecks` (extends it) |
| `ExistentialsAndSkolems` | `deriveFreshSkolems`, `isRawParameter`, `existentialTransform`, `packSymbols` | 2, 1, 2, 3 | `Typers`, `Namers`, `UnCurry`, `SpecializeTypes`, `PatternExpansion`, `Types` |
| `Translations` | `picklerTag`, `picklerSubTag`, `isTreeSymbolPickled` | 1, 1, 1 | `Pickler`, `UnPickler` |
| `Kinds` | `kindsConform`, `checkKindBounds0`, `KindErrors` | 2, 1, 2 | `Infer`, `Types` |
| | `Kind`, `ProperTypeKind`, `TypeConKind`, `inferKind` | 7, 1, 3, 3 | `Typers` (more than reported), `IMain` (`:kind`), `JavaUniverseForce` |
| | `NoKindErrors` | 0 | |
| `InfoTransformers` | `InfoTransformer` | 3 | `SymbolTable`; extended by phases through `InfoTransform` |
| `AnnotationCheckers` | `AnnotationChecker`, `addAnnotationChecker`, `removeAllAnnotationCheckers` | 0, 0, 0 | plugin API |
| | `annotationsConform`, `annotationsLub`, `adaptBoundsToAnnotations` | 6, 2, 1 | `TypeComparers`, `GlbLubs`, `Types` |
| | `addAnnotations`, `canAdaptAnnotations`, `adaptAnnotations`, `adaptTypeOfReturn` | 1 each | `AnalyzerPlugins` |
| | `annotationsGlb` | 0 | |
| `FreshNames` | `freshTermName`, `freshTypeName` | 23, 7 | parser, typer, tree gen, reify, everywhere |
| | `FreshNameExtractor` | 4 | `Parsers`, a JUnit test |
| | `globalFreshNameCreator` | 1 | `JavaUniverse` |
| `GlbLubs` | `lub`, `glb` | 23, 12 | everywhere; implement `api.Types` |
| | `weakLub`, `sameWeakLubAsLub` | 3, 3 | `Typers` |
| | `lubResults`, `glbResults` | 5, 5 | `GlbLubs`; overridden by `SynchronizedTypes` |
| | `lubList`, `spanningTypes`, `numericLub`, `GlbFailure` | 0 | |
| `CommonOwners` | `commonOwner`, `commonOwnerMap`, `CommonOwnerMap` | 5, 4, 1 | `GlbLubs`, `Types`; overridden by `SynchronizedTypes` |
| `TypeToStrings` | `typeToString` | 1 | `Types` |
| | `toStringRecursions`, `toStringSubjects` | 1, 0 | overridden by `SynchronizedTypes` |
| `FindMembers` | `FindMembers`, `HasMember`, `findMemberInstance` | 1, 2, 1 | `Types`, `Namers` |
| | `FindMember` | 0 | |
| `PhaseAssembly` | `computePhaseAssembly` | 1 | `Global` |
| `DocComments` | `DocComment` | 26 | parsers, namers, scaladoc, interactive, repl |
| | `docComments`, `expandedDocComment`, `rawDocComment`, `docCommentPos`, `fillDocComment`, `useCases`, `lookupVariable`, `UseCase`, `clearDocComments` | 1 to 3 | scaladoc, `interactive.Global`, `Typers` |
| | `cookedDocComments`, `cookedDocComment`, `replaceInheritDocToInheritdoc`, `merge`, `expandInheritdoc`, `ExpansionLimitExceeded` | 0 | |

Notes:

- Only `lub` and `glb` implement public API (`api.Types`). `freshTermName`/`freshTypeName` and the `CapturedVariables` methods also appear in `api.Internals` and `macros.Universe`, but there they are members of `ReificationSupportApi` and `MacroInternalApi`, which `Internals` implements by delegating; they do not need to be members of the universe.
- The repository is not the whole world: plugins and macros can cast to `Global` and call anything. A GitHub code search for the less common names (`existentialTransform`, `packSymbols`, `deriveFreshSkolems`, `varianceInType`, `kindsConform`, `inferKind`, `propagatePackageBoundary`, `expandedDocComment`, `cookedDocComment`, `addAnnotationChecker`) finds almost only copies of the compiler; the exceptions are a few research plugins (for example miniboxing's use of `deriveFreshSkolems`). Metals does not use the `DocComments` forwarders. scala-compiler has no binary compatibility promise across 2.13 minor releases, and plugins are cross-published per full version, so a source break here costs a plugin author a change from `global.x` to `global.component.x`.

## What changed after the census

### Removed: forwarders nobody uses

`ReificationSupportImpl`, `setPackageAccessBoundary`, `NoKindErrors`, `annotationsGlb`, `lubList`, `spanningTypes`, `numericLub`, `GlbFailure`, `FindMember`, `cookedDocComments`, `cookedDocComment`, `replaceInheritDocToInheritdoc`, `merge`, `expandInheritdoc`, `ExpansionLimitExceeded`.

### Rewritten to option A

One commit per component, each compiling:

- `Translations`: the pickler and unpickler call `translations.picklerTag(...)` etc.
- `PrivateWithin`: `ClassfileParser` and `JavaMirrors` call `packageAccessBoundaries.propagatePackageBoundary(...)`.
- `CapturedVariables`: `Internals` (the macro API), reify, `LambdaLift` and `Typers` go through `capturedVariables`.
- `ExistentialsAndSkolems`: `Typers`, `Namers`, `SpecializeTypes`, `PatternExpansion`, `UnCurry`, `Types`.
- `Variances`: `varianceInType`/`varianceInTypes` in `Infer`, `Implicits`, `Validators`.
- `Kinds`: `Infer`, `Types`, `Typers` (`import kinds.Kind`), and the REPL's `:kind` (`import global.kinds.{Kind, TypeConKind, inferKind}`).
- `TypeDebugging`'s string helpers in `Types` and `Symbols`.
- `AnnotationCheckers`' internal entry points in `TypeComparers`, `GlbLubs`, `Types` and `AnalyzerPlugins`. `annotationsConform` is on the subtyping path, but a call through the component val is the same field load and virtual call that the forwarder does after inlining.
- `StdCreators`: the macro context's `Aliases` imports the creators from the static object `StdCreators`.
- `GlbLubs`' `weakLub` and `sameWeakLubAsLub` in `Typers`; `globalFreshNameCreator` in `JavaUniverse`; `commonOwner` and `typeToString` in `Types` and `GlbLubs`.
- `DocComments` other than `DocComment` and `docComments`: scaladoc, `interactive.Global`, `Typers` and one JUnit test go through `docCommentsSupport`.

### Kept on the universe

- `lub`, `glb`: public API.
- `build`: 60 uses across quasiquotes, reify and the toolbox.
- `freshTermName`, `freshTypeName`, `FreshNameExtractor`: about 30 uses spread over the parser, typer and tree generation, and the obvious thing for a plugin to call.
- `typeDebug`, `noPrint`: the typer's tracing uses them throughout.
- `InfoTransformer`, `AnnotationChecker`, `addAnnotationChecker`, `removeAllAnnotationCheckers`, `VarianceValidator`: extension points for phases and plugins.
- `DocComment` and `docComments`: the parsers, namers and REPL use them.
- `FindMembers`, `findMemberInstance`, `HasMember`: inside `Type`, the component val `findMembers` is shadowed by the method `Type.findMembers`, and `Namers` uses `HasMember` from outside `reflect.internal`.
- `computePhaseAssembly`: a public method of `Global` before this change, not a forwarder it added.
- The state that runtime reflection overrides: `lubResults`, `glbResults`, `indent`, `toStringRecursions`, `toStringSubjects`, `commonOwnerMap`, `CommonOwnerMap`. These are the universe's state, and they stay where `SynchronizedTypes` can override them.

### Other changes

- All components take the universe as a constructor parameter (see above).
- Component vals on `SymbolTable` and `Global` are public, so that a plugin that used a removed forwarder can switch to `global.kinds.inferKind` and the like. Those on `Types` are `private[internal]`, as are their classes, except `glbLubs`, which the typer uses.
- Component vals are named after their trait, except where that name is taken: `infoTransformers` is an existing `var` on `SymbolTable`, `annotationCheckers` a field inside `AnnotationCheckers`, `docComments` the comment table on `Global`, and `privateWithin` a method of `Symbol`. Those are `infoTransformerSupport`, `annotationCheckerSupport`, `docCommentsSupport` and `packageAccessBoundaries`.
- `JavaUniverseForce` is regenerated by `run/t6240-universe-code-gen`, which now also walks `typeDebugging`, `kinds` and `freshNames`, the components that define objects.
