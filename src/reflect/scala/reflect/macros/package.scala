package scala.reflect

/** Scala macros.
 *
 *  === Overview ===
 *
 *  Macros are functions that are called by the compiler during compilation.
 *  Within these functions the programmer has access to compiler APIs exposed in [[scala.reflect.macros.Context]].
 *  For example, it is possible to generate, analyze and typecheck code.
 *
 *  Since the 2.10.0 Scala includes macros that can be enabled
 *  with `import language.experimental.macros` on per-file basis
 *  or with `-language:experimental.macros` on per-compilation basis.
 *
 *  Macros significantly simplify code analysis and code generation, which makes them a tool of choice for
 *  a multitude of [[http://scalamacros.org/usecases/index.html real-world use cases]].
 *  Scenarios that traditionally involve writing and maintaining boilerplate can be addressed
 *  with macros in concise and maintainable way.
 *
 *  === Writing macros ===
 *
 *  This documentation page explains a type-safe `printf` macro through an end-to-end example.
 *  To follow the example, create a file named <code>Macros.scala</code> and paste the following
 *  code (be sure to follow the comments in the code, they reveal important things to know about
 *  the macro system, accompanying APIs and infrastructure):
 *
 *  {{{
 *  import scala.reflect.macros.Context
 *  import collection.mutable.ListBuffer
 *  import collection.mutable.Stack
 *
 *  object Macros {
 *    // macro definition is a normal function with almost no restrictions on its signature
 *    // its body, though, is nothing more that a reference to an implementation
 *    def printf(format: String, params: Any*): Unit = macro impl
 *
 *    // macro implementation must correspond to macro definitions that use it
 *    // required signature is quite involved, but the compiler knows what it wants
 *    // should a mismatch occur, it will print the expected signature in the error message
 *    def impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
 *      // STEP I: compiler API is exposed in scala.reflect.macros.Context
 *      // its most important part, reflection API, is accessible via c.universe
 *      // it's customary to import c.universe._
 *      // because it includes a lot of routinely used types and functions
 *      import c.universe._
 *
 *      // STEP II: the macro starts with parsing the provided format string
 *      // macros run during the compile-time, so they operate on trees, not on values
 *      // this means that the format parameter of the macro will be a compile-time literal
 *      // not an object of type java.lang.String
 *      // this also means that the code below won't work for printf("%d" + "%d", ...)
 *      // because in that case format won't be a string literal
 *      // but rather an abstract syntax that represents addition of two string literals
 *      val Literal(Constant(s_format: String)) = format.tree
 *
 *      // STEP IIIa: after parsing the format string, the macro needs to generate the code
 *      // that will partially perform formatting at compile-time
 *      // the paragraph below creates temporary vals that precompute the arguments
 *      // to learn about dynamic generation of Scala code, follow the documentation
 *      // on trees, available in the scala.reflect.api package
 *      val evals = ListBuffer[ValDef]()
 *      def precompute(value: Tree, tpe: Type): Ident = {
 *        val freshName = newTermName(c.fresh("eval$"))
 *        evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
 *        Ident(freshName)
 *      }
 *
 *      // STEP IIIb: tree manipulations proceed in this code snippet
 *      // the snippet extracts trees from parameters of a macro and transforms them
 *      // note the use of typeOf to create Scala types corresponding to forma specifiers
 *      // information on types can be found in the docs for the scala.reflect.api package
 *      val paramsStack = Stack[Tree]((params map (_.tree)): _*)
 *      val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
 *        case "%d" => precompute(paramsStack.pop, typeOf[Int])
 *        case "%s" => precompute(paramsStack.pop, typeOf[String])
 *        case "%%" => Literal(Constant("%"))
 *        case part => Literal(Constant(part))
 *      }
 *
 *      // STEP IV: the code that has been generated is now combined into a Block
 *      // note the call to reify, which provides a shortcut for creating ASTs
 *      // reify is discussed in details in docs for scala.reflect.api.Universe
 *      val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
 *      c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
 *    }
 *  }
 *  }}}
 *
 *  To summarize the code provided above, macros are mini-compiler plugins that get executed whenever a compiler
 *  comes across an invocation of a method declared as a macro. Such methods, dubbed ''macro definitions'', use
 *  the `macro` keyword to reference ''macro implementations''.
 *
 *  Macro implementations use [[scala.reflect.api.package reflection API]] to communicate
 *  with the compiler. The gateway to that API is `c`, a ubiquitous parameter of type [[scala.reflect.macros.Context]]
 *  that must be present in all macro implementations. Compiler universe is available through `c.universe`.
 *
 *  Input arguments to a macro implementation are [[scala.reflect.api.Trees abstract syntax trees]], which
 *  correspond to the arguments of the method invocation that triggered a macro expansion. These trees
 *  are wrapped in [[scala.reflect.api.Exprs exprs]], typed wrappers over trees.
 *
 *  The end result produced by a macro implementation is an abstract syntax tree
 *  wrapped in an expr. This tree represents the code that the compiler will use
 *  to replace the original method invocation.
 *  To learn more about how to create trees that correspond to given Scala code and how to perform
 *  tree manipulations, visit [[scala.reflect.api.Trees the documentation page on trees]].
 *
 *  === Compiling macros ===
 *
 *  In 2.10.0 macros are an experimental feature, so they need to be enabled before use.
 *  Normal compilation of the snippet written above (using `scalac Macros.scala`) fails as follows:
 *
 *  {{{
 *  C:/Projects/Kepler/sandbox>scalac Macros.scala
 *  Macros.scala:8: error: macro definition needs to be enabled
 *  by making the implicit value language.experimental.macros visible.
 *  This can be achieved by adding the import clause 'import language.experimental.macros'
 *  or by setting the compiler option -language:experimental.macros.
 *  See the Scala docs for value scala.language.experimental.macros for a discussion
 *  why the feature needs to be explicitly enabled.
 *    def printf(format: String, params: Any*): Unit = macro printf_impl
 *        ^
 *  one error found
 *  }}}
 *
 *  To enable macros one should use either `import language.experimental.macros` on per-file basis
 *  or `-language:experimental.macros` (providing a compiler switch) on per-compilation basis.
 *
 *  {{{
 *  C:/Projects/Kepler/sandbox>scalac -language:experimental.macros Macros.scala
 *  <scalac has exited with code 0>
 *  }}}
 *
 *  === Using macros ===
 *
 *  Create a file named <code>Test.scala</code> and paste the following code (just as simple as that,
 *  to use a macro, it's only necessary to import it and call it as it were a regular function).
 *
 *  {{{
 *  object Test extends App {
 *    import Macros._
 *    printf("hello %s!", "world")
 *  }
 *  }}}
 *
 *  An important rule about using macros is separate compilation. To perform macro expansion, compiler
 *  needs a macro implementation in executable form. Thus macro implementations need to be compiled before
 *  the main compilation, otherwise compiler will produce `macro implementation not found` errors.
 *
 *  In the REPL, however, macros and their usages can be written in the same session. That's because
 *  the REPL compiles every line of input in a separate compilation run.
 *
 *  {{{
 *  C:/Projects/Kepler/sandbox>scalac Test.scala
 *  <scalac has exited with code 0>
 *
 *  C:/Projects/Kepler/sandbox>scala Test
 *  hello world!
 *  }}}
 *
 *  The test snippet seems to work! To see what happens under the covers, enable the `-Ymacro-debug-lite` compiler flag.
 *
 *  {{{
 *  C:/Projects/Kepler/sandbox>scalac -Ymacro-debug-lite Test.scala
 *  typechecking macro expansion Macros.printf("hello %s!", "world") at
 *  source-C:/Projects/Kepler/sandbox\Test.scala,line-3,offset=52
 *  {
 *    val eval$1: String = "world";
 *    scala.this.Predef.print("hello ");
 *    scala.this.Predef.print(eval$1);
 *    scala.this.Predef.print("!");
 *    ()
 *  }
 *  Block(List(
 *  ValDef(Modifiers(), newTermName("eval$1"), TypeTree(String), Literal(Constant("world"))),
 *  Apply(
 *    Select(Select(This(newTypeName("scala")), newTermName("Predef")), newTermName("print")),
 *    List(Literal(Constant("hello")))),
 *  Apply(
 *    Select(Select(This(newTypeName("scala")), newTermName("Predef")), newTermName("print")),
 *    List(Ident(newTermName("eval$1")))),
 *  Apply(
 *    Select(Select(This(newTypeName("scala")), newTermName("Predef")), newTermName("print")),
 *    List(Literal(Constant("!"))))),
 *  Literal(Constant(())))
 *  }}}
 *
 *  With `-Ymacro-debug-lite` one can see both pseudo-Scala representation of the code generated by macro expansion
 *  and raw AST representation of the expansion. Both have their merits: the former is useful for surface analysis,
 *  while the latter is invaluable for fine-grained debugging.
 *
 *  === Writing bigger macros ===
 *
 *  When the code of a macro implementation grows big enough to warrant modularization beyond the body
 *  of the implementation method, it becomes apparent that one needs to carry around the context parameter,
 *  because most things of interest are path-dependent on the context.
 *
 *  One of the approaches is to write a class that takes a parameter of type `Context` and then split the
 *  macro implementation into a series of methods of that class. This is natural and simple, except that
 *  it's hard to get it right. Here's a typical compilation error.
 *
 *  {{{
 *  scala> class Helper(val c: Context) {
 *       | def generate: c.Tree = ???
 *       | }
 *  defined class Helper
 *
 *  scala> def impl(c: Context): c.Expr[Unit] = {
 *       | val helper = new Helper(c)
 *       | c.Expr(helper.generate)
 *       | }
 *  <console>:32: error: type mismatch;
 *   found   : helper.c.Tree
 *      (which expands to)  helper.c.universe.Tree
 *   required: c.Tree
 *      (which expands to)  c.universe.Tree
 *         c.Expr(helper.generate)
 *                       ^
 *  }}}
 *
 *  The problem in this snippet is in a path-dependent type mismatch. The Scala compiler
 *  does not understand that `c` in `impl` is the same object as `c` in `Helper`, even though the helper
 *  is constructed using the original `c`.
 *
 *  Luckily just a small nudge is all that is needed for the compiler to figure out what's going on.
 *  One of the possible ways of doing that is using refinement types (the example below is the simplest
 *  application of the idea; for example, one could also write an implicit conversion from `Context`
 *  to `Helper` to avoid explicit instantiations and simplify the calls).
 *
 *  {{{
 *  scala> abstract class Helper {
 *       | val c: Context
 *       | def generate: c.Tree = ???
 *       | }
 *  defined class Helper
 *
 *  scala> def impl(c1: Context): c1.Expr[Unit] = {
 *       | val helper = new { val c: c1.type = c1 } with Helper
 *       | c1.Expr(helper.generate)
 *       | }
 *  impl: (c1: scala.reflect.macros.Context)c1.Expr[Unit]
 *  }}}
 *
 *  An alternative approach is to use the [[scala.Singleton]] upper bound to express the fact
 *  that `Helper`'s `C` has the same identity as `impl`'s `C` (note that it is mandatory to
 *  explicitly spell out the type argument when instantiating `Helper`).
 *
 *  {{{
 *  scala> class Helper[C <: Context with Singleton](val c: C) {
 *       | def generate: c.Tree = ???
 *       | }
 *  defined class Helper
 *
 *  scala> def impl(c: Context): c.Expr[Unit] = {
 *       | val helper = new Helper[c.type](c)
 *       | c.Expr(helper.generate)
 *       | }
 *  impl: (c: scala.reflect.macros.Context)c.Expr[Unit]
 *  }}}
 */
package object macros {
}