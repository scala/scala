/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.annotation

/**
 * Methods annotated `@nolink` are emitted normally in bytecode, but they cannot be
 * referenced by client code. This allows adding binary compatible overrides when the
 * bytecode signature is refined.
 *
 * Example:
 *
 * {{{
 *   class A { def f: AnyRef = "A.f" }
 *   class C extends A
 * }}}
 *
 * Adding an override of `f` to `C` and refining the result type is not forwards
 * binary compatible:
 *
 * {{{
 *    class C extends A { override def f: String = "C.f" }
 * }}}
 *
 * The class `C` gets a new method in bytecode with return type `String`. In order
 * to implement overriding, the compiler generates a "bridge" method in `C` which
 * has the same signature as the overridden method (return type `Object`).
 *
 * Annotating the override `@nolink` hides it and avoids client code to refer to
 * to it. So an invocation `(new C).f` resolves to the method `A.f: AnyRef` and
 * uses the signature `f: Object` in bytecode. Since the bridge method for `C.f`
 * overrides the method `A.f`, at run-time the method `C.f` is invoked.
 *
 * The `-Xuse-nolink` compiler flag disables the special handling of methods `@nolink`.
 */
final class nolink extends StaticAnnotation
