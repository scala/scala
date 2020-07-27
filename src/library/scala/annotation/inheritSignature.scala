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
 * Methods annotated `@inheritSignature` are emitted `private` in bytecode, which
 * allows adding binary compatible overrides when the bytecode signature is refined.
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
 * By annotating the override `@inheritSignature`, the new method `C.f: String`
 * is made `private`, only the bridge method remains `public`.
 *
 * Invocations of `C.f: String` are rewritten to `A.f`.
 */
final class inheritSignature extends StaticAnnotation
