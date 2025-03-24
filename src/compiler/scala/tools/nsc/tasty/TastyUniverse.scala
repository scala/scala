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

package scala.tools.nsc.tasty

import bridge._

/**A facade to `scala.tools.nsc.symbtab.SymbolTable`, providing operations that map from the language of TASTy to the
 * nsc compiler, e.g. to create trees, resolve types and symbols.
 */
abstract class TastyUniverse extends TastyCore
  with FlagOps
  with TypeOps
  with AnnotationOps
  with ContextOps
  with SymbolOps
  with NameOps
  with TreeOps
