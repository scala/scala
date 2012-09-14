package scala.reflect.macros
package internal

/** Links macro definitions with their implementation.
 *  This is necessary to preserve macro def -> macro impl links between compilation runs.
 *
 *  More precisely, after typechecking right-hand side of a macro def
 *  `typedMacroBody` slaps `macroImpl` annotation onto the macro def
 *  with the result of typechecking as a sole parameter.
 *
 *  As an unfortunate consequence, this annotation must be defined in scala-library.jar,
 *  because anyone (even those programmers who compile their programs with only scala-library on classpath)
 *  must be able to define macros.
 *
 *  To lessen the weirdness we define this annotation as `private[scala]`.
 *  It will not prevent pickling, but it will prevent application developers (and scaladocs) from seeing the annotation.
 */
private[scala] class macroImpl(val referenceToMacroImpl: Any) extends scala.annotation.StaticAnnotation
