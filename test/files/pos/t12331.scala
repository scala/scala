//scalac: -Ymacro-annotations
//-Ymacro-annotations -Yvalidate-pos:_ -Vprint:_ -Vprint-pos

import annotation._

trait T { val x: Int }

@strictfp class C extends { val x = 0 } with T
