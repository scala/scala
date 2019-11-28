package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyUniverse

trait TreeOps extends TastyKernel { self: TastyUniverse =>
  import Contexts._

  object Trees {
    /** A base trait for lazy tree fields.
     *  These can be instantiated with Lazy instances which
     *  can delay tree construction until the field is first demanded.
     */
    trait Lazy[+T <: AnyRef] {
      def complete(implicit ctx: Context): T
    }
  }

}
