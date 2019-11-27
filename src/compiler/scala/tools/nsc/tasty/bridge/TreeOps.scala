package scala.tools.nsc.tasty.bridge

trait TreeOps extends TastyKernel {

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
