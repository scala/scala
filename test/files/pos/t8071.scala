class ann extends annotation.StaticAnnotation

object Use {
  def ??? = sys.error("!!!")
  val field = new AnyRef {     // <---- error reported here
    class Mem { def x: (Int @ann) = ??? }
    type Memory = Mem
  }
}

/**
error: type mismatch;
 found   : AnyRef{type Mem(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>) <: AnyRef{def x: Int}; type Memory = this.Mem(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>)}
 required: AnyRef{type Mem(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>) <: AnyRef{def x: Int @ann}; type Memory = this.Mem(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>)(in <refinement of AnyRef>)}
 */
