trait Val { val x: Int = 123 }
trait Var { var x: Int = 123 }
trait Lazy { lazy val x: Int = 123 }

trait ValForVal extends Val { val x: Int = 1 } // needs override
trait VarForVal extends Val { var x: Int = 1 } // needs override
trait DefForVal extends Val { def x: Int = 1 } // needs override
trait ValForVar extends Var { val x: Int = 1 } // needs override
trait VarForVar extends Var { var x: Int = 1 } // needs override
trait DefForVar extends Var { def x: Int = 1 } // needs override
trait ValForLazy extends Lazy { val x: Int = 1 } // needs override
trait VarForLazy extends Lazy { var x: Int = 1 } // needs override
trait DefForLazy extends Lazy { def x: Int = 1 } // needs override

trait ValForValOvr extends Val { override val x: Int = 1 } // override ok
trait VarForValOvr extends Val { override var x: Int = 1 } // bad override
trait DefForValOvr extends Val { override def x: Int = 1 } // bad override
trait ValForVarOvr extends Var { override val x: Int = 1 } // bad override -- unsound if used in path and var changes
trait VarForVarOvr extends Var { override var x: Int = 1 } // bad override -- why?
trait DefForVarOvr extends Var { override def x: Int = 1 } // bad override -- why?
trait ValForLazyOvr extends Lazy { override val x: Int = 1 } // bad override -- why?
trait VarForLazyOvr extends Lazy { override var x: Int = 1 } // bad override -- why?
trait DefForLazyOvr extends Lazy { override def x: Int = 1 } // bad override -- why?

class CValForVal extends Val { val x: Int = 1 } // needs override
class CVarForVal extends Val { var x: Int = 1 } // needs override
class CDefForVal extends Val { def x: Int = 1 } // needs override
class CValForVar extends Var { val x: Int = 1 } // needs override
class CVarForVar extends Var { var x: Int = 1 } // needs override
class CDefForVar extends Var { def x: Int = 1 } // needs override
class CValForLazy extends Lazy { val x: Int = 1 } // needs override
class CVarForLazy extends Lazy { var x: Int = 1 } // needs override
class CDefForLazy extends Lazy { def x: Int = 1 } // needs override

class CValForValOvr extends Val { override val x: Int = 1 } // override ok
class CVarForValOvr extends Val { override var x: Int = 1 } // bad override
class CDefForValOvr extends Val { override def x: Int = 1 } // bad override
class CValForVarOvr extends Var { override val x: Int = 1 } // bad override -- unsound if used in path and var changes
class CVarForVarOvr extends Var { override var x: Int = 1 } // bad override -- why?
class CDefForVarOvr extends Var { override def x: Int = 1 } // bad override -- why?
class CValForLazyOvr extends Lazy { override val x: Int = 1 } // bad override -- why?
class CVarForLazyOvr extends Lazy { override var x: Int = 1 } // bad override -- why?
class CDefForLazyOvr extends Lazy { override def x: Int = 1 } // bad override -- why?

class CVal { val x: Int = 123 }
class CVar { var x: Int = 123 }
class CLazy { lazy val x: Int = 123 }

trait ValForCVal extends CVal { val x: Int = 1 } // needs override
trait VarForCVal extends CVal { var x: Int = 1 } // needs override
trait DefForCVal extends CVal { def x: Int = 1 } // needs override
trait ValForCVar extends CVar { val x: Int = 1 } // needs override
trait VarForCVar extends CVar { var x: Int = 1 } // needs override
trait DefForCVar extends CVar { def x: Int = 1 } // needs override
trait ValForCLazy extends CLazy { val x: Int = 1 } // needs override
trait VarForCLazy extends CLazy { var x: Int = 1 } // needs override
trait DefForCLazy extends CLazy { def x: Int = 1 } // needs override

trait ValForCValOvr extends CVal { override val x: Int = 1 } // override ok
trait VarForCValOvr extends CVal { override var x: Int = 1 } // bad override
trait DefForCValOvr extends CVal { override def x: Int = 1 } // bad override
trait ValForCVarOvr extends CVar { override val x: Int = 1 } // bad override -- unsound if used in path and var changes
trait VarForCVarOvr extends CVar { override var x: Int = 1 } // bad override -- why?
trait DefForCVarOvr extends CVar { override def x: Int = 1 } // bad override -- why?
trait ValForCLazyOvr extends CLazy { override val x: Int = 1 } // bad override -- why?
trait VarForCLazyOvr extends CLazy { override var x: Int = 1 } // bad override -- why?
trait DefForCLazyOvr extends CLazy { override def x: Int = 1 } // bad override -- why?

class CValForCVal extends CVal { val x: Int = 1 } // needs override
class CVarForCVal extends CVal { var x: Int = 1 } // needs override
class CDefForCVal extends CVal { def x: Int = 1 } // needs override
class CValForCVar extends CVar { val x: Int = 1 } // needs override
class CVarForCVar extends CVar { var x: Int = 1 } // needs override
class CDefForCVar extends CVar { def x: Int = 1 } // needs override
class CValForCLazy extends CLazy { val x: Int = 1 } // needs override
class CVarForCLazy extends CLazy { var x: Int = 1 } // needs override
class CDefForCLazy extends CLazy { def x: Int = 1 } // needs override

class CValForCValOvr extends CVal { override val x: Int = 1 } // override ok
class CVarForCValOvr extends CVal { override var x: Int = 1 } // bad override
class CDefForCValOvr extends CVal { override def x: Int = 1 } // bad override
class CValForCVarOvr extends CVar { override val x: Int = 1 } // bad override -- unsound if used in path and var changes
class CVarForCVarOvr extends CVar { override var x: Int = 1 } // bad override -- why?
class CDefForCVarOvr extends CVar { override def x: Int = 1 } // bad override -- why?
class CValForCLazyOvr extends CLazy { override val x: Int = 1 } // bad override -- why?
class CVarForCLazyOvr extends CLazy { override var x: Int = 1 } // bad override -- why?
class CDefForCLazyOvr extends CLazy { override def x: Int = 1 } // bad override -- why?
