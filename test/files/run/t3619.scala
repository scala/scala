class Dep(x: Int)(implicit val nameClash: String)

object Test extends App {
  implicit val nameClash: String = "meh"

  def meth(implicit w: String) = 1

  // when typing Meh's default constructor Meh.this.nameClash (inherited from Dep)
  // shadows Test.nameClash, thus, when inferring the argument `w` in the call to meth,
  // Test.nameClash is not eligible statically, Meh.this.nameClash is picked (which then causes the VerifyError)
  // BUG: Meth.this.nameClash should not be in (the implicit) scope during the super constructor call in the first place
  class Meh extends Dep(meth)
  /*
    class Meh extends Dep {
      def this() {
        this(Test.this.meth(Meh.this.nameClash))(Test.this.nameClash)
      }
    }
  */
  
  new Meh
}


/*
  {
    def this(a: String, b: Int) {
      this()
    }
    def this(x: String) {
      this(Meh.this.nameClash, 1)
    }
  }
*/
