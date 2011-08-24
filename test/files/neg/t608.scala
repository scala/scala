trait CrashDueToTypeError {
  def id[a](x :a) :a = x

  trait Bifunctor {
    type a; //   content
    type s <: Bifunctor

    // uncomment        this-vvvvvvvvvvvvvvvvvvvvvvvvvvvv, and it compiles
    def bimap[c](f :a=>c) :s{/*type s=Bifunctor.this.s;*/type a=c; }
  }

  def hylo[hs <: Bifunctor,ha,hb,hc]
      (f :hb=>hs{type s=hs; type a=ha},
       g :hs{type s=hs; type a=ha}=>hc)(x :hb)
       :hc
       = g(f(x).bimap(id))
}
