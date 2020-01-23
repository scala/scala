object Test {
  import reflect.runtime.universe._

  def t1[F[_]](implicit  tt:     TypeTag[F[_]]) = tt
  def t2[F[_]](implicit wtt: WeakTypeTag[F[_]]) = wtt
  def t3[F[_]](implicit  tt:     TypeTag[F[Any]]) = tt
  def t4[F[_]](implicit wtt: WeakTypeTag[F[Any]]) = wtt

  trait In[a]; trait Co[+a]; trait Cn[-a]
  t1[In]; t2[In]; t3[In]; t4[In]
  t1[Co]; t2[Co]; t3[Co]; t4[Co]
  t1[Cn]; t2[Cn]; t3[Cn]; t4[Cn]
}