object Test {
  val r = new R
  new r.attr() // Was: error while loading attr, class file '.../t7532-pos.obj/R$attr.class' has location not matching its contents: contains class
  new R.attr1
}