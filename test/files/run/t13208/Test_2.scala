object Test extends App {
  final val K = 1
  assert(M.m(1).isInstanceOf[java.lang.Long])
  assert(M.m(K).isInstanceOf[java.lang.Long])
  assert(M.m('a').isInstanceOf[java.lang.Long])
}
