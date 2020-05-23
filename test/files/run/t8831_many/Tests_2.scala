object Test extends App {

val ch_Cpv_pv = new Cpv_pv('a', 'b')
val i_Cpv_pv = new Cpv_pv(1, 2)
val Cpv_pv(extracted1i_Cpv_pv, extracted2i_Cpv_pv) = i_Cpv_pv
val Cpv_pv(extracted1ch_Cpv_pv, extracted2ch_Cpv_pv) = ch_Cpv_pv
assert(1 == extracted1i_Cpv_pv)
assert(2 == extracted2i_Cpv_pv)
assert('a' == extracted1ch_Cpv_pv)
assert('b' == extracted2ch_Cpv_pv)

assert(1 == i_Cpv_pv.p1)
assert(2 == i_Cpv_pv.p2)
assert('a' == ch_Cpv_pv.p1)
assert('b' == ch_Cpv_pv.p2)


val ch_Cpv_v = new Cpv_v('a', 'b')
val i_Cpv_v = new Cpv_v(1, 2)
val Cpv_v(extracted1i_Cpv_v, extracted2i_Cpv_v) = i_Cpv_v
val Cpv_v(extracted1ch_Cpv_v, extracted2ch_Cpv_v) = ch_Cpv_v
assert(1 == extracted1i_Cpv_v)
assert(2 == extracted2i_Cpv_v)
assert('a' == extracted1ch_Cpv_v)
assert('b' == extracted2ch_Cpv_v)

assert(2 == i_Cpv_v.a)
assert('b' == ch_Cpv_v.a)
assert(1 == i_Cpv_v.p1)
assert(2 == i_Cpv_v.p2)
assert('a' == ch_Cpv_v.p1)
assert('b' == ch_Cpv_v.p2)


val ch_Cpv_n = new Cpv_n('a', 'b')
val i_Cpv_n = new Cpv_n(1, 2)
val Cpv_n(extracted1i_Cpv_n, extracted2i_Cpv_n) = i_Cpv_n
val Cpv_n(extracted1ch_Cpv_n, extracted2ch_Cpv_n) = ch_Cpv_n
assert(1 == extracted1i_Cpv_n)
assert(2 == extracted2i_Cpv_n)
assert('a' == extracted1ch_Cpv_n)
assert('b' == extracted2ch_Cpv_n)

assert(2 == i_Cpv_n.a)
assert('b' == ch_Cpv_n.a)
assert(1 == i_Cpv_n.p1)
assert(2 == i_Cpv_n.p2)
assert('a' == ch_Cpv_n.p1)
assert('b' == ch_Cpv_n.p2)


val ch_Cv_pv = new Cv_pv('a', 'b')
val i_Cv_pv = new Cv_pv(1, 2)
val Cv_pv(extracted1i_Cv_pv, extracted2i_Cv_pv) = i_Cv_pv
val Cv_pv(extracted1ch_Cv_pv, extracted2ch_Cv_pv) = ch_Cv_pv
assert(1 == extracted1i_Cv_pv)
assert(2 == extracted2i_Cv_pv)
assert('a' == extracted1ch_Cv_pv)
assert('b' == extracted2ch_Cv_pv)

assert(1 == i_Cv_pv.`a b`)
assert('a' == ch_Cv_pv.`a b`)
assert(1 == i_Cv_pv.p1)
assert(2 == i_Cv_pv.p2)
assert('a' == ch_Cv_pv.p1)
assert('b' == ch_Cv_pv.p2)


val ch_Cv_v = new Cv_v('a', 'b')
val i_Cv_v = new Cv_v(1, 2)
val Cv_v(extracted1i_Cv_v, extracted2i_Cv_v) = i_Cv_v
val Cv_v(extracted1ch_Cv_v, extracted2ch_Cv_v) = ch_Cv_v
assert(1 == extracted1i_Cv_v)
assert(2 == extracted2i_Cv_v)
assert('a' == extracted1ch_Cv_v)
assert('b' == extracted2ch_Cv_v)

assert(1 == i_Cv_v.`a b`)
assert(2 == i_Cv_v.a)
assert('a' == ch_Cv_v.`a b`)
assert('b' == ch_Cv_v.a)
assert(1 == i_Cv_v.p1)
assert(2 == i_Cv_v.p2)
assert('a' == ch_Cv_v.p1)
assert('b' == ch_Cv_v.p2)


val ch_Cv_n = new Cv_n('a', 'b')
val i_Cv_n = new Cv_n(1, 2)
val Cv_n(extracted1i_Cv_n, extracted2i_Cv_n) = i_Cv_n
val Cv_n(extracted1ch_Cv_n, extracted2ch_Cv_n) = ch_Cv_n
assert(1 == extracted1i_Cv_n)
assert(2 == extracted2i_Cv_n)
assert('a' == extracted1ch_Cv_n)
assert('b' == extracted2ch_Cv_n)

assert(1 == i_Cv_n.`a b`)
assert(2 == i_Cv_n.a)
assert('a' == ch_Cv_n.`a b`)
assert('b' == ch_Cv_n.a)
assert(1 == i_Cv_n.p1)
assert(2 == i_Cv_n.p2)
assert('a' == ch_Cv_n.p1)
assert('b' == ch_Cv_n.p2)


val ch_Cn_pv = new Cn_pv('a', 'b')
val i_Cn_pv = new Cn_pv(1, 2)
val Cn_pv(extracted1i_Cn_pv, extracted2i_Cn_pv) = i_Cn_pv
val Cn_pv(extracted1ch_Cn_pv, extracted2ch_Cn_pv) = ch_Cn_pv
assert(1 == extracted1i_Cn_pv)
assert(2 == extracted2i_Cn_pv)
assert('a' == extracted1ch_Cn_pv)
assert('b' == extracted2ch_Cn_pv)

assert(1 == i_Cn_pv.`a b`)
assert('a' == ch_Cn_pv.`a b`)
assert(1 == i_Cn_pv.p1)
assert(2 == i_Cn_pv.p2)
assert('a' == ch_Cn_pv.p1)
assert('b' == ch_Cn_pv.p2)


val ch_Cn_v = new Cn_v('a', 'b')
val i_Cn_v = new Cn_v(1, 2)
val Cn_v(extracted1i_Cn_v, extracted2i_Cn_v) = i_Cn_v
val Cn_v(extracted1ch_Cn_v, extracted2ch_Cn_v) = ch_Cn_v
assert(1 == extracted1i_Cn_v)
assert(2 == extracted2i_Cn_v)
assert('a' == extracted1ch_Cn_v)
assert('b' == extracted2ch_Cn_v)

assert(1 == i_Cn_v.`a b`)
assert(2 == i_Cn_v.a)
assert('a' == ch_Cn_v.`a b`)
assert('b' == ch_Cn_v.a)
assert(1 == i_Cn_v.p1)
assert(2 == i_Cn_v.p2)
assert('a' == ch_Cn_v.p1)
assert('b' == ch_Cn_v.p2)


val ch_Cn_n = new Cn_n('a', 'b')
val i_Cn_n = new Cn_n(1, 2)
val Cn_n(extracted1i_Cn_n, extracted2i_Cn_n) = i_Cn_n
val Cn_n(extracted1ch_Cn_n, extracted2ch_Cn_n) = ch_Cn_n
assert(1 == extracted1i_Cn_n)
assert(2 == extracted2i_Cn_n)
assert('a' == extracted1ch_Cn_n)
assert('b' == extracted2ch_Cn_n)

assert(1 == i_Cn_n.`a b`)
assert(2 == i_Cn_n.a)
assert('a' == ch_Cn_n.`a b`)
assert('b' == ch_Cn_n.a)
assert(1 == i_Cn_n.p1)
assert(2 == i_Cn_n.p2)
assert('a' == ch_Cn_n.p1)
assert('b' == ch_Cn_n.p2)


val ch_Rpv_pv = new Rpv_pv('a', 'b')
val i_Rpv_pv = new Rpv_pv(1, 2)
assert(1 == i_Rpv_pv.p1)
assert(2 == i_Rpv_pv.p2)
assert('a' == ch_Rpv_pv.p1)
assert('b' == ch_Rpv_pv.p2)


val ch_Rpv_v = new Rpv_v('a', 'b')
val i_Rpv_v = new Rpv_v(1, 2)
assert(2 == i_Rpv_v.a)
assert('b' == ch_Rpv_v.a)
assert(1 == i_Rpv_v.p1)
assert(2 == i_Rpv_v.p2)
assert('a' == ch_Rpv_v.p1)
assert('b' == ch_Rpv_v.p2)


val ch_Rpv_n = new Rpv_n('a', 'b')
val i_Rpv_n = new Rpv_n(1, 2)
assert(1 == i_Rpv_n.p1)
assert(2 == i_Rpv_n.p2)
assert('a' == ch_Rpv_n.p1)
assert('b' == ch_Rpv_n.p2)


val ch_Rv_pv = new Rv_pv('a', 'b')
val i_Rv_pv = new Rv_pv(1, 2)
assert(1 == i_Rv_pv.`a b`)
assert('a' == ch_Rv_pv.`a b`)
assert(1 == i_Rv_pv.p1)
assert(2 == i_Rv_pv.p2)
assert('a' == ch_Rv_pv.p1)
assert('b' == ch_Rv_pv.p2)


val ch_Rv_v = new Rv_v('a', 'b')
val i_Rv_v = new Rv_v(1, 2)
assert(1 == i_Rv_v.`a b`)
assert(2 == i_Rv_v.a)
assert('a' == ch_Rv_v.`a b`)
assert('b' == ch_Rv_v.a)
assert(1 == i_Rv_v.p1)
assert(2 == i_Rv_v.p2)
assert('a' == ch_Rv_v.p1)
assert('b' == ch_Rv_v.p2)


val ch_Rv_n = new Rv_n('a', 'b')
val i_Rv_n = new Rv_n(1, 2)
assert(1 == i_Rv_n.`a b`)
assert('a' == ch_Rv_n.`a b`)
assert(1 == i_Rv_n.p1)
assert(2 == i_Rv_n.p2)
assert('a' == ch_Rv_n.p1)
assert('b' == ch_Rv_n.p2)


val ch_Rn_pv = new Rn_pv('a', 'b')
val i_Rn_pv = new Rn_pv(1, 2)
assert(1 == i_Rn_pv.p1)
assert(2 == i_Rn_pv.p2)
assert('a' == ch_Rn_pv.p1)
assert('b' == ch_Rn_pv.p2)


val ch_Rn_v = new Rn_v('a', 'b')
val i_Rn_v = new Rn_v(1, 2)
assert(2 == i_Rn_v.a)
assert('b' == ch_Rn_v.a)
assert(1 == i_Rn_v.p1)
assert(2 == i_Rn_v.p2)
assert('a' == ch_Rn_v.p1)
assert('b' == ch_Rn_v.p2)


val ch_Rn_n = new Rn_n('a', 'b')
val i_Rn_n = new Rn_n(1, 2)
assert(1 == i_Rn_n.p1)
assert(2 == i_Rn_n.p2)
assert('a' == ch_Rn_n.p1)
assert('b' == ch_Rn_n.p2)

TestJoint.joint()
}
