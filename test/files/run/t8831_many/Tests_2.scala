object Test extends App {

val ch_Cpvt_pvt = new Cpvt_pvt('a', 'b')
val i_Cpvt_pvt = new Cpvt_pvt(1, 2)
val Cpvt_pvt(extracted1i_Cpvt_pvt, extracted2i_Cpvt_pvt) = i_Cpvt_pvt
val Cpvt_pvt(extracted1ch_Cpvt_pvt, extracted2ch_Cpvt_pvt) = ch_Cpvt_pvt
assert(1 == extracted1i_Cpvt_pvt)
assert(2 == extracted2i_Cpvt_pvt)
assert('a' == extracted1ch_Cpvt_pvt)
assert('b' == extracted2ch_Cpvt_pvt)

assert(1 == i_Cpvt_pvt.p1)
assert(2 == i_Cpvt_pvt.p2)
assert('a' == ch_Cpvt_pvt.p1)
assert('b' == ch_Cpvt_pvt.p2)


val ch_Cpvt_pvf = new Cpvt_pvf('a', 'b')
val i_Cpvt_pvf = new Cpvt_pvf(1, 2)
val Cpvt_pvf(extracted1i_Cpvt_pvf, extracted2i_Cpvt_pvf) = i_Cpvt_pvf
val Cpvt_pvf(extracted1ch_Cpvt_pvf, extracted2ch_Cpvt_pvf) = ch_Cpvt_pvf
assert(1 == extracted1i_Cpvt_pvf)
assert(2 == extracted2i_Cpvt_pvf)
assert('a' == extracted1ch_Cpvt_pvf)
assert('b' == extracted2ch_Cpvt_pvf)

assert(1 == i_Cpvt_pvf.p1)
assert('a' == ch_Cpvt_pvf.p1)


val ch_Cpvt_vt = new Cpvt_vt('a', 'b')
val i_Cpvt_vt = new Cpvt_vt(1, 2)
val Cpvt_vt(extracted1i_Cpvt_vt, extracted2i_Cpvt_vt) = i_Cpvt_vt
val Cpvt_vt(extracted1ch_Cpvt_vt, extracted2ch_Cpvt_vt) = ch_Cpvt_vt
assert(1 == extracted1i_Cpvt_vt)
assert(2 == extracted2i_Cpvt_vt)
assert('a' == extracted1ch_Cpvt_vt)
assert('b' == extracted2ch_Cpvt_vt)

assert(2 == i_Cpvt_vt.a)
assert('b' == ch_Cpvt_vt.a)
assert(1 == i_Cpvt_vt.p1)
assert(2 == i_Cpvt_vt.p2)
assert('a' == ch_Cpvt_vt.p1)
assert('b' == ch_Cpvt_vt.p2)


val ch_Cpvt_vf = new Cpvt_vf('a', 'b')
val i_Cpvt_vf = new Cpvt_vf(1, 2)
val Cpvt_vf(extracted1i_Cpvt_vf, extracted2i_Cpvt_vf) = i_Cpvt_vf
val Cpvt_vf(extracted1ch_Cpvt_vf, extracted2ch_Cpvt_vf) = ch_Cpvt_vf
assert(1 == extracted1i_Cpvt_vf)
assert(2 == extracted2i_Cpvt_vf)
assert('a' == extracted1ch_Cpvt_vf)
assert('b' == extracted2ch_Cpvt_vf)

assert(2 == i_Cpvt_vf.a)
assert('b' == ch_Cpvt_vf.a)
assert(1 == i_Cpvt_vf.p1)
assert('a' == ch_Cpvt_vf.p1)


val ch_Cpvt_nt = new Cpvt_nt('a', 'b')
val i_Cpvt_nt = new Cpvt_nt(1, 2)
val Cpvt_nt(extracted1i_Cpvt_nt, extracted2i_Cpvt_nt) = i_Cpvt_nt
val Cpvt_nt(extracted1ch_Cpvt_nt, extracted2ch_Cpvt_nt) = ch_Cpvt_nt
assert(1 == extracted1i_Cpvt_nt)
assert(2 == extracted2i_Cpvt_nt)
assert('a' == extracted1ch_Cpvt_nt)
assert('b' == extracted2ch_Cpvt_nt)

assert(2 == i_Cpvt_nt.a)
assert('b' == ch_Cpvt_nt.a)
assert(1 == i_Cpvt_nt.p1)
assert(2 == i_Cpvt_nt.p2)
assert('a' == ch_Cpvt_nt.p1)
assert('b' == ch_Cpvt_nt.p2)


val ch_Cpvt_nf = new Cpvt_nf('a', 'b')
val i_Cpvt_nf = new Cpvt_nf(1, 2)
val Cpvt_nf(extracted1i_Cpvt_nf, extracted2i_Cpvt_nf) = i_Cpvt_nf
val Cpvt_nf(extracted1ch_Cpvt_nf, extracted2ch_Cpvt_nf) = ch_Cpvt_nf
assert(1 == extracted1i_Cpvt_nf)
assert(2 == extracted2i_Cpvt_nf)
assert('a' == extracted1ch_Cpvt_nf)
assert('b' == extracted2ch_Cpvt_nf)

assert(2 == i_Cpvt_nf.a)
assert('b' == ch_Cpvt_nf.a)
assert(1 == i_Cpvt_nf.p1)
assert('a' == ch_Cpvt_nf.p1)


val ch_Cpvf_pvt = new Cpvf_pvt('a', 'b')
val i_Cpvf_pvt = new Cpvf_pvt(1, 2)
val Cpvf_pvt(extracted1i_Cpvf_pvt, extracted2i_Cpvf_pvt) = i_Cpvf_pvt
val Cpvf_pvt(extracted1ch_Cpvf_pvt, extracted2ch_Cpvf_pvt) = ch_Cpvf_pvt
assert(1 == extracted1i_Cpvf_pvt)
assert(2 == extracted2i_Cpvf_pvt)
assert('a' == extracted1ch_Cpvf_pvt)
assert('b' == extracted2ch_Cpvf_pvt)

assert(2 == i_Cpvf_pvt.p2)
assert('b' == ch_Cpvf_pvt.p2)


val ch_Cpvf_pvf = new Cpvf_pvf('a', 'b')
val i_Cpvf_pvf = new Cpvf_pvf(1, 2)
val Cpvf_pvf(extracted1i_Cpvf_pvf, extracted2i_Cpvf_pvf) = i_Cpvf_pvf
val Cpvf_pvf(extracted1ch_Cpvf_pvf, extracted2ch_Cpvf_pvf) = ch_Cpvf_pvf
assert(1 == extracted1i_Cpvf_pvf)
assert(2 == extracted2i_Cpvf_pvf)
assert('a' == extracted1ch_Cpvf_pvf)
assert('b' == extracted2ch_Cpvf_pvf)



val ch_Cpvf_vt = new Cpvf_vt('a', 'b')
val i_Cpvf_vt = new Cpvf_vt(1, 2)
val Cpvf_vt(extracted1i_Cpvf_vt, extracted2i_Cpvf_vt) = i_Cpvf_vt
val Cpvf_vt(extracted1ch_Cpvf_vt, extracted2ch_Cpvf_vt) = ch_Cpvf_vt
assert(1 == extracted1i_Cpvf_vt)
assert(2 == extracted2i_Cpvf_vt)
assert('a' == extracted1ch_Cpvf_vt)
assert('b' == extracted2ch_Cpvf_vt)

assert(2 == i_Cpvf_vt.a)
assert('b' == ch_Cpvf_vt.a)
assert(2 == i_Cpvf_vt.p2)
assert('b' == ch_Cpvf_vt.p2)


val ch_Cpvf_vf = new Cpvf_vf('a', 'b')
val i_Cpvf_vf = new Cpvf_vf(1, 2)
val Cpvf_vf(extracted1i_Cpvf_vf, extracted2i_Cpvf_vf) = i_Cpvf_vf
val Cpvf_vf(extracted1ch_Cpvf_vf, extracted2ch_Cpvf_vf) = ch_Cpvf_vf
assert(1 == extracted1i_Cpvf_vf)
assert(2 == extracted2i_Cpvf_vf)
assert('a' == extracted1ch_Cpvf_vf)
assert('b' == extracted2ch_Cpvf_vf)

assert(2 == i_Cpvf_vf.a)
assert('b' == ch_Cpvf_vf.a)


val ch_Cpvf_nt = new Cpvf_nt('a', 'b')
val i_Cpvf_nt = new Cpvf_nt(1, 2)
val Cpvf_nt(extracted1i_Cpvf_nt, extracted2i_Cpvf_nt) = i_Cpvf_nt
val Cpvf_nt(extracted1ch_Cpvf_nt, extracted2ch_Cpvf_nt) = ch_Cpvf_nt
assert(1 == extracted1i_Cpvf_nt)
assert(2 == extracted2i_Cpvf_nt)
assert('a' == extracted1ch_Cpvf_nt)
assert('b' == extracted2ch_Cpvf_nt)

assert(2 == i_Cpvf_nt.a)
assert('b' == ch_Cpvf_nt.a)
assert(2 == i_Cpvf_nt.p2)
assert('b' == ch_Cpvf_nt.p2)


val ch_Cpvf_nf = new Cpvf_nf('a', 'b')
val i_Cpvf_nf = new Cpvf_nf(1, 2)
val Cpvf_nf(extracted1i_Cpvf_nf, extracted2i_Cpvf_nf) = i_Cpvf_nf
val Cpvf_nf(extracted1ch_Cpvf_nf, extracted2ch_Cpvf_nf) = ch_Cpvf_nf
assert(1 == extracted1i_Cpvf_nf)
assert(2 == extracted2i_Cpvf_nf)
assert('a' == extracted1ch_Cpvf_nf)
assert('b' == extracted2ch_Cpvf_nf)

assert(2 == i_Cpvf_nf.a)
assert('b' == ch_Cpvf_nf.a)


val ch_Cvt_pvt = new Cvt_pvt('a', 'b')
val i_Cvt_pvt = new Cvt_pvt(1, 2)
val Cvt_pvt(extracted1i_Cvt_pvt, extracted2i_Cvt_pvt) = i_Cvt_pvt
val Cvt_pvt(extracted1ch_Cvt_pvt, extracted2ch_Cvt_pvt) = ch_Cvt_pvt
assert(1 == extracted1i_Cvt_pvt)
assert(2 == extracted2i_Cvt_pvt)
assert('a' == extracted1ch_Cvt_pvt)
assert('b' == extracted2ch_Cvt_pvt)

assert(1 == i_Cvt_pvt.`a b`)
assert('a' == ch_Cvt_pvt.`a b`)
assert(1 == i_Cvt_pvt.p1)
assert(2 == i_Cvt_pvt.p2)
assert('a' == ch_Cvt_pvt.p1)
assert('b' == ch_Cvt_pvt.p2)


val ch_Cvt_pvf = new Cvt_pvf('a', 'b')
val i_Cvt_pvf = new Cvt_pvf(1, 2)
val Cvt_pvf(extracted1i_Cvt_pvf, extracted2i_Cvt_pvf) = i_Cvt_pvf
val Cvt_pvf(extracted1ch_Cvt_pvf, extracted2ch_Cvt_pvf) = ch_Cvt_pvf
assert(1 == extracted1i_Cvt_pvf)
assert(2 == extracted2i_Cvt_pvf)
assert('a' == extracted1ch_Cvt_pvf)
assert('b' == extracted2ch_Cvt_pvf)

assert(1 == i_Cvt_pvf.`a b`)
assert('a' == ch_Cvt_pvf.`a b`)
assert(1 == i_Cvt_pvf.p1)
assert('a' == ch_Cvt_pvf.p1)


val ch_Cvt_vt = new Cvt_vt('a', 'b')
val i_Cvt_vt = new Cvt_vt(1, 2)
val Cvt_vt(extracted1i_Cvt_vt, extracted2i_Cvt_vt) = i_Cvt_vt
val Cvt_vt(extracted1ch_Cvt_vt, extracted2ch_Cvt_vt) = ch_Cvt_vt
assert(1 == extracted1i_Cvt_vt)
assert(2 == extracted2i_Cvt_vt)
assert('a' == extracted1ch_Cvt_vt)
assert('b' == extracted2ch_Cvt_vt)

assert(1 == i_Cvt_vt.`a b`)
assert(2 == i_Cvt_vt.a)
assert('a' == ch_Cvt_vt.`a b`)
assert('b' == ch_Cvt_vt.a)
assert(1 == i_Cvt_vt.p1)
assert(2 == i_Cvt_vt.p2)
assert('a' == ch_Cvt_vt.p1)
assert('b' == ch_Cvt_vt.p2)


val ch_Cvt_vf = new Cvt_vf('a', 'b')
val i_Cvt_vf = new Cvt_vf(1, 2)
val Cvt_vf(extracted1i_Cvt_vf, extracted2i_Cvt_vf) = i_Cvt_vf
val Cvt_vf(extracted1ch_Cvt_vf, extracted2ch_Cvt_vf) = ch_Cvt_vf
assert(1 == extracted1i_Cvt_vf)
assert(2 == extracted2i_Cvt_vf)
assert('a' == extracted1ch_Cvt_vf)
assert('b' == extracted2ch_Cvt_vf)

assert(1 == i_Cvt_vf.`a b`)
assert(2 == i_Cvt_vf.a)
assert('a' == ch_Cvt_vf.`a b`)
assert('b' == ch_Cvt_vf.a)
assert(1 == i_Cvt_vf.p1)
assert('a' == ch_Cvt_vf.p1)


val ch_Cvt_nt = new Cvt_nt('a', 'b')
val i_Cvt_nt = new Cvt_nt(1, 2)
val Cvt_nt(extracted1i_Cvt_nt, extracted2i_Cvt_nt) = i_Cvt_nt
val Cvt_nt(extracted1ch_Cvt_nt, extracted2ch_Cvt_nt) = ch_Cvt_nt
assert(1 == extracted1i_Cvt_nt)
assert(2 == extracted2i_Cvt_nt)
assert('a' == extracted1ch_Cvt_nt)
assert('b' == extracted2ch_Cvt_nt)

assert(1 == i_Cvt_nt.`a b`)
assert(2 == i_Cvt_nt.a)
assert('a' == ch_Cvt_nt.`a b`)
assert('b' == ch_Cvt_nt.a)
assert(1 == i_Cvt_nt.p1)
assert(2 == i_Cvt_nt.p2)
assert('a' == ch_Cvt_nt.p1)
assert('b' == ch_Cvt_nt.p2)


val ch_Cvt_nf = new Cvt_nf('a', 'b')
val i_Cvt_nf = new Cvt_nf(1, 2)
val Cvt_nf(extracted1i_Cvt_nf, extracted2i_Cvt_nf) = i_Cvt_nf
val Cvt_nf(extracted1ch_Cvt_nf, extracted2ch_Cvt_nf) = ch_Cvt_nf
assert(1 == extracted1i_Cvt_nf)
assert(2 == extracted2i_Cvt_nf)
assert('a' == extracted1ch_Cvt_nf)
assert('b' == extracted2ch_Cvt_nf)

assert(1 == i_Cvt_nf.`a b`)
assert(2 == i_Cvt_nf.a)
assert('a' == ch_Cvt_nf.`a b`)
assert('b' == ch_Cvt_nf.a)
assert(1 == i_Cvt_nf.p1)
assert('a' == ch_Cvt_nf.p1)


val ch_Cvf_pvt = new Cvf_pvt('a', 'b')
val i_Cvf_pvt = new Cvf_pvt(1, 2)
val Cvf_pvt(extracted1i_Cvf_pvt, extracted2i_Cvf_pvt) = i_Cvf_pvt
val Cvf_pvt(extracted1ch_Cvf_pvt, extracted2ch_Cvf_pvt) = ch_Cvf_pvt
assert(1 == extracted1i_Cvf_pvt)
assert(2 == extracted2i_Cvf_pvt)
assert('a' == extracted1ch_Cvf_pvt)
assert('b' == extracted2ch_Cvf_pvt)

assert(1 == i_Cvf_pvt.`a b`)
assert('a' == ch_Cvf_pvt.`a b`)
assert(2 == i_Cvf_pvt.p2)
assert('b' == ch_Cvf_pvt.p2)


val ch_Cvf_pvf = new Cvf_pvf('a', 'b')
val i_Cvf_pvf = new Cvf_pvf(1, 2)
val Cvf_pvf(extracted1i_Cvf_pvf, extracted2i_Cvf_pvf) = i_Cvf_pvf
val Cvf_pvf(extracted1ch_Cvf_pvf, extracted2ch_Cvf_pvf) = ch_Cvf_pvf
assert(1 == extracted1i_Cvf_pvf)
assert(2 == extracted2i_Cvf_pvf)
assert('a' == extracted1ch_Cvf_pvf)
assert('b' == extracted2ch_Cvf_pvf)

assert(1 == i_Cvf_pvf.`a b`)
assert('a' == ch_Cvf_pvf.`a b`)


val ch_Cvf_vt = new Cvf_vt('a', 'b')
val i_Cvf_vt = new Cvf_vt(1, 2)
val Cvf_vt(extracted1i_Cvf_vt, extracted2i_Cvf_vt) = i_Cvf_vt
val Cvf_vt(extracted1ch_Cvf_vt, extracted2ch_Cvf_vt) = ch_Cvf_vt
assert(1 == extracted1i_Cvf_vt)
assert(2 == extracted2i_Cvf_vt)
assert('a' == extracted1ch_Cvf_vt)
assert('b' == extracted2ch_Cvf_vt)

assert(1 == i_Cvf_vt.`a b`)
assert(2 == i_Cvf_vt.a)
assert('a' == ch_Cvf_vt.`a b`)
assert('b' == ch_Cvf_vt.a)
assert(2 == i_Cvf_vt.p2)
assert('b' == ch_Cvf_vt.p2)


val ch_Cvf_vf = new Cvf_vf('a', 'b')
val i_Cvf_vf = new Cvf_vf(1, 2)
val Cvf_vf(extracted1i_Cvf_vf, extracted2i_Cvf_vf) = i_Cvf_vf
val Cvf_vf(extracted1ch_Cvf_vf, extracted2ch_Cvf_vf) = ch_Cvf_vf
assert(1 == extracted1i_Cvf_vf)
assert(2 == extracted2i_Cvf_vf)
assert('a' == extracted1ch_Cvf_vf)
assert('b' == extracted2ch_Cvf_vf)

assert(1 == i_Cvf_vf.`a b`)
assert(2 == i_Cvf_vf.a)
assert('a' == ch_Cvf_vf.`a b`)
assert('b' == ch_Cvf_vf.a)


val ch_Cvf_nt = new Cvf_nt('a', 'b')
val i_Cvf_nt = new Cvf_nt(1, 2)
val Cvf_nt(extracted1i_Cvf_nt, extracted2i_Cvf_nt) = i_Cvf_nt
val Cvf_nt(extracted1ch_Cvf_nt, extracted2ch_Cvf_nt) = ch_Cvf_nt
assert(1 == extracted1i_Cvf_nt)
assert(2 == extracted2i_Cvf_nt)
assert('a' == extracted1ch_Cvf_nt)
assert('b' == extracted2ch_Cvf_nt)

assert(1 == i_Cvf_nt.`a b`)
assert(2 == i_Cvf_nt.a)
assert('a' == ch_Cvf_nt.`a b`)
assert('b' == ch_Cvf_nt.a)
assert(2 == i_Cvf_nt.p2)
assert('b' == ch_Cvf_nt.p2)


val ch_Cvf_nf = new Cvf_nf('a', 'b')
val i_Cvf_nf = new Cvf_nf(1, 2)
val Cvf_nf(extracted1i_Cvf_nf, extracted2i_Cvf_nf) = i_Cvf_nf
val Cvf_nf(extracted1ch_Cvf_nf, extracted2ch_Cvf_nf) = ch_Cvf_nf
assert(1 == extracted1i_Cvf_nf)
assert(2 == extracted2i_Cvf_nf)
assert('a' == extracted1ch_Cvf_nf)
assert('b' == extracted2ch_Cvf_nf)

assert(1 == i_Cvf_nf.`a b`)
assert(2 == i_Cvf_nf.a)
assert('a' == ch_Cvf_nf.`a b`)
assert('b' == ch_Cvf_nf.a)


val ch_Cnt_pvt = new Cnt_pvt('a', 'b')
val i_Cnt_pvt = new Cnt_pvt(1, 2)
val Cnt_pvt(extracted1i_Cnt_pvt, extracted2i_Cnt_pvt) = i_Cnt_pvt
val Cnt_pvt(extracted1ch_Cnt_pvt, extracted2ch_Cnt_pvt) = ch_Cnt_pvt
assert(1 == extracted1i_Cnt_pvt)
assert(2 == extracted2i_Cnt_pvt)
assert('a' == extracted1ch_Cnt_pvt)
assert('b' == extracted2ch_Cnt_pvt)

assert(1 == i_Cnt_pvt.`a b`)
assert('a' == ch_Cnt_pvt.`a b`)
assert(1 == i_Cnt_pvt.p1)
assert(2 == i_Cnt_pvt.p2)
assert('a' == ch_Cnt_pvt.p1)
assert('b' == ch_Cnt_pvt.p2)


val ch_Cnt_pvf = new Cnt_pvf('a', 'b')
val i_Cnt_pvf = new Cnt_pvf(1, 2)
val Cnt_pvf(extracted1i_Cnt_pvf, extracted2i_Cnt_pvf) = i_Cnt_pvf
val Cnt_pvf(extracted1ch_Cnt_pvf, extracted2ch_Cnt_pvf) = ch_Cnt_pvf
assert(1 == extracted1i_Cnt_pvf)
assert(2 == extracted2i_Cnt_pvf)
assert('a' == extracted1ch_Cnt_pvf)
assert('b' == extracted2ch_Cnt_pvf)

assert(1 == i_Cnt_pvf.`a b`)
assert('a' == ch_Cnt_pvf.`a b`)
assert(1 == i_Cnt_pvf.p1)
assert('a' == ch_Cnt_pvf.p1)


val ch_Cnt_vt = new Cnt_vt('a', 'b')
val i_Cnt_vt = new Cnt_vt(1, 2)
val Cnt_vt(extracted1i_Cnt_vt, extracted2i_Cnt_vt) = i_Cnt_vt
val Cnt_vt(extracted1ch_Cnt_vt, extracted2ch_Cnt_vt) = ch_Cnt_vt
assert(1 == extracted1i_Cnt_vt)
assert(2 == extracted2i_Cnt_vt)
assert('a' == extracted1ch_Cnt_vt)
assert('b' == extracted2ch_Cnt_vt)

assert(1 == i_Cnt_vt.`a b`)
assert(2 == i_Cnt_vt.a)
assert('a' == ch_Cnt_vt.`a b`)
assert('b' == ch_Cnt_vt.a)
assert(1 == i_Cnt_vt.p1)
assert(2 == i_Cnt_vt.p2)
assert('a' == ch_Cnt_vt.p1)
assert('b' == ch_Cnt_vt.p2)


val ch_Cnt_vf = new Cnt_vf('a', 'b')
val i_Cnt_vf = new Cnt_vf(1, 2)
val Cnt_vf(extracted1i_Cnt_vf, extracted2i_Cnt_vf) = i_Cnt_vf
val Cnt_vf(extracted1ch_Cnt_vf, extracted2ch_Cnt_vf) = ch_Cnt_vf
assert(1 == extracted1i_Cnt_vf)
assert(2 == extracted2i_Cnt_vf)
assert('a' == extracted1ch_Cnt_vf)
assert('b' == extracted2ch_Cnt_vf)

assert(1 == i_Cnt_vf.`a b`)
assert(2 == i_Cnt_vf.a)
assert('a' == ch_Cnt_vf.`a b`)
assert('b' == ch_Cnt_vf.a)
assert(1 == i_Cnt_vf.p1)
assert('a' == ch_Cnt_vf.p1)


val ch_Cnt_nt = new Cnt_nt('a', 'b')
val i_Cnt_nt = new Cnt_nt(1, 2)
val Cnt_nt(extracted1i_Cnt_nt, extracted2i_Cnt_nt) = i_Cnt_nt
val Cnt_nt(extracted1ch_Cnt_nt, extracted2ch_Cnt_nt) = ch_Cnt_nt
assert(1 == extracted1i_Cnt_nt)
assert(2 == extracted2i_Cnt_nt)
assert('a' == extracted1ch_Cnt_nt)
assert('b' == extracted2ch_Cnt_nt)

assert(1 == i_Cnt_nt.`a b`)
assert(2 == i_Cnt_nt.a)
assert('a' == ch_Cnt_nt.`a b`)
assert('b' == ch_Cnt_nt.a)
assert(1 == i_Cnt_nt.p1)
assert(2 == i_Cnt_nt.p2)
assert('a' == ch_Cnt_nt.p1)
assert('b' == ch_Cnt_nt.p2)


val ch_Cnt_nf = new Cnt_nf('a', 'b')
val i_Cnt_nf = new Cnt_nf(1, 2)
val Cnt_nf(extracted1i_Cnt_nf, extracted2i_Cnt_nf) = i_Cnt_nf
val Cnt_nf(extracted1ch_Cnt_nf, extracted2ch_Cnt_nf) = ch_Cnt_nf
assert(1 == extracted1i_Cnt_nf)
assert(2 == extracted2i_Cnt_nf)
assert('a' == extracted1ch_Cnt_nf)
assert('b' == extracted2ch_Cnt_nf)

assert(1 == i_Cnt_nf.`a b`)
assert(2 == i_Cnt_nf.a)
assert('a' == ch_Cnt_nf.`a b`)
assert('b' == ch_Cnt_nf.a)
assert(1 == i_Cnt_nf.p1)
assert('a' == ch_Cnt_nf.p1)


val ch_Cnf_pvt = new Cnf_pvt('a', 'b')
val i_Cnf_pvt = new Cnf_pvt(1, 2)
val Cnf_pvt(extracted1i_Cnf_pvt, extracted2i_Cnf_pvt) = i_Cnf_pvt
val Cnf_pvt(extracted1ch_Cnf_pvt, extracted2ch_Cnf_pvt) = ch_Cnf_pvt
assert(1 == extracted1i_Cnf_pvt)
assert(2 == extracted2i_Cnf_pvt)
assert('a' == extracted1ch_Cnf_pvt)
assert('b' == extracted2ch_Cnf_pvt)

assert(1 == i_Cnf_pvt.`a b`)
assert('a' == ch_Cnf_pvt.`a b`)
assert(2 == i_Cnf_pvt.p2)
assert('b' == ch_Cnf_pvt.p2)


val ch_Cnf_pvf = new Cnf_pvf('a', 'b')
val i_Cnf_pvf = new Cnf_pvf(1, 2)
val Cnf_pvf(extracted1i_Cnf_pvf, extracted2i_Cnf_pvf) = i_Cnf_pvf
val Cnf_pvf(extracted1ch_Cnf_pvf, extracted2ch_Cnf_pvf) = ch_Cnf_pvf
assert(1 == extracted1i_Cnf_pvf)
assert(2 == extracted2i_Cnf_pvf)
assert('a' == extracted1ch_Cnf_pvf)
assert('b' == extracted2ch_Cnf_pvf)

assert(1 == i_Cnf_pvf.`a b`)
assert('a' == ch_Cnf_pvf.`a b`)


val ch_Cnf_vt = new Cnf_vt('a', 'b')
val i_Cnf_vt = new Cnf_vt(1, 2)
val Cnf_vt(extracted1i_Cnf_vt, extracted2i_Cnf_vt) = i_Cnf_vt
val Cnf_vt(extracted1ch_Cnf_vt, extracted2ch_Cnf_vt) = ch_Cnf_vt
assert(1 == extracted1i_Cnf_vt)
assert(2 == extracted2i_Cnf_vt)
assert('a' == extracted1ch_Cnf_vt)
assert('b' == extracted2ch_Cnf_vt)

assert(1 == i_Cnf_vt.`a b`)
assert(2 == i_Cnf_vt.a)
assert('a' == ch_Cnf_vt.`a b`)
assert('b' == ch_Cnf_vt.a)
assert(2 == i_Cnf_vt.p2)
assert('b' == ch_Cnf_vt.p2)


val ch_Cnf_vf = new Cnf_vf('a', 'b')
val i_Cnf_vf = new Cnf_vf(1, 2)
val Cnf_vf(extracted1i_Cnf_vf, extracted2i_Cnf_vf) = i_Cnf_vf
val Cnf_vf(extracted1ch_Cnf_vf, extracted2ch_Cnf_vf) = ch_Cnf_vf
assert(1 == extracted1i_Cnf_vf)
assert(2 == extracted2i_Cnf_vf)
assert('a' == extracted1ch_Cnf_vf)
assert('b' == extracted2ch_Cnf_vf)

assert(1 == i_Cnf_vf.`a b`)
assert(2 == i_Cnf_vf.a)
assert('a' == ch_Cnf_vf.`a b`)
assert('b' == ch_Cnf_vf.a)


val ch_Cnf_nt = new Cnf_nt('a', 'b')
val i_Cnf_nt = new Cnf_nt(1, 2)
val Cnf_nt(extracted1i_Cnf_nt, extracted2i_Cnf_nt) = i_Cnf_nt
val Cnf_nt(extracted1ch_Cnf_nt, extracted2ch_Cnf_nt) = ch_Cnf_nt
assert(1 == extracted1i_Cnf_nt)
assert(2 == extracted2i_Cnf_nt)
assert('a' == extracted1ch_Cnf_nt)
assert('b' == extracted2ch_Cnf_nt)

assert(1 == i_Cnf_nt.`a b`)
assert(2 == i_Cnf_nt.a)
assert('a' == ch_Cnf_nt.`a b`)
assert('b' == ch_Cnf_nt.a)
assert(2 == i_Cnf_nt.p2)
assert('b' == ch_Cnf_nt.p2)


val ch_Cnf_nf = new Cnf_nf('a', 'b')
val i_Cnf_nf = new Cnf_nf(1, 2)
val Cnf_nf(extracted1i_Cnf_nf, extracted2i_Cnf_nf) = i_Cnf_nf
val Cnf_nf(extracted1ch_Cnf_nf, extracted2ch_Cnf_nf) = ch_Cnf_nf
assert(1 == extracted1i_Cnf_nf)
assert(2 == extracted2i_Cnf_nf)
assert('a' == extracted1ch_Cnf_nf)
assert('b' == extracted2ch_Cnf_nf)

assert(1 == i_Cnf_nf.`a b`)
assert(2 == i_Cnf_nf.a)
assert('a' == ch_Cnf_nf.`a b`)
assert('b' == ch_Cnf_nf.a)


val ch_Rpvt_pvt = new Rpvt_pvt('a', 'b')
val i_Rpvt_pvt = new Rpvt_pvt(1, 2)
assert(1 == i_Rpvt_pvt.p1)
assert(2 == i_Rpvt_pvt.p2)
assert('a' == ch_Rpvt_pvt.p1)
assert('b' == ch_Rpvt_pvt.p2)


val ch_Rpvt_pvf = new Rpvt_pvf('a', 'b')
val i_Rpvt_pvf = new Rpvt_pvf(1, 2)
assert(1 == i_Rpvt_pvf.p1)
assert('a' == ch_Rpvt_pvf.p1)


val ch_Rpvt_vt = new Rpvt_vt('a', 'b')
val i_Rpvt_vt = new Rpvt_vt(1, 2)
assert(2 == i_Rpvt_vt.a)
assert('b' == ch_Rpvt_vt.a)
assert(1 == i_Rpvt_vt.p1)
assert(2 == i_Rpvt_vt.p2)
assert('a' == ch_Rpvt_vt.p1)
assert('b' == ch_Rpvt_vt.p2)


val ch_Rpvt_vf = new Rpvt_vf('a', 'b')
val i_Rpvt_vf = new Rpvt_vf(1, 2)
assert(2 == i_Rpvt_vf.a)
assert('b' == ch_Rpvt_vf.a)
assert(1 == i_Rpvt_vf.p1)
assert('a' == ch_Rpvt_vf.p1)


val ch_Rpvt_nt = new Rpvt_nt('a', 'b')
val i_Rpvt_nt = new Rpvt_nt(1, 2)
assert(1 == i_Rpvt_nt.p1)
assert(2 == i_Rpvt_nt.p2)
assert('a' == ch_Rpvt_nt.p1)
assert('b' == ch_Rpvt_nt.p2)


val ch_Rpvt_nf = new Rpvt_nf('a', 'b')
val i_Rpvt_nf = new Rpvt_nf(1, 2)
assert(1 == i_Rpvt_nf.p1)
assert('a' == ch_Rpvt_nf.p1)


val ch_Rpvf_pvt = new Rpvf_pvt('a', 'b')
val i_Rpvf_pvt = new Rpvf_pvt(1, 2)
assert(2 == i_Rpvf_pvt.p2)
assert('b' == ch_Rpvf_pvt.p2)


val ch_Rpvf_pvf = new Rpvf_pvf('a', 'b')
val i_Rpvf_pvf = new Rpvf_pvf(1, 2)


val ch_Rpvf_vt = new Rpvf_vt('a', 'b')
val i_Rpvf_vt = new Rpvf_vt(1, 2)
assert(2 == i_Rpvf_vt.a)
assert('b' == ch_Rpvf_vt.a)
assert(2 == i_Rpvf_vt.p2)
assert('b' == ch_Rpvf_vt.p2)


val ch_Rpvf_vf = new Rpvf_vf('a', 'b')
val i_Rpvf_vf = new Rpvf_vf(1, 2)
assert(2 == i_Rpvf_vf.a)
assert('b' == ch_Rpvf_vf.a)


val ch_Rpvf_nt = new Rpvf_nt('a', 'b')
val i_Rpvf_nt = new Rpvf_nt(1, 2)
assert(2 == i_Rpvf_nt.p2)
assert('b' == ch_Rpvf_nt.p2)


val ch_Rpvf_nf = new Rpvf_nf('a', 'b')
val i_Rpvf_nf = new Rpvf_nf(1, 2)


val ch_Rvt_pvt = new Rvt_pvt('a', 'b')
val i_Rvt_pvt = new Rvt_pvt(1, 2)
assert(1 == i_Rvt_pvt.`a b`)
assert('a' == ch_Rvt_pvt.`a b`)
assert(1 == i_Rvt_pvt.p1)
assert(2 == i_Rvt_pvt.p2)
assert('a' == ch_Rvt_pvt.p1)
assert('b' == ch_Rvt_pvt.p2)


val ch_Rvt_pvf = new Rvt_pvf('a', 'b')
val i_Rvt_pvf = new Rvt_pvf(1, 2)
assert(1 == i_Rvt_pvf.`a b`)
assert('a' == ch_Rvt_pvf.`a b`)
assert(1 == i_Rvt_pvf.p1)
assert('a' == ch_Rvt_pvf.p1)


val ch_Rvt_vt = new Rvt_vt('a', 'b')
val i_Rvt_vt = new Rvt_vt(1, 2)
assert(1 == i_Rvt_vt.`a b`)
assert(2 == i_Rvt_vt.a)
assert('a' == ch_Rvt_vt.`a b`)
assert('b' == ch_Rvt_vt.a)
assert(1 == i_Rvt_vt.p1)
assert(2 == i_Rvt_vt.p2)
assert('a' == ch_Rvt_vt.p1)
assert('b' == ch_Rvt_vt.p2)


val ch_Rvt_vf = new Rvt_vf('a', 'b')
val i_Rvt_vf = new Rvt_vf(1, 2)
assert(1 == i_Rvt_vf.`a b`)
assert(2 == i_Rvt_vf.a)
assert('a' == ch_Rvt_vf.`a b`)
assert('b' == ch_Rvt_vf.a)
assert(1 == i_Rvt_vf.p1)
assert('a' == ch_Rvt_vf.p1)


val ch_Rvt_nt = new Rvt_nt('a', 'b')
val i_Rvt_nt = new Rvt_nt(1, 2)
assert(1 == i_Rvt_nt.`a b`)
assert('a' == ch_Rvt_nt.`a b`)
assert(1 == i_Rvt_nt.p1)
assert(2 == i_Rvt_nt.p2)
assert('a' == ch_Rvt_nt.p1)
assert('b' == ch_Rvt_nt.p2)


val ch_Rvt_nf = new Rvt_nf('a', 'b')
val i_Rvt_nf = new Rvt_nf(1, 2)
assert(1 == i_Rvt_nf.`a b`)
assert('a' == ch_Rvt_nf.`a b`)
assert(1 == i_Rvt_nf.p1)
assert('a' == ch_Rvt_nf.p1)


val ch_Rvf_pvt = new Rvf_pvt('a', 'b')
val i_Rvf_pvt = new Rvf_pvt(1, 2)
assert(1 == i_Rvf_pvt.`a b`)
assert('a' == ch_Rvf_pvt.`a b`)
assert(2 == i_Rvf_pvt.p2)
assert('b' == ch_Rvf_pvt.p2)


val ch_Rvf_pvf = new Rvf_pvf('a', 'b')
val i_Rvf_pvf = new Rvf_pvf(1, 2)
assert(1 == i_Rvf_pvf.`a b`)
assert('a' == ch_Rvf_pvf.`a b`)


val ch_Rvf_vt = new Rvf_vt('a', 'b')
val i_Rvf_vt = new Rvf_vt(1, 2)
assert(1 == i_Rvf_vt.`a b`)
assert(2 == i_Rvf_vt.a)
assert('a' == ch_Rvf_vt.`a b`)
assert('b' == ch_Rvf_vt.a)
assert(2 == i_Rvf_vt.p2)
assert('b' == ch_Rvf_vt.p2)


val ch_Rvf_vf = new Rvf_vf('a', 'b')
val i_Rvf_vf = new Rvf_vf(1, 2)
assert(1 == i_Rvf_vf.`a b`)
assert(2 == i_Rvf_vf.a)
assert('a' == ch_Rvf_vf.`a b`)
assert('b' == ch_Rvf_vf.a)


val ch_Rvf_nt = new Rvf_nt('a', 'b')
val i_Rvf_nt = new Rvf_nt(1, 2)
assert(1 == i_Rvf_nt.`a b`)
assert('a' == ch_Rvf_nt.`a b`)
assert(2 == i_Rvf_nt.p2)
assert('b' == ch_Rvf_nt.p2)


val ch_Rvf_nf = new Rvf_nf('a', 'b')
val i_Rvf_nf = new Rvf_nf(1, 2)
assert(1 == i_Rvf_nf.`a b`)
assert('a' == ch_Rvf_nf.`a b`)


val ch_Rnt_pvt = new Rnt_pvt('a', 'b')
val i_Rnt_pvt = new Rnt_pvt(1, 2)
assert(1 == i_Rnt_pvt.p1)
assert(2 == i_Rnt_pvt.p2)
assert('a' == ch_Rnt_pvt.p1)
assert('b' == ch_Rnt_pvt.p2)


val ch_Rnt_pvf = new Rnt_pvf('a', 'b')
val i_Rnt_pvf = new Rnt_pvf(1, 2)
assert(1 == i_Rnt_pvf.p1)
assert('a' == ch_Rnt_pvf.p1)


val ch_Rnt_vt = new Rnt_vt('a', 'b')
val i_Rnt_vt = new Rnt_vt(1, 2)
assert(2 == i_Rnt_vt.a)
assert('b' == ch_Rnt_vt.a)
assert(1 == i_Rnt_vt.p1)
assert(2 == i_Rnt_vt.p2)
assert('a' == ch_Rnt_vt.p1)
assert('b' == ch_Rnt_vt.p2)


val ch_Rnt_vf = new Rnt_vf('a', 'b')
val i_Rnt_vf = new Rnt_vf(1, 2)
assert(2 == i_Rnt_vf.a)
assert('b' == ch_Rnt_vf.a)
assert(1 == i_Rnt_vf.p1)
assert('a' == ch_Rnt_vf.p1)


val ch_Rnt_nt = new Rnt_nt('a', 'b')
val i_Rnt_nt = new Rnt_nt(1, 2)
assert(1 == i_Rnt_nt.p1)
assert(2 == i_Rnt_nt.p2)
assert('a' == ch_Rnt_nt.p1)
assert('b' == ch_Rnt_nt.p2)


val ch_Rnt_nf = new Rnt_nf('a', 'b')
val i_Rnt_nf = new Rnt_nf(1, 2)
assert(1 == i_Rnt_nf.p1)
assert('a' == ch_Rnt_nf.p1)


val ch_Rnf_pvt = new Rnf_pvt('a', 'b')
val i_Rnf_pvt = new Rnf_pvt(1, 2)
assert(2 == i_Rnf_pvt.p2)
assert('b' == ch_Rnf_pvt.p2)


val ch_Rnf_pvf = new Rnf_pvf('a', 'b')
val i_Rnf_pvf = new Rnf_pvf(1, 2)


val ch_Rnf_vt = new Rnf_vt('a', 'b')
val i_Rnf_vt = new Rnf_vt(1, 2)
assert(2 == i_Rnf_vt.a)
assert('b' == ch_Rnf_vt.a)
assert(2 == i_Rnf_vt.p2)
assert('b' == ch_Rnf_vt.p2)


val ch_Rnf_vf = new Rnf_vf('a', 'b')
val i_Rnf_vf = new Rnf_vf(1, 2)
assert(2 == i_Rnf_vf.a)
assert('b' == ch_Rnf_vf.a)


val ch_Rnf_nt = new Rnf_nt('a', 'b')
val i_Rnf_nt = new Rnf_nt(1, 2)
assert(2 == i_Rnf_nt.p2)
assert('b' == ch_Rnf_nt.p2)


val ch_Rnf_nf = new Rnf_nf('a', 'b')
val i_Rnf_nf = new Rnf_nf(1, 2)

TestJoint.joint()
}
