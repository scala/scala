object Program {
 trait A { type T }
 type B = A { type T = String }
 trait C extends B
}
