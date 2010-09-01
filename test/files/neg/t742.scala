object Crash {
    type mul[m[n[s[_], z], z], n[s[_], z], z] = m[n, z]
    type _1[s1[_], z1] = s1[z1]
    type _2[s1[_], z1] = s1[z1]
    type p = mul[_1, _2, Any]  // mul[_1, _1, Any] needs -Yrecursion
    // _1[_2, Zero]
    // _2[Zero]
}