package scala.tools.nsc.transform.patmat

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.CompilerControl.Mode.DONT_INLINE
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.annotation.switch
import scala.util.Random

@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ClassMatchBenchmark {
  private final val count = 10000
  @Param(Array("4", "8", "16", "32", "64", "128", "256")) private var numCases = 0

  private var names: Array[Name]          = null
  private var classValue: ClassValue[Int] = null

  @Setup def setup(): Unit = {
    val r = new Random(12345)
    val names = Array[Name](
      Name0(), Name1(), Name2(), Name3(), Name4(), Name5(), Name6(), Name7(), Name8(), Name9(),
      Name10(), Name11(), Name12(), Name13(), Name14(), Name15(), Name16(), Name17(), Name18(), Name19(),
      Name20(), Name21(), Name22(), Name23(), Name24(), Name25(), Name26(), Name27(), Name28(), Name29(),
      Name30(), Name31(), Name32(), Name33(), Name34(), Name35(), Name36(), Name37(), Name38(), Name39(),
      Name40(), Name41(), Name42(), Name43(), Name44(), Name45(), Name46(), Name47(), Name48(), Name49(),
      Name50(), Name51(), Name52(), Name53(), Name54(), Name55(), Name56(), Name57(), Name58(), Name59(),
      Name60(), Name61(), Name62(), Name63(), Name64(), Name65(), Name66(), Name67(), Name68(), Name69(),
      Name70(), Name71(), Name72(), Name73(), Name74(), Name75(), Name76(), Name77(), Name78(), Name79(),
      Name80(), Name81(), Name82(), Name83(), Name84(), Name85(), Name86(), Name87(), Name88(), Name89(),
      Name90(), Name91(), Name92(), Name93(), Name94(), Name95(), Name96(), Name97(), Name98(), Name99(),
      Name100(), Name101(), Name102(), Name103(), Name104(), Name105(), Name106(), Name107(), Name108(), Name109(),
      Name110(), Name111(), Name112(), Name113(), Name114(), Name115(), Name116(), Name117(), Name118(), Name119(),
      Name120(), Name121(), Name122(), Name123(), Name124(), Name125(), Name126(), Name127(), Name128(), Name129(),
      Name130(), Name131(), Name132(), Name133(), Name134(), Name135(), Name136(), Name137(), Name138(), Name139(),
      Name140(), Name141(), Name142(), Name143(), Name144(), Name145(), Name146(), Name147(), Name148(), Name149(),
      Name150(), Name151(), Name152(), Name153(), Name154(), Name155(), Name156(), Name157(), Name158(), Name159(),
      Name160(), Name161(), Name162(), Name163(), Name164(), Name165(), Name166(), Name167(), Name168(), Name169(),
      Name170(), Name171(), Name172(), Name173(), Name174(), Name175(), Name176(), Name177(), Name178(), Name179(),
      Name180(), Name181(), Name182(), Name183(), Name184(), Name185(), Name186(), Name187(), Name188(), Name189(),
      Name190(), Name191(), Name192(), Name193(), Name194(), Name195(), Name196(), Name197(), Name198(), Name199(),
      Name200(), Name201(), Name202(), Name203(), Name204(), Name205(), Name206(), Name207(), Name208(), Name209(),
      Name210(), Name211(), Name212(), Name213(), Name214(), Name215(), Name216(), Name217(), Name218(), Name219(),
      Name220(), Name221(), Name222(), Name223(), Name224(), Name225(), Name226(), Name227(), Name228(), Name229(),
      Name230(), Name231(), Name232(), Name233(), Name234(), Name235(), Name236(), Name237(), Name238(), Name239(),
      Name240(), Name241(), Name242(), Name243(), Name244(), Name245(), Name246(), Name247(), Name248(), Name249(),
      Name250(), Name251(), Name252(), Name253(), Name254(), Name255(),
    )
    this.names = Array.fill(count)(names(r.nextInt(numCases)))
    this.classValue = new NameClassValue
  }

  @Benchmark @OperationsPerInvocation(count) def patmatShow(bh: Blackhole): Unit = {
    val names = this.names
    var i = 0
    while (i < names.length) {
      val x = names(i) match {
        case Name0() => "0" case Name1() => "1" case Name2() => "2" case Name3() => "3" case Name4() => "4"
        case Name5() => "5" case Name6() => "6" case Name7() => "7" case Name8() => "8" case Name9() => "9"
        case Name10() => "10" case Name11() => "11" case Name12() => "12" case Name13() => "13" case Name14() => "14"
        case Name15() => "15" case Name16() => "16" case Name17() => "17" case Name18() => "18" case Name19() => "19"
        case Name20() => "20" case Name21() => "21" case Name22() => "22" case Name23() => "23" case Name24() => "24"
        case Name25() => "25" case Name26() => "26" case Name27() => "27" case Name28() => "28" case Name29() => "29"
        case Name30() => "30" case Name31() => "31" case Name32() => "32" case Name33() => "33" case Name34() => "34"
        case Name35() => "35" case Name36() => "36" case Name37() => "37" case Name38() => "38" case Name39() => "39"
        case Name40() => "40" case Name41() => "41" case Name42() => "42" case Name43() => "43" case Name44() => "44"
        case Name45() => "45" case Name46() => "46" case Name47() => "47" case Name48() => "48" case Name49() => "49"
        case Name50() => "50" case Name51() => "51" case Name52() => "52" case Name53() => "53" case Name54() => "54"
        case Name55() => "55" case Name56() => "56" case Name57() => "57" case Name58() => "58" case Name59() => "59"
        case Name60() => "60" case Name61() => "61" case Name62() => "62" case Name63() => "63" case Name64() => "64"
        case Name65() => "65" case Name66() => "66" case Name67() => "67" case Name68() => "68" case Name69() => "69"
        case Name70() => "70" case Name71() => "71" case Name72() => "72" case Name73() => "73" case Name74() => "74"
        case Name75() => "75" case Name76() => "76" case Name77() => "77" case Name78() => "78" case Name79() => "79"
        case Name80() => "80" case Name81() => "81" case Name82() => "82" case Name83() => "83" case Name84() => "84"
        case Name85() => "85" case Name86() => "86" case Name87() => "87" case Name88() => "88" case Name89() => "89"
        case Name90() => "90" case Name91() => "91" case Name92() => "92" case Name93() => "93" case Name94() => "94"
        case Name95() => "95" case Name96() => "96" case Name97() => "97" case Name98() => "98" case Name99() => "99"
        case Name100() => "100" case Name101() => "101" case Name102() => "102" case Name103() => "103" case Name104() => "104"
        case Name105() => "105" case Name106() => "106" case Name107() => "107" case Name108() => "108" case Name109() => "109"
        case Name110() => "110" case Name111() => "111" case Name112() => "112" case Name113() => "113" case Name114() => "114"
        case Name115() => "115" case Name116() => "116" case Name117() => "117" case Name118() => "118" case Name119() => "119"
        case Name120() => "120" case Name121() => "121" case Name122() => "122" case Name123() => "123" case Name124() => "124"
        case Name125() => "125" case Name126() => "126" case Name127() => "127" case Name128() => "128" case Name129() => "129"
        case Name130() => "130" case Name131() => "131" case Name132() => "132" case Name133() => "133" case Name134() => "134"
        case Name135() => "135" case Name136() => "136" case Name137() => "137" case Name138() => "138" case Name139() => "139"
        case Name140() => "140" case Name141() => "141" case Name142() => "142" case Name143() => "143" case Name144() => "144"
        case Name145() => "145" case Name146() => "146" case Name147() => "147" case Name148() => "148" case Name149() => "149"
        case Name150() => "150" case Name151() => "151" case Name152() => "152" case Name153() => "153" case Name154() => "154"
        case Name155() => "155" case Name156() => "156" case Name157() => "157" case Name158() => "158" case Name159() => "159"
        case Name160() => "160" case Name161() => "161" case Name162() => "162" case Name163() => "163" case Name164() => "164"
        case Name165() => "165" case Name166() => "166" case Name167() => "167" case Name168() => "168" case Name169() => "169"
        case Name170() => "170" case Name171() => "171" case Name172() => "172" case Name173() => "173" case Name174() => "174"
        case Name175() => "175" case Name176() => "176" case Name177() => "177" case Name178() => "178" case Name179() => "179"
        case Name180() => "180" case Name181() => "181" case Name182() => "182" case Name183() => "183" case Name184() => "184"
        case Name185() => "185" case Name186() => "186" case Name187() => "187" case Name188() => "188" case Name189() => "189"
        case Name190() => "190" case Name191() => "191" case Name192() => "192" case Name193() => "193" case Name194() => "194"
        case Name195() => "195" case Name196() => "196" case Name197() => "197" case Name198() => "198" case Name199() => "199"
        case Name200() => "200" case Name201() => "201" case Name202() => "202" case Name203() => "203" case Name204() => "204"
        case Name205() => "205" case Name206() => "206" case Name207() => "207" case Name208() => "208" case Name209() => "209"
        case Name210() => "210" case Name211() => "211" case Name212() => "212" case Name213() => "213" case Name214() => "214"
        case Name215() => "215" case Name216() => "216" case Name217() => "217" case Name218() => "218" case Name219() => "219"
        case Name220() => "220" case Name221() => "221" case Name222() => "222" case Name223() => "223" case Name224() => "224"
        case Name225() => "225" case Name226() => "226" case Name227() => "227" case Name228() => "228" case Name229() => "229"
        case Name230() => "230" case Name231() => "231" case Name232() => "232" case Name233() => "233" case Name234() => "234"
        case Name235() => "235" case Name236() => "236" case Name237() => "237" case Name238() => "238" case Name239() => "239"
        case Name240() => "240" case Name241() => "241" case Name242() => "242" case Name243() => "243" case Name244() => "244"
        case Name245() => "245" case Name246() => "246" case Name247() => "247" case Name248() => "248" case Name249() => "249"
        case Name250() => "250" case Name251() => "251" case Name252() => "252" case Name253() => "253" case Name254() => "254"
        case Name255() => "255"
      }
      bh.consume(x)
      i += 1
    }
  }

  @Benchmark @OperationsPerInvocation(count) def virtualShow(bh: Blackhole): Unit = {
    val names = this.names
    var i = 0
    while (i < names.length) {
      bh.consume(names(i).virtualShow)
      i += 1
    }
  }

  @Benchmark @OperationsPerInvocation(count) def intSwitchShow(bh: Blackhole): Unit = {
    val names = this.names
    var i = 0
    while (i < names.length) {
      val x = (names(i)._id: @switch) match {
        case 0 => "0" case 1 => "1" case 2 => "2" case 3 => "3" case 4 => "4"
        case 5 => "5" case 6 => "6" case 7 => "7" case 8 => "8" case 9 => "9"
        case 10 => "10" case 11 => "11" case 12 => "12" case 13 => "13" case 14 => "14"
        case 15 => "15" case 16 => "16" case 17 => "17" case 18 => "18" case 19 => "19"
        case 20 => "20" case 21 => "21" case 22 => "22" case 23 => "23" case 24 => "24"
        case 25 => "25" case 26 => "26" case 27 => "27" case 28 => "28" case 29 => "29"
        case 30 => "30" case 31 => "31" case 32 => "32" case 33 => "33" case 34 => "34"
        case 35 => "35" case 36 => "36" case 37 => "37" case 38 => "38" case 39 => "39"
        case 40 => "40" case 41 => "41" case 42 => "42" case 43 => "43" case 44 => "44"
        case 45 => "45" case 46 => "46" case 47 => "47" case 48 => "48" case 49 => "49"
        case 50 => "50" case 51 => "51" case 52 => "52" case 53 => "53" case 54 => "54"
        case 55 => "55" case 56 => "56" case 57 => "57" case 58 => "58" case 59 => "59"
        case 60 => "60" case 61 => "61" case 62 => "62" case 63 => "63" case 64 => "64"
        case 65 => "65" case 66 => "66" case 67 => "67" case 68 => "68" case 69 => "69"
        case 70 => "70" case 71 => "71" case 72 => "72" case 73 => "73" case 74 => "74"
        case 75 => "75" case 76 => "76" case 77 => "77" case 78 => "78" case 79 => "79"
        case 80 => "80" case 81 => "81" case 82 => "82" case 83 => "83" case 84 => "84"
        case 85 => "85" case 86 => "86" case 87 => "87" case 88 => "88" case 89 => "89"
        case 90 => "90" case 91 => "91" case 92 => "92" case 93 => "93" case 94 => "94"
        case 95 => "95" case 96 => "96" case 97 => "97" case 98 => "98" case 99 => "99"
        case 100 => "100" case 101 => "101" case 102 => "102" case 103 => "103" case 104 => "104"
        case 105 => "105" case 106 => "106" case 107 => "107" case 108 => "108" case 109 => "109"
        case 110 => "110" case 111 => "111" case 112 => "112" case 113 => "113" case 114 => "114"
        case 115 => "115" case 116 => "116" case 117 => "117" case 118 => "118" case 119 => "119"
        case 120 => "120" case 121 => "121" case 122 => "122" case 123 => "123" case 124 => "124"
        case 125 => "125" case 126 => "126" case 127 => "127" case 128 => "128" case 129 => "129"
        case 130 => "130" case 131 => "131" case 132 => "132" case 133 => "133" case 134 => "134"
        case 135 => "135" case 136 => "136" case 137 => "137" case 138 => "138" case 139 => "139"
        case 140 => "140" case 141 => "141" case 142 => "142" case 143 => "143" case 144 => "144"
        case 145 => "145" case 146 => "146" case 147 => "147" case 148 => "148" case 149 => "149"
        case 150 => "150" case 151 => "151" case 152 => "152" case 153 => "153" case 154 => "154"
        case 155 => "155" case 156 => "156" case 157 => "157" case 158 => "158" case 159 => "159"
        case 160 => "160" case 161 => "161" case 162 => "162" case 163 => "163" case 164 => "164"
        case 165 => "165" case 166 => "166" case 167 => "167" case 168 => "168" case 169 => "169"
        case 170 => "170" case 171 => "171" case 172 => "172" case 173 => "173" case 174 => "174"
        case 175 => "175" case 176 => "176" case 177 => "177" case 178 => "178" case 179 => "179"
        case 180 => "180" case 181 => "181" case 182 => "182" case 183 => "183" case 184 => "184"
        case 185 => "185" case 186 => "186" case 187 => "187" case 188 => "188" case 189 => "189"
        case 190 => "190" case 191 => "191" case 192 => "192" case 193 => "193" case 194 => "194"
        case 195 => "195" case 196 => "196" case 197 => "197" case 198 => "198" case 199 => "199"
        case 200 => "200" case 201 => "201" case 202 => "202" case 203 => "203" case 204 => "204"
        case 205 => "205" case 206 => "206" case 207 => "207" case 208 => "208" case 209 => "209"
        case 210 => "210" case 211 => "211" case 212 => "212" case 213 => "213" case 214 => "214"
        case 215 => "215" case 216 => "216" case 217 => "217" case 218 => "218" case 219 => "219"
        case 220 => "220" case 221 => "221" case 222 => "222" case 223 => "223" case 224 => "224"
        case 225 => "225" case 226 => "226" case 227 => "227" case 228 => "228" case 229 => "229"
        case 230 => "230" case 231 => "231" case 232 => "232" case 233 => "233" case 234 => "234"
        case 235 => "235" case 236 => "236" case 237 => "237" case 238 => "238" case 239 => "239"
        case 240 => "240" case 241 => "241" case 242 => "242" case 243 => "243" case 244 => "244"
        case 245 => "245" case 246 => "246" case 247 => "247" case 248 => "248" case 249 => "249"
        case 250 => "250" case 251 => "251" case 252 => "252" case 253 => "253" case 254 => "254"
        case 255 => "255"
      }
      bh.consume(x)
      i += 1
    }
  }

  @Benchmark @OperationsPerInvocation(count) def justClassValueLookup(bh: Blackhole): Unit = {
    val names      = this.names
    val classValue = this.classValue
    var i = 0
    while (i < names.length) {
      bh.consume(classValue.get(names(i).getClass))
      i += 1
    }
  }

  @Benchmark @OperationsPerInvocation(count) def classValueShow(bh: Blackhole): Unit = {
    val names      = this.names
    val classValue = this.classValue
    var i = 0
    while (i < names.length) {
      val x = (classValue.get(names(i).getClass): @switch) match {
        case 0 => "0" case 1 => "1" case 2 => "2" case 3 => "3" case 4 => "4"
        case 5 => "5" case 6 => "6" case 7 => "7" case 8 => "8" case 9 => "9"
        case 10 => "10" case 11 => "11" case 12 => "12" case 13 => "13" case 14 => "14"
        case 15 => "15" case 16 => "16" case 17 => "17" case 18 => "18" case 19 => "19"
        case 20 => "20" case 21 => "21" case 22 => "22" case 23 => "23" case 24 => "24"
        case 25 => "25" case 26 => "26" case 27 => "27" case 28 => "28" case 29 => "29"
        case 30 => "30" case 31 => "31" case 32 => "32" case 33 => "33" case 34 => "34"
        case 35 => "35" case 36 => "36" case 37 => "37" case 38 => "38" case 39 => "39"
        case 40 => "40" case 41 => "41" case 42 => "42" case 43 => "43" case 44 => "44"
        case 45 => "45" case 46 => "46" case 47 => "47" case 48 => "48" case 49 => "49"
        case 50 => "50" case 51 => "51" case 52 => "52" case 53 => "53" case 54 => "54"
        case 55 => "55" case 56 => "56" case 57 => "57" case 58 => "58" case 59 => "59"
        case 60 => "60" case 61 => "61" case 62 => "62" case 63 => "63" case 64 => "64"
        case 65 => "65" case 66 => "66" case 67 => "67" case 68 => "68" case 69 => "69"
        case 70 => "70" case 71 => "71" case 72 => "72" case 73 => "73" case 74 => "74"
        case 75 => "75" case 76 => "76" case 77 => "77" case 78 => "78" case 79 => "79"
        case 80 => "80" case 81 => "81" case 82 => "82" case 83 => "83" case 84 => "84"
        case 85 => "85" case 86 => "86" case 87 => "87" case 88 => "88" case 89 => "89"
        case 90 => "90" case 91 => "91" case 92 => "92" case 93 => "93" case 94 => "94"
        case 95 => "95" case 96 => "96" case 97 => "97" case 98 => "98" case 99 => "99"
        case 100 => "100" case 101 => "101" case 102 => "102" case 103 => "103" case 104 => "104"
        case 105 => "105" case 106 => "106" case 107 => "107" case 108 => "108" case 109 => "109"
        case 110 => "110" case 111 => "111" case 112 => "112" case 113 => "113" case 114 => "114"
        case 115 => "115" case 116 => "116" case 117 => "117" case 118 => "118" case 119 => "119"
        case 120 => "120" case 121 => "121" case 122 => "122" case 123 => "123" case 124 => "124"
        case 125 => "125" case 126 => "126" case 127 => "127" case 128 => "128" case 129 => "129"
        case 130 => "130" case 131 => "131" case 132 => "132" case 133 => "133" case 134 => "134"
        case 135 => "135" case 136 => "136" case 137 => "137" case 138 => "138" case 139 => "139"
        case 140 => "140" case 141 => "141" case 142 => "142" case 143 => "143" case 144 => "144"
        case 145 => "145" case 146 => "146" case 147 => "147" case 148 => "148" case 149 => "149"
        case 150 => "150" case 151 => "151" case 152 => "152" case 153 => "153" case 154 => "154"
        case 155 => "155" case 156 => "156" case 157 => "157" case 158 => "158" case 159 => "159"
        case 160 => "160" case 161 => "161" case 162 => "162" case 163 => "163" case 164 => "164"
        case 165 => "165" case 166 => "166" case 167 => "167" case 168 => "168" case 169 => "169"
        case 170 => "170" case 171 => "171" case 172 => "172" case 173 => "173" case 174 => "174"
        case 175 => "175" case 176 => "176" case 177 => "177" case 178 => "178" case 179 => "179"
        case 180 => "180" case 181 => "181" case 182 => "182" case 183 => "183" case 184 => "184"
        case 185 => "185" case 186 => "186" case 187 => "187" case 188 => "188" case 189 => "189"
        case 190 => "190" case 191 => "191" case 192 => "192" case 193 => "193" case 194 => "194"
        case 195 => "195" case 196 => "196" case 197 => "197" case 198 => "198" case 199 => "199"
        case 200 => "200" case 201 => "201" case 202 => "202" case 203 => "203" case 204 => "204"
        case 205 => "205" case 206 => "206" case 207 => "207" case 208 => "208" case 209 => "209"
        case 210 => "210" case 211 => "211" case 212 => "212" case 213 => "213" case 214 => "214"
        case 215 => "215" case 216 => "216" case 217 => "217" case 218 => "218" case 219 => "219"
        case 220 => "220" case 221 => "221" case 222 => "222" case 223 => "223" case 224 => "224"
        case 225 => "225" case 226 => "226" case 227 => "227" case 228 => "228" case 229 => "229"
        case 230 => "230" case 231 => "231" case 232 => "232" case 233 => "233" case 234 => "234"
        case 235 => "235" case 236 => "236" case 237 => "237" case 238 => "238" case 239 => "239"
        case 240 => "240" case 241 => "241" case 242 => "242" case 243 => "243" case 244 => "244"
        case 245 => "245" case 246 => "246" case 247 => "247" case 248 => "248" case 249 => "249"
        case 250 => "250" case 251 => "251" case 252 => "252" case 253 => "253" case 254 => "254"
        case 255 => "255"
      }
      bh.consume(x)
      i += 1
    }
  }

  @Benchmark @OperationsPerInvocation(count) def classNameHashSwitchShow(bh: Blackhole): Unit = {
    val names = this.names
    var i = 0
    while (i < names.length) {
      val name = names(i)
      val cls  = name.getClass
      val x = ((cls.getName.##): @switch) match {
        case -1200720095 => "0"
        case -1200720094 => "1"
        case -1200720093 => "2"
        case -1200720092 => "3"
        case -1200720091 => "4"
        case -1200720090 => "5"
        case -1200720089 => "6"
        case -1200720088 => "7"
        case -1200720087 => "8"
        case -1200720086 => "9"
        case 1432382798  => "10"
        case 1432382799  => "11"
        case 1432382800  => "12"
        case 1432382801  => "13"
        case 1432382802  => "14"
        case 1432382803  => "15"
        case 1432382804  => "16"
        case 1432382805  => "17"
        case 1432382806  => "18"
        case 1432382807  => "19"
        case 1432382829  => "20"
        case 1432382830  => "21"
        case 1432382831  => "22"
        case 1432382832  => "23"
        case 1432382833  => "24"
        case 1432382834  => "25"
        case 1432382835  => "26"
        case 1432382836  => "27"
        case 1432382837  => "28"
        case 1432382838  => "29"
        case 1432382860  => "30"
        case 1432382861  => "31"
        case 1432382862  => "32"
        case 1432382863  => "33"
        case 1432382864  => "34"
        case 1432382865  => "35"
        case 1432382866  => "36"
        case 1432382867  => "37"
        case 1432382868  => "38"
        case 1432382869  => "39"
        case 1432382891  => "40"
        case 1432382892  => "41"
        case 1432382893  => "42"
        case 1432382894  => "43"
        case 1432382895  => "44"
        case 1432382896  => "45"
        case 1432382897  => "46"
        case 1432382898  => "47"
        case 1432382899  => "48"
        case 1432382900  => "49"
        case 1432382922  => "50"
        case 1432382923  => "51"
        case 1432382924  => "52"
        case 1432382925  => "53"
        case 1432382926  => "54"
        case 1432382927  => "55"
        case 1432382928  => "56"
        case 1432382929  => "57"
        case 1432382930  => "58"
        case 1432382931  => "59"
        case 1432382953  => "60"
        case 1432382954  => "61"
        case 1432382955  => "62"
        case 1432382956  => "63"
        case 1432382957  => "64"
        case 1432382958  => "65"
        case 1432382959  => "66"
        case 1432382960  => "67"
        case 1432382961  => "68"
        case 1432382962  => "69"
        case 1432382984  => "70"
        case 1432382985  => "71"
        case 1432382986  => "72"
        case 1432382987  => "73"
        case 1432382988  => "74"
        case 1432382989  => "75"
        case 1432382990  => "76"
        case 1432382991  => "77"
        case 1432382992  => "78"
        case 1432382993  => "79"
        case 1432383015  => "80"
        case 1432383016  => "81"
        case 1432383017  => "82"
        case 1432383018  => "83"
        case 1432383019  => "84"
        case 1432383020  => "85"
        case 1432383021  => "86"
        case 1432383022  => "87"
        case 1432383023  => "88"
        case 1432383024  => "89"
        case 1432383046  => "90"
        case 1432383047  => "91"
        case 1432383048  => "92"
        case 1432383049  => "93"
        case 1432383050  => "94"
        case 1432383051  => "95"
        case 1432383052  => "96"
        case 1432383053  => "97"
        case 1432383054  => "98"
        case 1432383055  => "99"
        case 1454193826  => "100"
        case 1454193827  => "101"
        case 1454193828  => "102"
        case 1454193829  => "103"
        case 1454193830  => "104"
        case 1454193831  => "105"
        case 1454193832  => "106"
        case 1454193833  => "107"
        case 1454193834  => "108"
        case 1454193835  => "109"
        case 1454193857  => "110"
        case 1454193858  => "111"
        case 1454193859  => "112"
        case 1454193860  => "113"
        case 1454193861  => "114"
        case 1454193862  => "115"
        case 1454193863  => "116"
        case 1454193864  => "117"
        case 1454193865  => "118"
        case 1454193866  => "119"
        case 1454193888  => "120"
        case 1454193889  => "121"
        case 1454193890  => "122"
        case 1454193891  => "123"
        case 1454193892  => "124"
        case 1454193893  => "125"
        case 1454193894  => "126"
        case 1454193895  => "127"
        case 1454193896  => "128"
        case 1454193897  => "129"
        case 1454193919  => "130"
        case 1454193920  => "131"
        case 1454193921  => "132"
        case 1454193922  => "133"
        case 1454193923  => "134"
        case 1454193924  => "135"
        case 1454193925  => "136"
        case 1454193926  => "137"
        case 1454193927  => "138"
        case 1454193928  => "139"
        case 1454193950  => "140"
        case 1454193951  => "141"
        case 1454193952  => "142"
        case 1454193953  => "143"
        case 1454193954  => "144"
        case 1454193955  => "145"
        case 1454193956  => "146"
        case 1454193957  => "147"
        case 1454193958  => "148"
        case 1454193959  => "149"
        case 1454193981  => "150"
        case 1454193982  => "151"
        case 1454193983  => "152"
        case 1454193984  => "153"
        case 1454193985  => "154"
        case 1454193986  => "155"
        case 1454193987  => "156"
        case 1454193988  => "157"
        case 1454193989  => "158"
        case 1454193990  => "159"
        case 1454194012  => "160"
        case 1454194013  => "161"
        case 1454194014  => "162"
        case 1454194015  => "163"
        case 1454194016  => "164"
        case 1454194017  => "165"
        case 1454194018  => "166"
        case 1454194019  => "167"
        case 1454194020  => "168"
        case 1454194021  => "169"
        case 1454194043  => "170"
        case 1454194044  => "171"
        case 1454194045  => "172"
        case 1454194046  => "173"
        case 1454194047  => "174"
        case 1454194048  => "175"
        case 1454194049  => "176"
        case 1454194050  => "177"
        case 1454194051  => "178"
        case 1454194052  => "179"
        case 1454194074  => "180"
        case 1454194075  => "181"
        case 1454194076  => "182"
        case 1454194077  => "183"
        case 1454194078  => "184"
        case 1454194079  => "185"
        case 1454194080  => "186"
        case 1454194081  => "187"
        case 1454194082  => "188"
        case 1454194083  => "189"
        case 1454194105  => "190"
        case 1454194106  => "191"
        case 1454194107  => "192"
        case 1454194108  => "193"
        case 1454194109  => "194"
        case 1454194110  => "195"
        case 1454194111  => "196"
        case 1454194112  => "197"
        case 1454194113  => "198"
        case 1454194114  => "199"
        case 1454194787  => "200"
        case 1454194788  => "201"
        case 1454194789  => "202"
        case 1454194790  => "203"
        case 1454194791  => "204"
        case 1454194792  => "205"
        case 1454194793  => "206"
        case 1454194794  => "207"
        case 1454194795  => "208"
        case 1454194796  => "209"
        case 1454194818  => "210"
        case 1454194819  => "211"
        case 1454194820  => "212"
        case 1454194821  => "213"
        case 1454194822  => "214"
        case 1454194823  => "215"
        case 1454194824  => "216"
        case 1454194825  => "217"
        case 1454194826  => "218"
        case 1454194827  => "219"
        case 1454194849  => "220"
        case 1454194850  => "221"
        case 1454194851  => "222"
        case 1454194852  => "223"
        case 1454194853  => "224"
        case 1454194854  => "225"
        case 1454194855  => "226"
        case 1454194856  => "227"
        case 1454194857  => "228"
        case 1454194858  => "229"
        case 1454194880  => "230"
        case 1454194881  => "231"
        case 1454194882  => "232"
        case 1454194883  => "233"
        case 1454194884  => "234"
        case 1454194885  => "235"
        case 1454194886  => "236"
        case 1454194887  => "237"
        case 1454194888  => "238"
        case 1454194889  => "239"
        case 1454194911  => "240"
        case 1454194912  => "241"
        case 1454194913  => "242"
        case 1454194914  => "243"
        case 1454194915  => "244"
        case 1454194916  => "245"
        case 1454194917  => "246"
        case 1454194918  => "247"
        case 1454194919  => "248"
        case 1454194920  => "249"
        case 1454194942  => "250"
        case 1454194943  => "251"
        case 1454194944  => "252"
        case 1454194945  => "253"
        case 1454194946  => "254"
        case 1454194947  => "255"
        case hashCode    => throw new MatchError(s"No case for: $name -> $cls -> $hashCode")
      }
      bh.consume(x)
      i += 1
    }
  }

/*
  This benchmark compares pattern matching to alternatives, specifically:
  1. using virtual methods instead (like our Tree#transform/traverse)
  2. doing a tableswitch on int field (like our Promise.Transformation)
  3. using a ClassValue as a more efficient way to store the int (like exotic's TypeSwitch)
  4. using the instance's class's name's hash, which are all memoised, in a jumptable

  The results appear to indicate that:

  1. < 16 cases, patmat beats virtual method calls
  2. = 16 cases, patmat vs virtual overlap in error margins
  3. > 16 cases, patmat loses to virtual method calls
  4. int switching seems to only out perform virtual at 32+ cases
  5. class name hash switching beats class value, up to 32 cases (and matches performance at 64)
*/
}

final class NameClassValue extends ClassValue[Int] {
  def computeValue(runtimeClass: Class[_]) = runtimeClass match {
    case ClsName0 => 0 case ClsName1 => 1 case ClsName2 => 2 case ClsName3 => 3 case ClsName4 => 4
    case ClsName5 => 5 case ClsName6 => 6 case ClsName7 => 7 case ClsName8 => 8 case ClsName9 => 9
    case ClsName10 => 10 case ClsName11 => 11 case ClsName12 => 12 case ClsName13 => 13 case ClsName14 => 14
    case ClsName15 => 15 case ClsName16 => 16 case ClsName17 => 17 case ClsName18 => 18 case ClsName19 => 19
    case ClsName20 => 20 case ClsName21 => 21 case ClsName22 => 22 case ClsName23 => 23 case ClsName24 => 24
    case ClsName25 => 25 case ClsName26 => 26 case ClsName27 => 27 case ClsName28 => 28 case ClsName29 => 29
    case ClsName30 => 30 case ClsName31 => 31 case ClsName32 => 32 case ClsName33 => 33 case ClsName34 => 34
    case ClsName35 => 35 case ClsName36 => 36 case ClsName37 => 37 case ClsName38 => 38 case ClsName39 => 39
    case ClsName40 => 40 case ClsName41 => 41 case ClsName42 => 42 case ClsName43 => 43 case ClsName44 => 44
    case ClsName45 => 45 case ClsName46 => 46 case ClsName47 => 47 case ClsName48 => 48 case ClsName49 => 49
    case ClsName50 => 50 case ClsName51 => 51 case ClsName52 => 52 case ClsName53 => 53 case ClsName54 => 54
    case ClsName55 => 55 case ClsName56 => 56 case ClsName57 => 57 case ClsName58 => 58 case ClsName59 => 59
    case ClsName60 => 60 case ClsName61 => 61 case ClsName62 => 62 case ClsName63 => 63 case ClsName64 => 64
    case ClsName65 => 65 case ClsName66 => 66 case ClsName67 => 67 case ClsName68 => 68 case ClsName69 => 69
    case ClsName70 => 70 case ClsName71 => 71 case ClsName72 => 72 case ClsName73 => 73 case ClsName74 => 74
    case ClsName75 => 75 case ClsName76 => 76 case ClsName77 => 77 case ClsName78 => 78 case ClsName79 => 79
    case ClsName80 => 80 case ClsName81 => 81 case ClsName82 => 82 case ClsName83 => 83 case ClsName84 => 84
    case ClsName85 => 85 case ClsName86 => 86 case ClsName87 => 87 case ClsName88 => 88 case ClsName89 => 89
    case ClsName90 => 90 case ClsName91 => 91 case ClsName92 => 92 case ClsName93 => 93 case ClsName94 => 94
    case ClsName95 => 95 case ClsName96 => 96 case ClsName97 => 97 case ClsName98 => 98 case ClsName99 => 99
    case ClsName100 => 100 case ClsName101 => 101 case ClsName102 => 102 case ClsName103 => 103 case ClsName104 => 104
    case ClsName105 => 105 case ClsName106 => 106 case ClsName107 => 107 case ClsName108 => 108 case ClsName109 => 109
    case ClsName110 => 110 case ClsName111 => 111 case ClsName112 => 112 case ClsName113 => 113 case ClsName114 => 114
    case ClsName115 => 115 case ClsName116 => 116 case ClsName117 => 117 case ClsName118 => 118 case ClsName119 => 119
    case ClsName120 => 120 case ClsName121 => 121 case ClsName122 => 122 case ClsName123 => 123 case ClsName124 => 124
    case ClsName125 => 125 case ClsName126 => 126 case ClsName127 => 127 case ClsName128 => 128 case ClsName129 => 129
    case ClsName130 => 130 case ClsName131 => 131 case ClsName132 => 132 case ClsName133 => 133 case ClsName134 => 134
    case ClsName135 => 135 case ClsName136 => 136 case ClsName137 => 137 case ClsName138 => 138 case ClsName139 => 139
    case ClsName140 => 140 case ClsName141 => 141 case ClsName142 => 142 case ClsName143 => 143 case ClsName144 => 144
    case ClsName145 => 145 case ClsName146 => 146 case ClsName147 => 147 case ClsName148 => 148 case ClsName149 => 149
    case ClsName150 => 150 case ClsName151 => 151 case ClsName152 => 152 case ClsName153 => 153 case ClsName154 => 154
    case ClsName155 => 155 case ClsName156 => 156 case ClsName157 => 157 case ClsName158 => 158 case ClsName159 => 159
    case ClsName160 => 160 case ClsName161 => 161 case ClsName162 => 162 case ClsName163 => 163 case ClsName164 => 164
    case ClsName165 => 165 case ClsName166 => 166 case ClsName167 => 167 case ClsName168 => 168 case ClsName169 => 169
    case ClsName170 => 170 case ClsName171 => 171 case ClsName172 => 172 case ClsName173 => 173 case ClsName174 => 174
    case ClsName175 => 175 case ClsName176 => 176 case ClsName177 => 177 case ClsName178 => 178 case ClsName179 => 179
    case ClsName180 => 180 case ClsName181 => 181 case ClsName182 => 182 case ClsName183 => 183 case ClsName184 => 184
    case ClsName185 => 185 case ClsName186 => 186 case ClsName187 => 187 case ClsName188 => 188 case ClsName189 => 189
    case ClsName190 => 190 case ClsName191 => 191 case ClsName192 => 192 case ClsName193 => 193 case ClsName194 => 194
    case ClsName195 => 195 case ClsName196 => 196 case ClsName197 => 197 case ClsName198 => 198 case ClsName199 => 199
    case ClsName200 => 200 case ClsName201 => 201 case ClsName202 => 202 case ClsName203 => 203 case ClsName204 => 204
    case ClsName205 => 205 case ClsName206 => 206 case ClsName207 => 207 case ClsName208 => 208 case ClsName209 => 209
    case ClsName210 => 210 case ClsName211 => 211 case ClsName212 => 212 case ClsName213 => 213 case ClsName214 => 214
    case ClsName215 => 215 case ClsName216 => 216 case ClsName217 => 217 case ClsName218 => 218 case ClsName219 => 219
    case ClsName220 => 220 case ClsName221 => 221 case ClsName222 => 222 case ClsName223 => 223 case ClsName224 => 224
    case ClsName225 => 225 case ClsName226 => 226 case ClsName227 => 227 case ClsName228 => 228 case ClsName229 => 229
    case ClsName230 => 230 case ClsName231 => 231 case ClsName232 => 232 case ClsName233 => 233 case ClsName234 => 234
    case ClsName235 => 235 case ClsName236 => 236 case ClsName237 => 237 case ClsName238 => 238 case ClsName239 => 239
    case ClsName240 => 240 case ClsName241 => 241 case ClsName242 => 242 case ClsName243 => 243 case ClsName244 => 244
    case ClsName245 => 245 case ClsName246 => 246 case ClsName247 => 247 case ClsName248 => 248 case ClsName249 => 249
    case ClsName250 => 250 case ClsName251 => 251 case ClsName252 => 252 case ClsName253 => 253 case ClsName254 => 254
    case ClsName255 => 255
  }

  private val ClsName0 = classOf[Name0]
  private val ClsName1 = classOf[Name1]
  private val ClsName2 = classOf[Name2]
  private val ClsName3 = classOf[Name3]
  private val ClsName4 = classOf[Name4]
  private val ClsName5 = classOf[Name5]
  private val ClsName6 = classOf[Name6]
  private val ClsName7 = classOf[Name7]
  private val ClsName8 = classOf[Name8]
  private val ClsName9 = classOf[Name9]
  private val ClsName10 = classOf[Name10]
  private val ClsName11 = classOf[Name11]
  private val ClsName12 = classOf[Name12]
  private val ClsName13 = classOf[Name13]
  private val ClsName14 = classOf[Name14]
  private val ClsName15 = classOf[Name15]
  private val ClsName16 = classOf[Name16]
  private val ClsName17 = classOf[Name17]
  private val ClsName18 = classOf[Name18]
  private val ClsName19 = classOf[Name19]
  private val ClsName20 = classOf[Name20]
  private val ClsName21 = classOf[Name21]
  private val ClsName22 = classOf[Name22]
  private val ClsName23 = classOf[Name23]
  private val ClsName24 = classOf[Name24]
  private val ClsName25 = classOf[Name25]
  private val ClsName26 = classOf[Name26]
  private val ClsName27 = classOf[Name27]
  private val ClsName28 = classOf[Name28]
  private val ClsName29 = classOf[Name29]
  private val ClsName30 = classOf[Name30]
  private val ClsName31 = classOf[Name31]
  private val ClsName32 = classOf[Name32]
  private val ClsName33 = classOf[Name33]
  private val ClsName34 = classOf[Name34]
  private val ClsName35 = classOf[Name35]
  private val ClsName36 = classOf[Name36]
  private val ClsName37 = classOf[Name37]
  private val ClsName38 = classOf[Name38]
  private val ClsName39 = classOf[Name39]
  private val ClsName40 = classOf[Name40]
  private val ClsName41 = classOf[Name41]
  private val ClsName42 = classOf[Name42]
  private val ClsName43 = classOf[Name43]
  private val ClsName44 = classOf[Name44]
  private val ClsName45 = classOf[Name45]
  private val ClsName46 = classOf[Name46]
  private val ClsName47 = classOf[Name47]
  private val ClsName48 = classOf[Name48]
  private val ClsName49 = classOf[Name49]
  private val ClsName50 = classOf[Name50]
  private val ClsName51 = classOf[Name51]
  private val ClsName52 = classOf[Name52]
  private val ClsName53 = classOf[Name53]
  private val ClsName54 = classOf[Name54]
  private val ClsName55 = classOf[Name55]
  private val ClsName56 = classOf[Name56]
  private val ClsName57 = classOf[Name57]
  private val ClsName58 = classOf[Name58]
  private val ClsName59 = classOf[Name59]
  private val ClsName60 = classOf[Name60]
  private val ClsName61 = classOf[Name61]
  private val ClsName62 = classOf[Name62]
  private val ClsName63 = classOf[Name63]
  private val ClsName64 = classOf[Name64]
  private val ClsName65 = classOf[Name65]
  private val ClsName66 = classOf[Name66]
  private val ClsName67 = classOf[Name67]
  private val ClsName68 = classOf[Name68]
  private val ClsName69 = classOf[Name69]
  private val ClsName70 = classOf[Name70]
  private val ClsName71 = classOf[Name71]
  private val ClsName72 = classOf[Name72]
  private val ClsName73 = classOf[Name73]
  private val ClsName74 = classOf[Name74]
  private val ClsName75 = classOf[Name75]
  private val ClsName76 = classOf[Name76]
  private val ClsName77 = classOf[Name77]
  private val ClsName78 = classOf[Name78]
  private val ClsName79 = classOf[Name79]
  private val ClsName80 = classOf[Name80]
  private val ClsName81 = classOf[Name81]
  private val ClsName82 = classOf[Name82]
  private val ClsName83 = classOf[Name83]
  private val ClsName84 = classOf[Name84]
  private val ClsName85 = classOf[Name85]
  private val ClsName86 = classOf[Name86]
  private val ClsName87 = classOf[Name87]
  private val ClsName88 = classOf[Name88]
  private val ClsName89 = classOf[Name89]
  private val ClsName90 = classOf[Name90]
  private val ClsName91 = classOf[Name91]
  private val ClsName92 = classOf[Name92]
  private val ClsName93 = classOf[Name93]
  private val ClsName94 = classOf[Name94]
  private val ClsName95 = classOf[Name95]
  private val ClsName96 = classOf[Name96]
  private val ClsName97 = classOf[Name97]
  private val ClsName98 = classOf[Name98]
  private val ClsName99 = classOf[Name99]
  private val ClsName100 = classOf[Name100]
  private val ClsName101 = classOf[Name101]
  private val ClsName102 = classOf[Name102]
  private val ClsName103 = classOf[Name103]
  private val ClsName104 = classOf[Name104]
  private val ClsName105 = classOf[Name105]
  private val ClsName106 = classOf[Name106]
  private val ClsName107 = classOf[Name107]
  private val ClsName108 = classOf[Name108]
  private val ClsName109 = classOf[Name109]
  private val ClsName110 = classOf[Name110]
  private val ClsName111 = classOf[Name111]
  private val ClsName112 = classOf[Name112]
  private val ClsName113 = classOf[Name113]
  private val ClsName114 = classOf[Name114]
  private val ClsName115 = classOf[Name115]
  private val ClsName116 = classOf[Name116]
  private val ClsName117 = classOf[Name117]
  private val ClsName118 = classOf[Name118]
  private val ClsName119 = classOf[Name119]
  private val ClsName120 = classOf[Name120]
  private val ClsName121 = classOf[Name121]
  private val ClsName122 = classOf[Name122]
  private val ClsName123 = classOf[Name123]
  private val ClsName124 = classOf[Name124]
  private val ClsName125 = classOf[Name125]
  private val ClsName126 = classOf[Name126]
  private val ClsName127 = classOf[Name127]
  private val ClsName128 = classOf[Name128]
  private val ClsName129 = classOf[Name129]
  private val ClsName130 = classOf[Name130]
  private val ClsName131 = classOf[Name131]
  private val ClsName132 = classOf[Name132]
  private val ClsName133 = classOf[Name133]
  private val ClsName134 = classOf[Name134]
  private val ClsName135 = classOf[Name135]
  private val ClsName136 = classOf[Name136]
  private val ClsName137 = classOf[Name137]
  private val ClsName138 = classOf[Name138]
  private val ClsName139 = classOf[Name139]
  private val ClsName140 = classOf[Name140]
  private val ClsName141 = classOf[Name141]
  private val ClsName142 = classOf[Name142]
  private val ClsName143 = classOf[Name143]
  private val ClsName144 = classOf[Name144]
  private val ClsName145 = classOf[Name145]
  private val ClsName146 = classOf[Name146]
  private val ClsName147 = classOf[Name147]
  private val ClsName148 = classOf[Name148]
  private val ClsName149 = classOf[Name149]
  private val ClsName150 = classOf[Name150]
  private val ClsName151 = classOf[Name151]
  private val ClsName152 = classOf[Name152]
  private val ClsName153 = classOf[Name153]
  private val ClsName154 = classOf[Name154]
  private val ClsName155 = classOf[Name155]
  private val ClsName156 = classOf[Name156]
  private val ClsName157 = classOf[Name157]
  private val ClsName158 = classOf[Name158]
  private val ClsName159 = classOf[Name159]
  private val ClsName160 = classOf[Name160]
  private val ClsName161 = classOf[Name161]
  private val ClsName162 = classOf[Name162]
  private val ClsName163 = classOf[Name163]
  private val ClsName164 = classOf[Name164]
  private val ClsName165 = classOf[Name165]
  private val ClsName166 = classOf[Name166]
  private val ClsName167 = classOf[Name167]
  private val ClsName168 = classOf[Name168]
  private val ClsName169 = classOf[Name169]
  private val ClsName170 = classOf[Name170]
  private val ClsName171 = classOf[Name171]
  private val ClsName172 = classOf[Name172]
  private val ClsName173 = classOf[Name173]
  private val ClsName174 = classOf[Name174]
  private val ClsName175 = classOf[Name175]
  private val ClsName176 = classOf[Name176]
  private val ClsName177 = classOf[Name177]
  private val ClsName178 = classOf[Name178]
  private val ClsName179 = classOf[Name179]
  private val ClsName180 = classOf[Name180]
  private val ClsName181 = classOf[Name181]
  private val ClsName182 = classOf[Name182]
  private val ClsName183 = classOf[Name183]
  private val ClsName184 = classOf[Name184]
  private val ClsName185 = classOf[Name185]
  private val ClsName186 = classOf[Name186]
  private val ClsName187 = classOf[Name187]
  private val ClsName188 = classOf[Name188]
  private val ClsName189 = classOf[Name189]
  private val ClsName190 = classOf[Name190]
  private val ClsName191 = classOf[Name191]
  private val ClsName192 = classOf[Name192]
  private val ClsName193 = classOf[Name193]
  private val ClsName194 = classOf[Name194]
  private val ClsName195 = classOf[Name195]
  private val ClsName196 = classOf[Name196]
  private val ClsName197 = classOf[Name197]
  private val ClsName198 = classOf[Name198]
  private val ClsName199 = classOf[Name199]
  private val ClsName200 = classOf[Name200]
  private val ClsName201 = classOf[Name201]
  private val ClsName202 = classOf[Name202]
  private val ClsName203 = classOf[Name203]
  private val ClsName204 = classOf[Name204]
  private val ClsName205 = classOf[Name205]
  private val ClsName206 = classOf[Name206]
  private val ClsName207 = classOf[Name207]
  private val ClsName208 = classOf[Name208]
  private val ClsName209 = classOf[Name209]
  private val ClsName210 = classOf[Name210]
  private val ClsName211 = classOf[Name211]
  private val ClsName212 = classOf[Name212]
  private val ClsName213 = classOf[Name213]
  private val ClsName214 = classOf[Name214]
  private val ClsName215 = classOf[Name215]
  private val ClsName216 = classOf[Name216]
  private val ClsName217 = classOf[Name217]
  private val ClsName218 = classOf[Name218]
  private val ClsName219 = classOf[Name219]
  private val ClsName220 = classOf[Name220]
  private val ClsName221 = classOf[Name221]
  private val ClsName222 = classOf[Name222]
  private val ClsName223 = classOf[Name223]
  private val ClsName224 = classOf[Name224]
  private val ClsName225 = classOf[Name225]
  private val ClsName226 = classOf[Name226]
  private val ClsName227 = classOf[Name227]
  private val ClsName228 = classOf[Name228]
  private val ClsName229 = classOf[Name229]
  private val ClsName230 = classOf[Name230]
  private val ClsName231 = classOf[Name231]
  private val ClsName232 = classOf[Name232]
  private val ClsName233 = classOf[Name233]
  private val ClsName234 = classOf[Name234]
  private val ClsName235 = classOf[Name235]
  private val ClsName236 = classOf[Name236]
  private val ClsName237 = classOf[Name237]
  private val ClsName238 = classOf[Name238]
  private val ClsName239 = classOf[Name239]
  private val ClsName240 = classOf[Name240]
  private val ClsName241 = classOf[Name241]
  private val ClsName242 = classOf[Name242]
  private val ClsName243 = classOf[Name243]
  private val ClsName244 = classOf[Name244]
  private val ClsName245 = classOf[Name245]
  private val ClsName246 = classOf[Name246]
  private val ClsName247 = classOf[Name247]
  private val ClsName248 = classOf[Name248]
  private val ClsName249 = classOf[Name249]
  private val ClsName250 = classOf[Name250]
  private val ClsName251 = classOf[Name251]
  private val ClsName252 = classOf[Name252]
  private val ClsName253 = classOf[Name253]
  private val ClsName254 = classOf[Name254]
  private val ClsName255 = classOf[Name255]
}

sealed abstract class Name(val _id: Int) {
  def virtualShow: String
}

final case class Name0() extends Name(0) { def virtualShow = "0" }
final case class Name1() extends Name(1) { def virtualShow = "1" }
final case class Name2() extends Name(2) { def virtualShow = "2" }
final case class Name3() extends Name(3) { def virtualShow = "3" }
final case class Name4() extends Name(4) { def virtualShow = "4" }
final case class Name5() extends Name(5) { def virtualShow = "5" }
final case class Name6() extends Name(6) { def virtualShow = "6" }
final case class Name7() extends Name(7) { def virtualShow = "7" }
final case class Name8() extends Name(8) { def virtualShow = "8" }
final case class Name9() extends Name(9) { def virtualShow = "9" }
final case class Name10() extends Name(10) { def virtualShow = "10" }
final case class Name11() extends Name(11) { def virtualShow = "11" }
final case class Name12() extends Name(12) { def virtualShow = "12" }
final case class Name13() extends Name(13) { def virtualShow = "13" }
final case class Name14() extends Name(14) { def virtualShow = "14" }
final case class Name15() extends Name(15) { def virtualShow = "15" }
final case class Name16() extends Name(16) { def virtualShow = "16" }
final case class Name17() extends Name(17) { def virtualShow = "17" }
final case class Name18() extends Name(18) { def virtualShow = "18" }
final case class Name19() extends Name(19) { def virtualShow = "19" }
final case class Name20() extends Name(20) { def virtualShow = "20" }
final case class Name21() extends Name(21) { def virtualShow = "21" }
final case class Name22() extends Name(22) { def virtualShow = "22" }
final case class Name23() extends Name(23) { def virtualShow = "23" }
final case class Name24() extends Name(24) { def virtualShow = "24" }
final case class Name25() extends Name(25) { def virtualShow = "25" }
final case class Name26() extends Name(26) { def virtualShow = "26" }
final case class Name27() extends Name(27) { def virtualShow = "27" }
final case class Name28() extends Name(28) { def virtualShow = "28" }
final case class Name29() extends Name(29) { def virtualShow = "29" }
final case class Name30() extends Name(30) { def virtualShow = "30" }
final case class Name31() extends Name(31) { def virtualShow = "31" }
final case class Name32() extends Name(32) { def virtualShow = "32" }
final case class Name33() extends Name(33) { def virtualShow = "33" }
final case class Name34() extends Name(34) { def virtualShow = "34" }
final case class Name35() extends Name(35) { def virtualShow = "35" }
final case class Name36() extends Name(36) { def virtualShow = "36" }
final case class Name37() extends Name(37) { def virtualShow = "37" }
final case class Name38() extends Name(38) { def virtualShow = "38" }
final case class Name39() extends Name(39) { def virtualShow = "39" }
final case class Name40() extends Name(40) { def virtualShow = "40" }
final case class Name41() extends Name(41) { def virtualShow = "41" }
final case class Name42() extends Name(42) { def virtualShow = "42" }
final case class Name43() extends Name(43) { def virtualShow = "43" }
final case class Name44() extends Name(44) { def virtualShow = "44" }
final case class Name45() extends Name(45) { def virtualShow = "45" }
final case class Name46() extends Name(46) { def virtualShow = "46" }
final case class Name47() extends Name(47) { def virtualShow = "47" }
final case class Name48() extends Name(48) { def virtualShow = "48" }
final case class Name49() extends Name(49) { def virtualShow = "49" }
final case class Name50() extends Name(50) { def virtualShow = "50" }
final case class Name51() extends Name(51) { def virtualShow = "51" }
final case class Name52() extends Name(52) { def virtualShow = "52" }
final case class Name53() extends Name(53) { def virtualShow = "53" }
final case class Name54() extends Name(54) { def virtualShow = "54" }
final case class Name55() extends Name(55) { def virtualShow = "55" }
final case class Name56() extends Name(56) { def virtualShow = "56" }
final case class Name57() extends Name(57) { def virtualShow = "57" }
final case class Name58() extends Name(58) { def virtualShow = "58" }
final case class Name59() extends Name(59) { def virtualShow = "59" }
final case class Name60() extends Name(60) { def virtualShow = "60" }
final case class Name61() extends Name(61) { def virtualShow = "61" }
final case class Name62() extends Name(62) { def virtualShow = "62" }
final case class Name63() extends Name(63) { def virtualShow = "63" }
final case class Name64() extends Name(64) { def virtualShow = "64" }
final case class Name65() extends Name(65) { def virtualShow = "65" }
final case class Name66() extends Name(66) { def virtualShow = "66" }
final case class Name67() extends Name(67) { def virtualShow = "67" }
final case class Name68() extends Name(68) { def virtualShow = "68" }
final case class Name69() extends Name(69) { def virtualShow = "69" }
final case class Name70() extends Name(70) { def virtualShow = "70" }
final case class Name71() extends Name(71) { def virtualShow = "71" }
final case class Name72() extends Name(72) { def virtualShow = "72" }
final case class Name73() extends Name(73) { def virtualShow = "73" }
final case class Name74() extends Name(74) { def virtualShow = "74" }
final case class Name75() extends Name(75) { def virtualShow = "75" }
final case class Name76() extends Name(76) { def virtualShow = "76" }
final case class Name77() extends Name(77) { def virtualShow = "77" }
final case class Name78() extends Name(78) { def virtualShow = "78" }
final case class Name79() extends Name(79) { def virtualShow = "79" }
final case class Name80() extends Name(80) { def virtualShow = "80" }
final case class Name81() extends Name(81) { def virtualShow = "81" }
final case class Name82() extends Name(82) { def virtualShow = "82" }
final case class Name83() extends Name(83) { def virtualShow = "83" }
final case class Name84() extends Name(84) { def virtualShow = "84" }
final case class Name85() extends Name(85) { def virtualShow = "85" }
final case class Name86() extends Name(86) { def virtualShow = "86" }
final case class Name87() extends Name(87) { def virtualShow = "87" }
final case class Name88() extends Name(88) { def virtualShow = "88" }
final case class Name89() extends Name(89) { def virtualShow = "89" }
final case class Name90() extends Name(90) { def virtualShow = "90" }
final case class Name91() extends Name(91) { def virtualShow = "91" }
final case class Name92() extends Name(92) { def virtualShow = "92" }
final case class Name93() extends Name(93) { def virtualShow = "93" }
final case class Name94() extends Name(94) { def virtualShow = "94" }
final case class Name95() extends Name(95) { def virtualShow = "95" }
final case class Name96() extends Name(96) { def virtualShow = "96" }
final case class Name97() extends Name(97) { def virtualShow = "97" }
final case class Name98() extends Name(98) { def virtualShow = "98" }
final case class Name99() extends Name(99) { def virtualShow = "99" }
final case class Name100() extends Name(100) { def virtualShow = "100" }
final case class Name101() extends Name(101) { def virtualShow = "101" }
final case class Name102() extends Name(102) { def virtualShow = "102" }
final case class Name103() extends Name(103) { def virtualShow = "103" }
final case class Name104() extends Name(104) { def virtualShow = "104" }
final case class Name105() extends Name(105) { def virtualShow = "105" }
final case class Name106() extends Name(106) { def virtualShow = "106" }
final case class Name107() extends Name(107) { def virtualShow = "107" }
final case class Name108() extends Name(108) { def virtualShow = "108" }
final case class Name109() extends Name(109) { def virtualShow = "109" }
final case class Name110() extends Name(110) { def virtualShow = "110" }
final case class Name111() extends Name(111) { def virtualShow = "111" }
final case class Name112() extends Name(112) { def virtualShow = "112" }
final case class Name113() extends Name(113) { def virtualShow = "113" }
final case class Name114() extends Name(114) { def virtualShow = "114" }
final case class Name115() extends Name(115) { def virtualShow = "115" }
final case class Name116() extends Name(116) { def virtualShow = "116" }
final case class Name117() extends Name(117) { def virtualShow = "117" }
final case class Name118() extends Name(118) { def virtualShow = "118" }
final case class Name119() extends Name(119) { def virtualShow = "119" }
final case class Name120() extends Name(120) { def virtualShow = "120" }
final case class Name121() extends Name(121) { def virtualShow = "121" }
final case class Name122() extends Name(122) { def virtualShow = "122" }
final case class Name123() extends Name(123) { def virtualShow = "123" }
final case class Name124() extends Name(124) { def virtualShow = "124" }
final case class Name125() extends Name(125) { def virtualShow = "125" }
final case class Name126() extends Name(126) { def virtualShow = "126" }
final case class Name127() extends Name(127) { def virtualShow = "127" }
final case class Name128() extends Name(128) { def virtualShow = "128" }
final case class Name129() extends Name(129) { def virtualShow = "129" }
final case class Name130() extends Name(130) { def virtualShow = "130" }
final case class Name131() extends Name(131) { def virtualShow = "131" }
final case class Name132() extends Name(132) { def virtualShow = "132" }
final case class Name133() extends Name(133) { def virtualShow = "133" }
final case class Name134() extends Name(134) { def virtualShow = "134" }
final case class Name135() extends Name(135) { def virtualShow = "135" }
final case class Name136() extends Name(136) { def virtualShow = "136" }
final case class Name137() extends Name(137) { def virtualShow = "137" }
final case class Name138() extends Name(138) { def virtualShow = "138" }
final case class Name139() extends Name(139) { def virtualShow = "139" }
final case class Name140() extends Name(140) { def virtualShow = "140" }
final case class Name141() extends Name(141) { def virtualShow = "141" }
final case class Name142() extends Name(142) { def virtualShow = "142" }
final case class Name143() extends Name(143) { def virtualShow = "143" }
final case class Name144() extends Name(144) { def virtualShow = "144" }
final case class Name145() extends Name(145) { def virtualShow = "145" }
final case class Name146() extends Name(146) { def virtualShow = "146" }
final case class Name147() extends Name(147) { def virtualShow = "147" }
final case class Name148() extends Name(148) { def virtualShow = "148" }
final case class Name149() extends Name(149) { def virtualShow = "149" }
final case class Name150() extends Name(150) { def virtualShow = "150" }
final case class Name151() extends Name(151) { def virtualShow = "151" }
final case class Name152() extends Name(152) { def virtualShow = "152" }
final case class Name153() extends Name(153) { def virtualShow = "153" }
final case class Name154() extends Name(154) { def virtualShow = "154" }
final case class Name155() extends Name(155) { def virtualShow = "155" }
final case class Name156() extends Name(156) { def virtualShow = "156" }
final case class Name157() extends Name(157) { def virtualShow = "157" }
final case class Name158() extends Name(158) { def virtualShow = "158" }
final case class Name159() extends Name(159) { def virtualShow = "159" }
final case class Name160() extends Name(160) { def virtualShow = "160" }
final case class Name161() extends Name(161) { def virtualShow = "161" }
final case class Name162() extends Name(162) { def virtualShow = "162" }
final case class Name163() extends Name(163) { def virtualShow = "163" }
final case class Name164() extends Name(164) { def virtualShow = "164" }
final case class Name165() extends Name(165) { def virtualShow = "165" }
final case class Name166() extends Name(166) { def virtualShow = "166" }
final case class Name167() extends Name(167) { def virtualShow = "167" }
final case class Name168() extends Name(168) { def virtualShow = "168" }
final case class Name169() extends Name(169) { def virtualShow = "169" }
final case class Name170() extends Name(170) { def virtualShow = "170" }
final case class Name171() extends Name(171) { def virtualShow = "171" }
final case class Name172() extends Name(172) { def virtualShow = "172" }
final case class Name173() extends Name(173) { def virtualShow = "173" }
final case class Name174() extends Name(174) { def virtualShow = "174" }
final case class Name175() extends Name(175) { def virtualShow = "175" }
final case class Name176() extends Name(176) { def virtualShow = "176" }
final case class Name177() extends Name(177) { def virtualShow = "177" }
final case class Name178() extends Name(178) { def virtualShow = "178" }
final case class Name179() extends Name(179) { def virtualShow = "179" }
final case class Name180() extends Name(180) { def virtualShow = "180" }
final case class Name181() extends Name(181) { def virtualShow = "181" }
final case class Name182() extends Name(182) { def virtualShow = "182" }
final case class Name183() extends Name(183) { def virtualShow = "183" }
final case class Name184() extends Name(184) { def virtualShow = "184" }
final case class Name185() extends Name(185) { def virtualShow = "185" }
final case class Name186() extends Name(186) { def virtualShow = "186" }
final case class Name187() extends Name(187) { def virtualShow = "187" }
final case class Name188() extends Name(188) { def virtualShow = "188" }
final case class Name189() extends Name(189) { def virtualShow = "189" }
final case class Name190() extends Name(190) { def virtualShow = "190" }
final case class Name191() extends Name(191) { def virtualShow = "191" }
final case class Name192() extends Name(192) { def virtualShow = "192" }
final case class Name193() extends Name(193) { def virtualShow = "193" }
final case class Name194() extends Name(194) { def virtualShow = "194" }
final case class Name195() extends Name(195) { def virtualShow = "195" }
final case class Name196() extends Name(196) { def virtualShow = "196" }
final case class Name197() extends Name(197) { def virtualShow = "197" }
final case class Name198() extends Name(198) { def virtualShow = "198" }
final case class Name199() extends Name(199) { def virtualShow = "199" }
final case class Name200() extends Name(200) { def virtualShow = "200" }
final case class Name201() extends Name(201) { def virtualShow = "201" }
final case class Name202() extends Name(202) { def virtualShow = "202" }
final case class Name203() extends Name(203) { def virtualShow = "203" }
final case class Name204() extends Name(204) { def virtualShow = "204" }
final case class Name205() extends Name(205) { def virtualShow = "205" }
final case class Name206() extends Name(206) { def virtualShow = "206" }
final case class Name207() extends Name(207) { def virtualShow = "207" }
final case class Name208() extends Name(208) { def virtualShow = "208" }
final case class Name209() extends Name(209) { def virtualShow = "209" }
final case class Name210() extends Name(210) { def virtualShow = "210" }
final case class Name211() extends Name(211) { def virtualShow = "211" }
final case class Name212() extends Name(212) { def virtualShow = "212" }
final case class Name213() extends Name(213) { def virtualShow = "213" }
final case class Name214() extends Name(214) { def virtualShow = "214" }
final case class Name215() extends Name(215) { def virtualShow = "215" }
final case class Name216() extends Name(216) { def virtualShow = "216" }
final case class Name217() extends Name(217) { def virtualShow = "217" }
final case class Name218() extends Name(218) { def virtualShow = "218" }
final case class Name219() extends Name(219) { def virtualShow = "219" }
final case class Name220() extends Name(220) { def virtualShow = "220" }
final case class Name221() extends Name(221) { def virtualShow = "221" }
final case class Name222() extends Name(222) { def virtualShow = "222" }
final case class Name223() extends Name(223) { def virtualShow = "223" }
final case class Name224() extends Name(224) { def virtualShow = "224" }
final case class Name225() extends Name(225) { def virtualShow = "225" }
final case class Name226() extends Name(226) { def virtualShow = "226" }
final case class Name227() extends Name(227) { def virtualShow = "227" }
final case class Name228() extends Name(228) { def virtualShow = "228" }
final case class Name229() extends Name(229) { def virtualShow = "229" }
final case class Name230() extends Name(230) { def virtualShow = "230" }
final case class Name231() extends Name(231) { def virtualShow = "231" }
final case class Name232() extends Name(232) { def virtualShow = "232" }
final case class Name233() extends Name(233) { def virtualShow = "233" }
final case class Name234() extends Name(234) { def virtualShow = "234" }
final case class Name235() extends Name(235) { def virtualShow = "235" }
final case class Name236() extends Name(236) { def virtualShow = "236" }
final case class Name237() extends Name(237) { def virtualShow = "237" }
final case class Name238() extends Name(238) { def virtualShow = "238" }
final case class Name239() extends Name(239) { def virtualShow = "239" }
final case class Name240() extends Name(240) { def virtualShow = "240" }
final case class Name241() extends Name(241) { def virtualShow = "241" }
final case class Name242() extends Name(242) { def virtualShow = "242" }
final case class Name243() extends Name(243) { def virtualShow = "243" }
final case class Name244() extends Name(244) { def virtualShow = "244" }
final case class Name245() extends Name(245) { def virtualShow = "245" }
final case class Name246() extends Name(246) { def virtualShow = "246" }
final case class Name247() extends Name(247) { def virtualShow = "247" }
final case class Name248() extends Name(248) { def virtualShow = "248" }
final case class Name249() extends Name(249) { def virtualShow = "249" }
final case class Name250() extends Name(250) { def virtualShow = "250" }
final case class Name251() extends Name(251) { def virtualShow = "251" }
final case class Name252() extends Name(252) { def virtualShow = "252" }
final case class Name253() extends Name(253) { def virtualShow = "253" }
final case class Name254() extends Name(254) { def virtualShow = "254" }
final case class Name255() extends Name(255) { def virtualShow = "255" }
