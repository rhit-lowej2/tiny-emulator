import scala.annotation.tailrec

extension (sc: StringContext)
  def b(args: Double*): Int = {
    // reuse the `s`-interpolator and then split on ','
    val base = "01"
    sc.s(args: _*).toList.map(base.indexOf(_)).reduceLeft(_ * base.length + _)
  }

case class Tiny(IP: Int, LI: Int, FR: Int, AC: Int, mem: Vector[Int]):
  private def setZF(cond: Boolean): Int = (if(cond) b"0010" else b"0000")
  def HLT(): Tiny = Tiny(IP+1, LI, FR | b"1000", AC, mem)
  def JMP(addr: Int): Tiny = Tiny(IP = addr, LI, FR, AC, mem)
  def JZE(addr: Int): Tiny = Tiny(if((FR & b"0010") > 0) mem(addr) else IP+1, LI, FR, AC, mem)
  def JNZ(addr: Int): Tiny = Tiny(if ((FR & b"0010") == 0) mem(addr) else IP+1, LI, FR, AC, mem)
  def LDA(addr: Int): Tiny = Tiny(IP+1, LI, FR | setZF(mem(addr)==0), AC = mem(addr), mem)
  def STA(addr: Int): Tiny = Tiny(IP+1, LI, FR, AC, mem.updated(addr,AC))
  def GET(): Tiny = ??? //Tiny(IP, LI, FR, AC, mem.updated(addr, AC))
  def PUT(): Tiny = ??? //Tiny(IP, LI, FR, AC, mem.updated(addr, AC))
  def ROL(): Tiny = //TODO:
    val newAC = AC << 1
//    val (OF, AC2) = if((newAC & b"10000") > 0)
//      (true, newAC & b"01111")
//    else
//      (false, newAC)
    Tiny(IP+1, LI, FR | setZF(AC==0), (AC << 1) + (FR & b"0001"), mem)

  def ROR(): Tiny = Tiny(IP+1, LI, FR | setZF(AC==0), (AC >> 1) & ((FR & b"0001") << 3), mem) //TODO:
  def ADC(addr: Int): Tiny =
    val res = AC+mem(addr) + (FR & b"0001")
    Tiny(IP+1, LI, FR | setZF(AC==0) | (if(res > b"1111") b"0001" else b"0000"), res % 8, mem) //TODO:
  def CCF(): Tiny = Tiny(IP+1, LI, FR & b"1110", AC, mem)
  def SCF(): Tiny = Tiny(IP+1, LI, FR | b"0001", AC, mem)
  def DEL(): Tiny = Tiny(IP+1, LI-1, FR | setZF(LI==0), AC, mem)
  def LDL(addr: Int): Tiny = Tiny(IP+1, LI = mem(addr), FR | setZF(LI==0), AC, mem)
  def FLA(): Tiny = Tiny(IP+1, LI, FR | setZF(AC==0), -AC, mem)
  def coreDump(): String =
    s"IP: $IP, LI: $LI, FR: {HF: ${(FR & 8) > 0}, OF: ${(FR & 4) > 0}, ZF: ${(FR & 2) > 0}, CF: ${(FR & 1) > 0}}, AC: $AC, mem: $mem"


enum op:
  case HLT, JMP, JZE, JNZ, LDA, STA, GET, PUT, ROL, ROR, ADC, CCF, SCF, DEL, LDL, FLA

@tailrec
def runner(tiny: Tiny, code: Vector[(op, Int)]): Tiny =
  val ip = tiny.IP
  val (inst, addr) = code(ip)
  println(s"${Console.BLUE}$inst ${if(addr != -1) addr else ""}${Console.RESET}")
  val newTiny = inst match
    case op.HLT => return tiny.HLT()
    case op.JMP => tiny.JMP(addr)
    case op.JZE => tiny.JZE(addr)
    case op.JNZ => tiny.JNZ(addr)
    case op.LDA => tiny.LDA(addr)
    case op.STA => tiny.STA(addr)
    case op.GET => tiny.GET()
    case op.PUT => tiny.PUT()
    case op.ROL => tiny.ROL()
    case op.ROR => tiny.ROR()
    case op.ADC => tiny.ADC(addr)
    case op.CCF => tiny.CCF()
    case op.SCF => tiny.SCF()
    case op.DEL => tiny.DEL()
    case op.LDL => tiny.LDL(addr)
    case op.FLA => tiny.FLA()
  println(s"${Console.GREEN}${newTiny.coreDump()}${Console.RESET}")
  runner(newTiny, code)

@main
def main(): Unit =
  val code = Vector[(op, Int)](
    (op.SCF, -1),
    (op.LDA, 1),
    (op.ADC, 0),
    (op.STA, 1),
    (op.LDA, 2),
    (op.CCF, -1),
    (op.LDL, 1),
    (op.JZE, 0),
    (op.DEL, 1),
    (op.FLA, -1),
    (op.ROL, -1),
    (op.ROR, -1),
    (op.HLT, -1),
  )

  val tiny = Tiny(0, 0, b"0000", 0, Vector.fill(20)(11))
  runner(tiny, code)



