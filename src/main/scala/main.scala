import scala.annotation.tailrec
import scala.language.{existentials, postfixOps}
import scala.collection.mutable

extension (sc: StringContext)
  def b(args: Double*): Int = {
    // reuse the `s`-interpolator and then split on ','
    val base = "01"
    sc.s(args: _*).toList.map(base.indexOf(_)).reduceLeft(_ * base.length + _)
  }

case class Tiny(IP: Int, LI: Int, FR: Int, AC: Int, mem: Vector[Int], input: List[Int], output: List[Int] = List.empty):
  private def setZF(cond: Boolean): Int = (if (cond) b"0010" else b"0000")

  override def toString: String =
    s"IP: $IP, LI: $LI, FR: {HF: ${(FR & 8) > 0}, OF: ${(FR & 4) > 0}, ZF: ${(FR & 2) > 0}, CF: ${(FR & 1) > 0}}, AC: $AC, mem: $mem, input: $input, output: $output"

  def HLT(): Tiny = Tiny(IP + 1, LI, FR | b"1000", AC, mem, input, output)

  def JMP(addr: Int): Tiny = Tiny(IP = addr, LI, FR, AC, mem, input, output)

  def JZE(addr: Int): Tiny = Tiny(if ((FR & b"0010") > 0) mem(addr) else IP + 1, LI, FR, AC, mem, input, output)

  def JNZ(addr: Int): Tiny = Tiny(if ((FR & b"0010") == 0) mem(addr) else IP + 1, LI, FR, AC, mem, input, output)

  def LDA(addr: Int): Tiny = Tiny(IP + 1, LI, FR | setZF(mem(addr) == 0), AC = mem(addr), mem, input, output)

  def STA(addr: Int): Tiny = Tiny(IP + 1, LI, FR, AC, mem.updated(addr, AC), input, output)

  def GET(): Tiny = Tiny(IP, LI, FR, AC = input.head, mem, input.tail, output)

  def PUT(): Tiny = Tiny(IP, LI, FR, AC, mem, input, AC :: output)

  def ROL(): Tiny = //TODO: implement OF bit
    val newAC = AC << 1
    //    val (OF, AC2) = if((newAC & b"10000") > 0)
    //      (true, newAC & b"01111")
    //    else
    //      (false, newAC)
    Tiny(IP + 1, LI, FR | setZF(AC == 0), (AC << 1) + (FR & b"0001"), mem, input, output)

  def ROR(): Tiny = Tiny(IP + 1, LI, FR | setZF(AC == 0), (AC >> 1) & ((FR & b"0001") << 3), mem, input, output) //TODO:

  def ADC(addr: Int): Tiny =
    val res = AC + mem(addr) + (FR & b"0001")
    Tiny(IP + 1, LI, FR | setZF(AC == 0) | (if (res > b"1111") b"0001" else b"0000"), res % 8, mem, input, output) //TODO:

  def CCF(): Tiny = Tiny(IP + 1, LI, FR & b"1110", AC, mem, input, output)

  def SCF(): Tiny = Tiny(IP + 1, LI, FR | b"0001", AC, mem, input, output)

  def DEL(): Tiny = Tiny(IP + 1, LI - 1, FR | setZF(LI == 0), AC, mem, input, output)

  def LDL(addr: Int): Tiny = Tiny(IP + 1, LI = mem(addr), FR | setZF(LI == 0), AC, mem, input, output)

  def FLA(): Tiny = Tiny(IP + 1, LI, FR | setZF(AC == 0), -AC, mem, input, output)


enum op:
  case HLT, JMP, JZE, JNZ, LDA, STA, GET, PUT, ROL, ROR, ADC, CCF, SCF, DEL, LDL, FLA

@tailrec
def runner(tiny: Tiny, code: Vector[(op, Int)]): List[Int] =
  println(s"${Console.GREEN}$tiny${Console.RESET}")
  val (inst, addr) = code(tiny.IP)
  println(s"${Console.BLUE}$inst ${if (addr != -1) addr else ""}${Console.RESET}")
  val newTiny = inst match
    case op.HLT =>
      tiny.HLT()
      return tiny.output
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
  runner(newTiny, code)


var totalCode: mutable.ListBuffer[(op, Int)] = mutable.ListBuffer.empty

class Asm

def HLT: Asm =
  totalCode.append((op.HLT, -1))
  Asm()

extension (a: Asm)
  infix def JMP(addr: Int): Asm =
    totalCode.append((op.JMP, addr))
    Asm()

extension (a: Asm)
  infix def JZE(addr: Int): Asm =
    totalCode.append((op.JZE, addr))
    Asm()

extension (a: Asm)
  infix def JNZ(addr: Int): Asm =
    totalCode.append((op.JNZ, addr))
    Asm()

extension (a: Asm)
  infix def LDA(addr: Int): Asm =
    totalCode.append((op.LDA, addr))
    Asm()

extension (a: Asm)
  infix def STA(addr: Int): Asm =
    totalCode.append((op.STA, addr))
    Asm()

def GET: Asm =
  totalCode.append((op.GET, -1))
  Asm()

def PUT: Asm =
  totalCode.append((op.PUT, -1))
  Asm()

def ROL: Asm =
  totalCode.append((op.ROL, -1))
  Asm()

def ROR: Asm =
  totalCode.append((op.ROR, -1))
  Asm()

extension (a: Asm)
  infix def ADC(addr: Int): Asm =
    totalCode.append((op.STA, addr))
    Asm()

def CCF: Asm =
  totalCode.append((op.CCF, -1))
  Asm()

def SCF: Asm =
  totalCode.append((op.SCF, -1))
  Asm()

def DEL: Asm =
  totalCode.append((op.DEL, -1))
  Asm()

extension (a: Asm)
  def LDL(addr: Int): Asm =
    totalCode.append((op.LDL, addr))
    Asm()

def FLA: Asm =
  totalCode.append((op.FLA, -1))
  Asm()

@main
def main(): Unit =

  Asm()
    SCF
    `LDA` 1
    `ADC` 0
    `STA` 1
    `LDA` 2
    CCF
    `LDL` 1
    `JZE` 0
    DEL
    FLA
    ROL
    ROR
    HLT


  println(totalCode)


//    LDA { 1 }
//    s1 test
//
//
//    1 PRINT
//
//  val x_ = (s1 union)
//
//  x_(s2)
//
//    val c =
//      s1 union
//      s2 union
//      s2
//
//  //  val LDA = (i: Int) => _
//
//  //  LDA 1
//
//
//    val sum: Int => Int => Int = x => y => x+y
//    sum(1)(2)
//    val half: MultiSet[T] => MultiSet[T] = s1 union
//    half s2

/*
  val code = Vector[(op, Int)](
    (op.SCF, -1),
    (op.LDA, 1),
    (op.ADC, 0),
    (op.STA, 1),
    (op.LDA, 2),
    (op.CCF, -1),
    (op.LDL, 1),
    (op.JZE, 0),
    (op.DEL, -1),
    (op.FLA, -1),
    (op.ROL, -1),
    (op.ROR, -1),
    (op.HLT, -1),
  )
*/

  val input = List(1, 2, 3, 4)
  val tiny = Tiny(0, 0, b"0000", 0, Vector.fill(20)(11), input)
  val output = runner(tiny, totalCode.toVector)// code)
  println(s"final output: $output")
