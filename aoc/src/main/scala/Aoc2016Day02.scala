import scala.annotation.tailrec
import scala.io.*
import scala.math.*

object Aoc2016Day02 extends App :

  enum Instruction(c: Char):
    case U extends Instruction('U')
    case D extends Instruction('D')
    case L extends Instruction('L')
    case R extends Instruction('R')

  object Instruction:
    def fromChar(c: Char): Instruction =
      c match
        case 'U' => U
        case 'D' => D
        case 'L' => L
        case 'R' => R
        case _ => sys.error(s"Invalid instruction character: $c")

  import Instruction.*

  //  Keypad layout as coordinates
  // (3,1)= 1   (3,2)= 2    (3,3)= 3
  // (2,1)= 4   (2,2)= 5    (2,3)= 6
  // (1,1)= 7   (1,2)= 8    (1,3)= 9
  case class OldKeyPad(x: Int, y:Int, state: String = ""):

    // Return the KeyPad number based on the coordinates
    def press: OldKeyPad =
      copy(state = state + ((9 - (y * 3)) + x).toString)

    def move(i: Instruction): OldKeyPad =
      i match
        case U => copy(y = (y + 1).min(3))
        case D => copy(y = (y - 1).max(1))
        case L => copy(x = (x - 1).max(1))
        case R => copy(x = (x + 1).min(3))

    def instructAndPress(i: Vector[Instruction]): OldKeyPad =
      i.foldLeft(this)(_ move _ ).press

  case class NewKeyPad(c: Char, state: String = ""):

    def press: NewKeyPad =
      copy(state = state + c.toString)

    def move(i: Instruction): NewKeyPad =
      i match
        case U if (c == 'D') => copy(c = 'B')
        case U if (c == 'A') => copy(c = '6')
        case U if (c == 'B') => copy(c = '7')
        case U if (c == 'C') => copy(c = '8')
        case U if (c == '6') => copy(c = '2')
        case U if (c == '7') => copy(c = '3')
        case U if (c == '8') => copy(c = '4')
        case U if (c == '3') => copy(c = '1')
        case D if (c == '1') => copy(c = '3')
        case D if (c == '2') => copy(c = '6')
        case D if (c == '3') => copy(c = '7')
        case D if (c == '4') => copy(c = '8')
        case D if (c == '6') => copy(c = 'A')
        case D if (c == '7') => copy(c = 'B')
        case D if (c == '8') => copy(c = 'C')
        case D if (c == 'B') => copy(c = 'D')
        case L if (c == '9') => copy(c = '8')
        case L if (c == '4') => copy(c = '3')
        case L if (c == '8') => copy(c = '7')
        case L if (c == 'C') => copy(c = 'B')
        case L if (c == '3') => copy(c = '2')
        case L if (c == '7') => copy(c = '6')
        case L if (c == 'B') => copy(c = 'A')
        case L if (c == '6') => copy(c = '5')
        case R if (c == '5') => copy(c = '6')
        case R if (c == '2') => copy(c = '3')
        case R if (c == '6') => copy(c = '7')
        case R if (c == 'A') => copy(c = 'B')
        case R if (c == '3') => copy(c = '4')
        case R if (c == '7') => copy(c = '8')
        case R if (c == 'B') => copy(c = 'C')
        case R if (c == '8') => copy(c = '9')
        case _               => copy()

    def instructAndPress(i: Vector[Instruction]): NewKeyPad =
      i.foldLeft(this)(_ move _).press

//  --- Part One ---

  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init

  val input1 =
    Source
      .fromResource("inputAoc2016Day02.txt")
      .getLines
      .map(_.map(fromChar).toVector).toList

  val answer1 = input1
                  .foldLeft(OldKeyPad(2,2))(_ instructAndPress _).state

  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

//  --- Part Two ---
  val start2 = System.currentTimeMillis
  val answer2 = input1
                  .foldLeft(NewKeyPad('5'))(_ instructAndPress _).state

  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
