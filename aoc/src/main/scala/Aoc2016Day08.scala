import scala.annotation.tailrec
import scala.io.*

object Aoc2016Day08 extends App :

//  case class Screen(s: Vector[Vector[Boolean]])

//  object Screen:


//  case class Inst(name: String, n1: Int, n2: Int)

//  def parseString(s: String): Inst =
//    s match
//      case s"rect ${n1}x${n2}"                => Inst("rect", n1.toInt, n2.toInt)
//      case s"rotate column x=${n1} by ${n2}"  => Inst("rotateCol", n1.toInt, n2.toInt)
//      case s"rotate row y=${n1} by ${n2}"     => Inst("rotateRow", n1.toInt, n2.toInt)
//      case _                                  => sys.error("Parsing error")

//  --- Part One ---
  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init

//  val input =
//    Source
//      .fromResource("inputAoc2016Day08.txt")
//      .getLines
//      .map(parseString)

//  println(input.mkString(","))

  val answer1 = "answer1"
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

  //  --- Part Two ---

  val start2 = System.currentTimeMillis

  val answer2 = "answer2"
  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
