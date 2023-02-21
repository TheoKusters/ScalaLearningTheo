import scala.annotation.tailrec
import scala.io.*

object Aoc2015Day08 extends App :

  case class Coding(inputString: String):

    def decode: String =
      def go (oldString: List[Char], newString: List[Char] = List.empty): List[Char] =
        oldString match
          case '\\' :: '\\'               :: rest => go (rest, newString :+ '\\')
          case '\\' :: '\"'               :: rest => go (rest, newString :+ '\"')
          case '\\' :: 'x'  :: msb :: lsb :: rest => go (rest, newString :+ Integer.parseInt(msb.toString + lsb.toString, 16).toChar)
          case head :: tail                       => go (tail, newString :+ head)
          case _                                  => newString
      go(inputString.drop(1).dropRight(1).toList, List.empty).mkString("")

    def decodeDiff: Int =
      inputString.length - decode.length

    def encode: String =
      def go(oldString: List[Char], newString: List[Char] = List.empty): List[Char] =
        oldString match
          case '\"' :: rest => go(rest, newString :+ '\\' :+ '\"')
          case '\\' :: rest => go(rest, newString :+ '\\' :+ '\\')
          case head :: tail => go(tail, newString :+ head)
          case _ => newString
      (go(inputString.toList, List('\"')) :+ '\"').mkString("")

    def encodeDiff: Int =
      encode.length - inputString.length

  val start1 = System.currentTimeMillis

  val input: List[Coding] =
    Source
      .fromResource("inputAoc2015Day08.txt")
      .getLines()
      .map(Coding.apply)
      .toList

//  println(s"${input.map(_.diff)}")
//  println(s"${input.map(str => str.decode)}")
//  println(input.map(str => str.encoded.length - str.decode.length ).sum)

  val answer1 = input.map(_.decodeDiff).sum
  println(s"Answer day 8, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")
  assert(answer1 == 1342)

  //  --- Part Two ---
  val start2 = System.currentTimeMillis

  println(s"${input.map(str => str.encode)}")
  val answer2 = input.map(_.encodeDiff).sum

//  println(s"${input.map(str => str.encode)}")
  println(s"Answer day 8, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
  assert(answer2 == 2074)
