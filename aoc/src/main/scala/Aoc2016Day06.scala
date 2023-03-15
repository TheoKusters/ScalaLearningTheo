import scala.annotation.tailrec
import scala.collection.MapView
import scala.io.*

object Aoc2016Day06 extends App :

//  --- Part One ---
  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init

  val signals =
    Source
      .fromResource("inputAoc2016Day06.txt")
      .getLines
      .toList

  def answer(part: MapView[Char, Int] => Char): String =
    signals
      .map(_.toList)
      .transpose
      .map(ll => part(ll.groupBy(identity).view.mapValues(_.size)))
      .mkString

  val answer1 = answer(_.maxBy(_._2)._1)

  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

  //  --- Part Two ---

  val start2 = System.currentTimeMillis

  val answer2 = answer(_.minBy(_._2)._1)

  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
