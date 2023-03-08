import scala.annotation.tailrec
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

  val answer1 = signals
                  .map(_.toList)
                  .transpose
                  .map(_.groupBy(identity)
                  .view.mapValues(_.size)
                  .maxBy(_._2)._1)
                  .mkString

  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

  //  --- Part Two ---

  val start2 = System.currentTimeMillis

  val answer2 = signals
                  .map(_.toList)
                  .transpose
                  .map(_.groupBy(identity)
                  .view.mapValues(_.size)
                  .minBy(_._2)._1)
                  .mkString

  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
