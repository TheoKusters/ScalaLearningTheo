import scala.annotation.tailrec
import scala.io.*

object Aoc2016Day03 extends App :

  case class Sides(x: Int, y: Int, z: Int):

    def isTriangle: Boolean =
      ((x + y) > z) & ((x + z) > y) & ((y + z) > x)

  object Sides:

    def fromString(s: String): Sides =
      val a = s.trim.split("\\s+")
      Sides(a(0).toInt, a(1).toInt, a(2).toInt)

//  --- Part One ---
  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init

  val design: Vector[Sides] =
    Source
      .fromResource("inputAoc2016Day03.txt")
      .getLines
      .map(Sides.fromString).toVector

  val answer1 = design.count(side => side.isTriangle)
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

  //  --- Part Two ---

  val start2 = System.currentTimeMillis

  type Line = List[Int]

  object Line:

    def fromString(s: String): Line =
      val is = s.trim.split("\\s+")
      List(is(0).toInt, is(1).toInt, is(2).toInt)

  extension (line: Line) def toSides: Sides =
    Sides(x = line(0), y = line(1), z = line(2))

  val lines  =
    Source
      .fromResource("inputAoc2016Day03.txt")
      .getLines
      .map(Line.fromString).toList

  val newDesign = lines.grouped(3).flatMap(_.transpose).map(_.toSides)

  val answer2 = newDesign.count(side => side.isTriangle)
  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
