//  --- Day 6: Probably a Fire Hazard ---
//  Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.
//
//  Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.
//
//  Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0.
//  The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs.
//  Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2
//  therefore refers to 9 lights in a 3x3 square. The lights all start turned off.
//
//  To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.
//
//  For example:
//
//  turn on 0,0 through 999,999 would turn on (or leave on) every light.
//  toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
//  turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
//  After following the instructions, how many lights are lit?
//
//  Your puzzle answer was 569999.

import Aoc2015Day06.Inst

import scala.annotation.tailrec
import scala.io.*

object Aoc2015Day06 extends App:

  // -- Modeling

  sealed abstract class Inst[A](name: String, x0: Int, y0: Int, x1: Int, y1: Int):
    def off(cur: A): A
    def on(cur: A): A
    def toggle(cur: A): A

    def run(state: A): A =
      name match
        case "off"    => off(state)
        case "on"     => on(state)
        case "toggle" => toggle(state)

    def affects(x: Int, y: Int): Boolean =
      x >= x0 && y >= y0 && x <= x1 && y <= y1

  object Inst:

    def parseFrom[A](make: (String, Int, Int, Int, Int) => Inst[A])(commands: List[String]): List[Inst[A]] =
      commands.map(_ match
        case s"turn $name $x0,$y0 through $x1,$y1" => make(name, x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case s"$name $x0,$y0 through $x1,$y1"      => make(name, x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case _                                     => sys.error("Parsing error")
      )

  type Mat[A] = List[List[A]]

  object Mat:

    def tabulate[A](sizeX: Int, sizeY: Int)(start: A)(instructions: List[Inst[A]]): Mat[A] =
      def process(x: Int, y: Int): A = instructions.foldLeft(start)((a,i) => if i.affects(x,y) then i.run(a) else a)
      List.tabulate(sizeX,sizeY)(process)



  //  -- Input --

  val commands: List[String] =
    Source
      .fromResource("inputAoc2015Day06.txt")
      .getLines()
      .toList

  //  --- Part One ---

  val start1 = System.currentTimeMillis

  def booleanInstMaker(name: String, x0: Int, y0: Int, x1: Int, y1: Int): Inst[Boolean] =
    new Inst[Boolean](name: String, x0: Int, y0: Int, x1: Int, y1: Int):
      def off(cur: Boolean)    = false
      def on(cur: Boolean)     = true
      def toggle(cur: Boolean) = !cur

  val lightsOn: Int =
    val parser = Inst.parseFrom(booleanInstMaker)
    Mat
      .tabulate(1000,1000)(false)(parser(commands))
      .flatten
      .count(_ == true)

  println(s"Answer day 6, part 1: $lightsOn [${System.currentTimeMillis - start1 }ms]")

  assert(lightsOn == 569999)

//  --- Part Two ---
//  You just finish implementing your winning light pattern when you realize you mistranslated Santa 's message from Ancient Nordic Elvish
//
//  The light grid you bought actually has individual brightness controls;
//  each light can have a brightness of zero or more.
//  The lights all start at zero.
//
//  The phrase turn on actually means that you should increase the brightness of those lights by 1.
//  The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.
//  The phrase toggle actually means that you should increase the brightness of those lights by 2.
//
//  What is the total brightness of all lights combined after following Santa 's instructions ?
//
//  Your puzzle answer was 17836115.

  val start2 = System.currentTimeMillis

  def intInstMaker(name: String, x0: Int, y0: Int, x1: Int, y1: Int): Inst[Int] =
    new Inst[Int](name: String, x0: Int, y0: Int, x1: Int, y1: Int):
      def off(cur: Int)    = if cur <= 0 then 0 else cur - 1
      def on(cur: Int)     = cur + 1
      def toggle(cur: Int) = cur + 2

  val totalLux: Int =
    val parser = Inst.parseFrom(intInstMaker)
    Mat
      .tabulate(1000,1000)(0)(parser(commands))
      .flatten
      .sum

  println(s"Answer day 6, part 2: $totalLux [${System.currentTimeMillis - start2 }ms]")

  assert(totalLux == 17836115)


