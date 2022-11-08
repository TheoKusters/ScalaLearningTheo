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

import scala.annotation.tailrec
import scala.io.*

object Day06 extends App {

  val input =
    Source
      .fromResource("./inputDay06.txt")
      .getLines()
      .toList

//  val input = List("turn on 0,0 through 1,1","turn off 4,4 through 4,4","toggle 0,0 through 3,3")

  val start1 = System.currentTimeMillis

  case class Instruction(action: String, fromX: Int, fromY: Int, toX: Int, toY: Int)
//  case class Coordinate(x: Int, y: Int)

  def parse(line:String) : Instruction =
    line match
      case s"turn on $startX,$startY through $endX,$endY" =>
        Instruction("turn on", startX.toInt, startY.toInt, endX.toInt, endY.toInt)
      case s"turn off $startX,$startY through $endX,$endY" =>
        Instruction("turn off", startX.toInt, startY.toInt, endX.toInt, endY.toInt)
      case s"toggle $startX,$startY through $endX,$endY" =>
        Instruction("toggle", startX.toInt, startY.toInt, endX.toInt, endY.toInt)
      case _ => sys.error("Parsing error")

  def dim(x: Int, y: Int, currentLux: Int, i: Instruction, mode: String): Int =
    if (x >= i.fromX && y >= i.fromY && x <= i.toX && y <= i.toY)
      mode match
        case "switch" =>
          i.action match
            case "toggle" => if (currentLux == 0) then 1 else 0
            case "turn on" => 1
            case "turn off" => 0
        case "dim" =>
           i.action match
             case "toggle" => currentLux + 2
             case "turn on" => currentLux + 1
             case "turn off" => if (currentLux <= 0) then 0 else currentLux - 1
    else currentLux

  def processOneBulb(x: Int, y: Int, mode: String): Int =
    input
      .foldLeft(0)((currentLux,instruction)=>dim(x,y,currentLux,parse(instruction),mode))

  val lightsOn = List
    .tabulate(1000,1000)((x,y)=>processOneBulb(x, y, "switch"))
    .flatten
    .sum

  println(s"Answer day 6, part 1: $lightsOn [${System.currentTimeMillis - start1 }ms]")

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

  val totalLux = List
    .tabulate(1000, 1000)((x, y) => processOneBulb(x, y, "dim"))
    .flatten
    .sum

  println(s"Answer day 6, part 2: $totalLux [${System.currentTimeMillis - start2 }ms]")
}
