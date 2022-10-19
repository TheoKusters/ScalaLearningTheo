// Day 3: Perfectly Spherical Houses in a Vacuum ---
// Santa is delivering presents to an infinite two-dimensional grid of houses.
//
// He begins by delivering a present to the house at his starting location,
// and then an elf at the North Pole calls him via radio and tells him where to move next.
// Moves are always exactly one house to the north (^), south (v), east (>), or west (<).
// After each move, he delivers another present to the house at his new location.
//
// However, the elf back at the north pole has had a little too much eggnog,
// and so his directions are a little off, and Santa ends up visiting some houses more than once.
// How many houses receive at least one present?
//
// For example:
//
//  > delivers presents to 2 houses: one at the starting location, and one to the east.
//  ^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
//  ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
//
// Your puzzle answer was 2572.

import scala.io.*

object Day03 extends App {

  val input =
    Source
      .fromFile("aoc/src/resources/inputDay03.txt")
      .toList

  val start1 = System.currentTimeMillis
  val UP = '^'
  val DOWN = 'v'
  val RIGHT = '>'
  val LEFT = '<'

  def addStopToRoute(l: List[(Int, Int)], direction: Char): List[(Int, Int)] =
    if (direction == UP) l.appended((l.last._1, l.last._2 + 1))
    else if (direction == DOWN) l.appended((l.last._1, l.last._2 - 1))
    else if (direction == RIGHT) l.appended((l.last._1 + 1, l.last._2))
    else if (direction == LEFT) l.appended((l.last._1 - 1, l.last._2))
    else l

  println(s"Answer day 3, part 1: ${input.foldLeft(List((0,0)))(addStopToRoute).distinct.size} [${System.currentTimeMillis - start1 }ms]")

//    --- Part Two ---
//  The next year, to speed up the process, Santa creates a robot version of himself,
//  Robo-Santa, to deliver presents with him.
//
//  Santa and Robo-Santa start at the same location (delivering two presents to the same starting house),
//  then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.
//
//  This year, how many houses receive at least one present?
//
//  For example:
//
//    ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
//    ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
//    ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.
//
// Your puzzle answer was 2631.

  val start2 = System.currentTimeMillis
  
  val santaInput = input
                    .zipWithIndex
                    .collect { case (x, i) if (i % 2 == 0) => x }
  val roboInput = input
                    .zipWithIndex
                    .collect {case (x, i) if !(i % 2 == 0) => x}
  
  val santaRoute = santaInput.foldLeft(List((0,0)))(addStopToRoute)
  val roboRoute = roboInput.foldLeft(List((0,0)))(addStopToRoute)

  println(s"Answer day 3, part 2: ${santaRoute.appendedAll(roboRoute).distinct.size} [${System.currentTimeMillis - start2 }ms]")
}
