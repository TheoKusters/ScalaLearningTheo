// --- Day 1: Not Quite Lisp ---
//
// Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - the directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
//
// An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.
//
// The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.
//
// For example:
//
// (()) and ()() both result in floor 0.
// ((( and (()(()( both result in floor 3.
// ))((((( also results in floor 3.
// ()) and ))( both result in floor -1 (the first basement level).
// ))) and )())()) both result in floor -3.
//  To what floor do the instructions take Santa?
//
// Your puzzle answer was 138.

import scala.io.*

object Aoc2015Day01 extends App {

  val input =
  Source
    .fromResource("inputAoc2015Day01.txt")
    .toList

  val start1 = System.currentTimeMillis
  val up = input.count(floorUp => floorUp == '(')
  val down = input.count(floorDown => floorDown == ')')

  def processDirection(floor: Int, direction: Char): Int =
    if (direction == '(') floor + 1
    else if (direction == ')') floor - 1
    else floor

  val floors = input
                .foldLeft(0)((floor, direction) => processDirection(floor, direction))

  println(s"Answer day 1, part 1: ${up - down}  [${System.currentTimeMillis - start1}ms]")
  println(s"Answer day 1, part 1: ${floors}  [${System.currentTimeMillis - start1}ms]")

//  --- Part Two ---
//  Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1).
//  The first character in the instructions has position 1, the second character has position 2, and so on.
//
//  For example:
//
//  ) causes him to enter the basement at character position 1.
//  ()()) causes him to enter the basement at character position 5.
//  What is the position of the character that causes Santa to first enter the basement?
//
//  Your puzzle answer was 1771.

  val start2 = System.currentTimeMillis

  def level(l: List[Char], pos: Int, floor: Int) : Int =
    if (floor == -1) pos
    else if (l.head == '(') level(l.tail, pos + 1, floor + 1)
    else if (l.head == ')') level(l.tail, pos + 1, floor - 1)
    else level(l.tail, pos + 1, floor)

  println(s"Answer day 1, part 2: ${level(input, 0, 0)}  [${System.currentTimeMillis - start2 }ms]")
}
