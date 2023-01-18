import scala.annotation.tailrec
import scala.io.*

object Day10 extends App :

  // Conway's constant ~ Look and Say: https://www.youtube.com/watch?v=ea7lJkEhytA

  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(3).init

  val input = "1113122113".toVector
  val iterations1 = 40
  val iterations2 = 50

  def lookAndSay (start: Vector[Char]) : Vector[Char] =
    def go (currentWord: Vector[Char], nextWord: Vector[Char], currentChar: Char, acc: Int) : Vector[Char] =
      currentWord match
        case h +: t if h == currentChar => go (t, nextWord, currentChar, acc + 1)
        case h +: t if h != currentChar
          => go (t, nextWord :++ acc.toString.toVector :+ currentChar, h, 1)
        case _ => nextWord :++ acc.toString.toVector :+ currentChar
    go (start, Vector.empty , start.head, 0)

  def loop(word:Vector[Char], acc: Int, iteration: Int): Vector[Char] =
    if (acc == iteration) word
    else loop(lookAndSay(word), acc+1, iteration)

  val answer1 = loop(input,0,iterations1).mkString("").length
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")
  assert(answer1 == 360154)

  //  --- Part Two ---
  val start2 = System.currentTimeMillis
  val answer2 = loop(input,0,iterations2).mkString("").length

  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
  assert(answer2 == 5103798)
