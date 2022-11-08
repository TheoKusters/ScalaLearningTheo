// --- Day 5: Doesn't He Have Intern-Elves For This? ---
// Santa needs help figuring out which strings in his text file are naughty or nice.
//
// A nice string is one with all of the following properties:
//
// It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
// It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
// It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
// For example:
//
// ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...),
//   and none of the disallowed substrings.
// aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
// jchzalrnumimnmhp is naughty because it has no double letter.
// haegwjzuvuyypxyu is naughty because it contains the string xy.
// dvszwmarrgswjxmb is naughty because it contains only one vowel.
// How many strings are nice?
//
// Your puzzle answer was 258.

import scala.annotation.tailrec
import scala.io.*

object Day05 extends App {

  val input =
    Source
      .fromResource("./inputDay05.txt")
      .getLines()
      .toList

//  val input = List("ugknbfddgicrmopn","aaa","jchzalrnumimnmhp","haegwjzuvuyypxyu","dvszwmarrgswjxmb")

  val start1 = System.currentTimeMillis

  def testVowels(testString: String): Boolean =
    val vowels = List('a', 'e', 'i', 'o', 'u')
    testString.count(c => vowels.contains(c)) > 2

  def testTwins(testString: String): Boolean =
    def go(l: List[Char]): Boolean =
      (l.head, l.tail) match
        case(head, Nil) => false
        case(head, tail) =>
          if (l.head == l.tail.head)  true
          else go(l.tail)
    go(testString.toList)

  def testValid(testString: String): Boolean =
    val invalidPairs = List("ab", "cd", "pq", "xy")
    def go(l: List[Char]): Boolean =
      (l.head, l.tail) match
        case (head, Nil) => true
        case (head, tail) =>
          if (invalidPairs.contains(l.head.toString.concat(l.tail.head.toString))) false
          else go(l.tail)
    go(testString.toList)

  def countNiceStrings(l: List[String]): Int =
    input.foldLeft(0)((n, str) =>
      if (testVowels(str) && testTwins(str) && testValid(str)) n + 1 else n)

  println(s"Answer day 5, part 1: ${countNiceStrings(input)} [${System.currentTimeMillis - start1 }ms]")

// --- Part Two ---
// Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.
// 
// Now, a nice string is one with all of the following properties:
// 
// It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
// It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
// For example:
// 
// qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
// xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
// uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
// ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.
// How many strings are nice under these new rules?
//
// Your puzzle answer was 53.

  val start2 = System.currentTimeMillis

  val repeatNonConsecutive = raw"([a-z])[\w]\1".r
  val repeatingPairs = raw"(\w\w)\w*\1".r

  def countNiceStringsWithRegex(l: List[String]): Int =
    input.foldLeft(0)((n, str) => repeatNonConsecutive.findFirstIn(str) match
      case Some(_) => repeatingPairs.findFirstIn(str) match
        case Some(_) => n + 1
        case None => n
      case None => n)

  println(s"Answer day 5, part 2: ${countNiceStringsWithRegex(input)} [${System.currentTimeMillis - start2 }ms]")
}
