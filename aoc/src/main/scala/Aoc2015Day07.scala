import scala.annotation.tailrec
import scala.io.*

object Aoc2015Day07 extends App :

  // ---------------------------------
  // Modelling
  // ---------------------------------

  type Env = Map[String, Int]

  sealed trait Exp:
    def run (env: Env) : Option[Int]
    def wire : String

  case class And (w1: String, w2: String, wire: String) extends Exp:
    def run(env: Env) : Option[Int] =
      for {
        v1 <- env.get(w1)
        v2 <- env.get(w2)
      } yield v1 & v2

  case class AndRh (n1: Int, w1: String, wire: String) extends Exp :
    def run(env: Env): Option[Int] =
      for {
        v1 <- env.get(w1)
      } yield v1 & n1

  case class Or (w1: String, w2: String, wire: String) extends Exp :
    def run(env: Env): Option[Int] =
      env.get(w1).flatMap(v1 => env.get(w2).map(v2 => v1 | v2))

  case class Rshift (n1: Int, w1: String, wire: String) extends Exp :
    def run(env: Env): Option[Int] =
      env.get(w1).map(v => v >> n1)

  case class Lshift (n1: Int, w1: String, wire: String) extends Exp :
    def run(env: Env): Option[Int] =
      env.get(w1).map(v => v << n1)

  case class Not (w1: String, wire: String) extends Exp :
    def run(env: Env): Option[Int] =
      env.get(w1).map(v => ~v & 0x0000FFFF )

  case class Equals (w1: String, wire: String) extends Exp :
    def run(env: Env): Option[Int] =
      env.get(w1)

  case class Value (n: Int, wire: String) extends Exp :
    def run(env: Env): Option[Int] =
      Some(n)

  def parser(s: String): Exp =
    s match
      case s"$n1 AND $w1 -> $wire" if n1.toIntOption.isDefined => AndRh(n1.toInt, w1, wire)
      case s"$w1 AND $w2 -> $wire" => And(w1, w2, wire)
      case s"$w1 OR $w2 -> $wire" => Or(w1, w2, wire)
      case s"$w1 RSHIFT $n1 -> $wire" => Rshift(n1.toInt, w1, wire)
      case s"$w1 LSHIFT $n1 -> $wire" => Lshift(n1.toInt, w1, wire)
      case s"NOT $w1 -> $wire" => Not(w1, wire)
      case s"$int -> $wire" if int.toIntOption.isDefined => Value(int.toInt, wire)
      case s"$w1 -> $wire" => Equals(w1, wire)

  val start1 = System.currentTimeMillis

  val input =
    Source
      .fromResource("inputAoc2015Day07.txt")
      .getLines()
      .toList

//  val input = List(
//    "y RSHIFT 2 -> x",
//    "956 -> y",
//    "x AND y -> d",
//    "x OR y -> e",
//    "x LSHIFT 2 -> a",
//    "y RSHIFT 2 -> g",
//    "NOT x -> h",
//    "NOT y -> i"
//  )

  val rules: List[Exp] = input.map(parser)
  println(s"Rules= $rules")

  def solve(rules: List[Exp], wire: String, env: Env = Map.empty) : Int =
    env.get(wire) match
      case Some(v) => v
      case None =>
        rules match
          case head :: tail => head.run(env) match
            case Some(v) => solve(tail, wire, env.updated(head.wire, v))
            case None => solve(tail :+ head, wire, env)
          case _ => sys.error("Boom")

  val answer1 = solve(rules, "a")

  println(s"Answer day 7, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")
  assert(answer1 == 956)

  //  --- Part Two ---
  val start2 = System.currentTimeMillis

  val rulesPart2 = Value(answer1, "b") :: rules.filterNot(_.wire == "b")
  val answer2 = solve(rulesPart2, "a")
  println(s"Answer day 7, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
  assert(answer2 == 40149)
