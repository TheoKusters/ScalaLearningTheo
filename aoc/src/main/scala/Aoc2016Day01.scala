import scala.annotation.tailrec
import scala.io.*

object Aoc2016Day01 extends App :

  case class Coord(x: Int, y: Int)

  enum ValidTurn(c: Char):
    case R extends ValidTurn('R')
    case L extends ValidTurn('L')

  object ValidTurn:
    def fromChar(c: Char): ValidTurn =
      c match
        case 'R'  => R
        case 'L'  => L
        case  _   => sys.error(s"Invalid turn character: $c")
  import ValidTurn.*

  case class Cmd(turn: ValidTurn, blocks: Int)
  object Cmd:
    def fromString(s:String): Cmd = Cmd(fromChar(s.trim.head), s.trim.tail.toInt)
  import Cmd.*

  enum Heading:
    case N
    case E
    case S
    case W
  import Heading.*

  case class HereWeAre(heading: Heading = N, route: Vector[Coord]):

    def taxiDistance = route.last.x.abs + route.last.y.abs

    def firstCrossingDistance =
      val crossing = firstCrossing.getOrElse(Coord(0,0))
      crossing.x.abs + crossing.y.abs

    private def firstCrossing =
      def loop(c: Vector[Coord]): Option[Coord] =
        c match
          case head +: tail if tail.contains(head)  => Some(head)
          case head +: tail                         => loop(tail)
          case _                                    => None
      loop(route)

    def startDriving(cmd: Cmd) : HereWeAre =
      def go(step: Int, route: Vector[Coord]): Vector[Coord] =
        if (step >= cmd.blocks) route
        else
          heading match
            case N => go(step + 1, route :+ Coord(route.last.x,  route.last.y + 1))
            case W => go(step + 1, route :+ Coord(route.last.x - 1,  route.last.y))
            case S => go(step + 1, route :+ Coord(route.last.x,  route.last.y - 1))
            case E => go(step + 1, route :+ Coord(route.last.x + 1,  route.last.y))
            case _ => sys.error("Invalid driving directions!")
      copy(route = go(0, route))

    def turn(cmd: Cmd) : HereWeAre =
      (heading, cmd.turn ) match
        case (N, L) | (S, R) => copy(heading = W)
        case (W, L) | (E, R) => copy(heading = S)
        case (S, L) | (N, R) => copy(heading = E)
        case (E, L) | (W, R) => copy(heading = N)
        case _ => sys.error("Invalid turn!")

  object HereWeAre:
    def airdrop: HereWeAre =
      HereWeAre(N,Vector(Coord(0,0)))

  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init

  val input1 =
    Source
      .fromResource("inputAoc2016Day01.txt")
      .mkString
  val commands = input1.split(',').map(fromString).toVector
  val drivingRoute = commands.foldLeft(HereWeAre.airdrop)((a, b) => a.turn(b).startDriving(b) )
  val answer1 = drivingRoute.taxiDistance
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

//  --- Part Two ---
  val start2 = System.currentTimeMillis
  val answer2 = drivingRoute.firstCrossingDistance
  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
