import scala.annotation.tailrec
import scala.io.*

object Aoc2015Day09 extends App :

  case class Distance(start: String, end: String, km: Int):
    def invert:
      Distance = Distance(end, start, km)

  object Distance:
    def fromString(inputLine: String): Distance =
      inputLine match
        case s"$start to $end = $km" => Distance(start, end, km.toInt)

  val start1 = System.currentTimeMillis

  val input : List[Distance] =
    Source
      .fromResource("./inputDay09.txt")
      .getLines()
      .map(Distance.fromString)
      .toList

  val distances = input :++ input.map(_.invert)
  val cities = distances.flatMap(road => List(road.start, road.end)).distinct
  val routes = cities.permutations
  val routeDistances = routes.map(route
    => (route, route.sliding(2).foldLeft(0){ case (routeDistance, road)
    => routeDistance + distances.filter(r => r.start == road(0) && r.end == road(1)).head.km})).toList.sortBy(_._2)
  val answer1 = routeDistances.head._2

  println(s"Answer day 9, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")
  assert(answer1 == 141)

  //  --- Part Two ---
  val start2 = System.currentTimeMillis

  val answer2 = routeDistances.last._2

  println(s"Answer day 9, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
  assert(answer2 == 736)
