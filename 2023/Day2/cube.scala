import scala.io.Source
import scala.collection.immutable.HashMap

@main def main() =
  val input = Source.fromFile("./input.txt").getLines().toList
  val checks = HashMap("red" -> 12, "green" -> 13, "blue" -> 14)
  val initMap = HashMap("red" -> 0, "green" -> 0, "blue" -> 0)
  val parsedGames = input.map { line =>
    val data = line.split(":")
    val gameIndex = data(0) match
      case s"Game $game" => Integer.parseInt(game)
    data(1).split(";").map { round =>
      round.split(",").foldLeft(initMap) { (acc, elem) =>
        val x = elem.trim().split(" ")
        acc.updated(x.last, Integer.parseInt(x.head))
      }
    }.fold(initMap) { (x, y) => x.merged(y) { (a, b) => (a._1, a._2.max(b._2)) }}
    .updated("index", gameIndex)
  }

  def part1(games: List[HashMap[String, Int]]) =
    games
      .filter { game => checks("red") >= game("red") && checks ("green") >= game("green") && checks ("blue") >= game("blue")
      }
      .map(_.apply("index"))
      .sum

  def part2(games: List[HashMap[String, Int]]) =
    games
      .map {game => game("red") * game("green") * game("blue") }
      .sum

  println(part1(parsedGames))
  println(part2(parsedGames))
