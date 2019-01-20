import scalaz.Scalaz._

object Alphametics {
  private final case class Riddle(addends: List[String], sum: String) {
    val letters: Set[Char] = (sum :: addends).mkString("").toSet

    def check(solution: Map[Char, Char]): Boolean =
      (addends.traverse(substitute(solution, _)), substitute(solution, sum)) match {
        case (Some(ts), Some(s)) => ts.sum == s
        case _ => false
      }

    private def substitute(solution: Map[Char, Char], addend: String): Option[Long] = {
      val addend0 = addend.map(solution)
      if (addend0 startsWith "0") None else Some(addend0.toLong)
    }
  }

  def solve(input: String): Option[Map[Char, Int]] = parse(input) flatMap solve

  private val Equality = """([A-Z+]+)==([A-Z]+)""".r
  private def parse(input: String): Option[Riddle] =
    input.filterNot(_.isWhitespace) match {
      case Equality(addends, sum) => Some(Riddle(addends.split('+').toList, sum))
      case _ => None
    }

  private val digits = Set('0' to '9': _*).toStream
  private def solve(riddle: Riddle): Option[Map[Char, Int]] =
    riddle.letters.toStream.foldRightM(Map.empty[Char, Char]) {
      (letter, solution) => for {
        digit <- digits filterNot solution.values.toList.contains
      } yield solution + (letter -> digit)
    } collectFirst {
      case solution if riddle.check(solution) => solution mapValues (_ - '0')
    }
}