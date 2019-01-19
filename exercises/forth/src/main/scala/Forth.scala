import scala.util.Try
import scalaz.Scalaz._
import scalaz._

class Forth {
  type Result[A] = Either[Error, A]

  sealed trait Error extends Product with Serializable
  object Error {
    type T = Error
    case object DivisionByZero extends T
    case object StackUnderflow extends T
    case object InvalidWord extends T
    case object UnknownWord extends T
    case object InvalidSyntax extends T
  }

  type EvaluationResult = Result[State]

  type Stack[A] = List[A]

  final case class State(stack: Stack[Int], scope: Map[String, Program]) {
    // Ugly, but needed for the given test suite
    override def toString: String = stack.reverse.mkString(" ")
  }
  private val initialState = State(Nil, Map.empty[String, Program])

  type Program = List[Command]

  sealed trait Command extends Product with Serializable
  object Command {
    type T = Command
    final case class PushConstant(constant: Int) extends T
    final case class EvaluateWord(word: String) extends T
    final case class DefineWord(word: String, definition: Program) extends T
  }

  sealed trait Token extends Product with Serializable
  object Token {
    type T = Token
    case object DefinitionBegin extends T
    case object DefinitionEnd extends T
    final case class Constant(constant: Int) extends T
    final case class Word(word: String) extends T

    private object Number {
      def unapply(s: String): Option[Int] = Try(s.toInt).toOption
    }

    private object Atom {
      def unapply(s: String): Option[String] = Some(s.toLowerCase)
    }

    def fromString: PartialFunction[String, T] = {
      case ":" => DefinitionBegin
      case ";" => DefinitionEnd
      case Number(n) => Constant(n)
      case Atom(w) => Word(w)
    }
  }

  def eval(input: String): EvaluationResult = {
    val lexems = input.split("\\s+").toList
    for {
      tokens  <- lexems.traverse(parseToken)
      program <- parse(tokens)
      result  <- eval(program)
    } yield result
  }

  private def parseToken(lexem: String): Result[Token] = Token.fromString.lift(lexem) match {
    case Some(t) => Right(t)
    case None => Left(Error.InvalidWord)
  }

  private def parse(tokens: List[Token],
                    definitions: List[(String, Program)] = Nil,
                    synonyms: Map[String, String] = Map.empty[String, String]): Result[Program] =
    tokens match {
      case Token.Word(word) :: ts =>
        val command = Command.EvaluateWord(synonyms.getOrElse(word, word))
        definitions match {
          case (name, body) :: definitions0 =>
            val definitions1 = (name, command :: body) :: definitions0
            parse(ts, definitions1, synonyms)

          case _ => for {
            program <- parse(ts, definitions, synonyms)
          } yield command :: program
        }

      case Token.Constant(constant) :: ts =>
        val command = Command.PushConstant(constant)
        definitions match {
          case (word, body) :: definitions0 =>
            val definitions1 = (word, command :: body) :: definitions0
            parse(ts, definitions1, synonyms)

          case _ => for {
            program <- parse(ts, definitions, synonyms)
          } yield command :: program
        }

      case Token.DefinitionBegin :: Token.Word(word) :: ts =>
        val definitions0 = (word -> Nil) :: definitions
        parse(ts, definitions0, synonyms)

      case Token.DefinitionBegin :: _ => Left(Error.InvalidSyntax)

      case Token.DefinitionEnd :: ts => definitions match {
        case (word, body) :: definitions0 =>
          val word0 = disambiguate(word, synonyms.values.toSet)
          val command = Command.DefineWord(word0, body.reverse)
          val synonyms0 = synonyms + (word -> word0)
          for {
            program <- parse(ts, definitions0, synonyms0)
          } yield command :: program

        case _ => Left(Error.InvalidSyntax)
      }

      case Nil => definitions match {
        case Nil => Right(Nil)
        case _ => Left(Error.InvalidSyntax)
      }
    }

  private def disambiguate(word: String, words: Set[String]): String =
    if (words contains word) disambiguate(word + "'", words) else word

  sealed trait ArithmeticOperator extends Product with Serializable
  object ArithmeticOperator {
    type T = ArithmeticOperator
    case object Add extends T
    case object Subtract extends T
    case object Multiply extends T
    case object Divide extends T

    def fromString: PartialFunction[String, T] = {
      case "+" => Add
      case "-" => Subtract
      case "*" => Multiply
      case "/" => Divide
    }
  }

  object ArithmeticOperation {
    def unapply(word: String): Option[ArithmeticOperator.T] =
      ArithmeticOperator.fromString.lift(word)
  }

  sealed trait StackOperator extends Product with Serializable
  object StackOperator {
    type T = StackOperator
    case object Dup  extends T
    case object Drop extends T
    case object Swap extends T
    case object Over extends T

    def fromString: PartialFunction[String, T] = {
      case "dup"  => Dup
      case "drop" => Drop
      case "swap" => Swap
      case "over" => Over
    }
  }

  object StackOperation {
    def unapply(word: String): Option[StackOperator.T] =
      StackOperator.fromString.lift(word.toLowerCase)
  }

  private def eval(program: Program, state: State = initialState): EvaluationResult =
    program.foldLeftM(state)(eval)

  private def eval(state: State, command: Command): Result[State] = command match {
    case Command.PushConstant(constant) =>
      Right(state.copy(stack = constant :: state.stack))

    case Command.EvaluateWord(word) => word match {
      case _ if state.scope contains word =>
        eval(state.scope(word), state)

      case ArithmeticOperation(op) => state.stack match {
        case a :: b :: stack => for {
          c <- eval(op, a, b)
        } yield state.copy(stack = c :: stack)

        case _ => Left(Error.StackUnderflow)
      }

      case StackOperation(op) => for {
        stack <- eval(op, state.stack)
      } yield state.copy(stack = stack)

      case _ => Left(Error.UnknownWord)
    }

    case Command.DefineWord(word, definition) =>
      Right(state.copy(scope = state.scope + (word -> definition)))
  }

  private def eval(op: ArithmeticOperator, a: Int, b: Int): Result[Int] = {
    import ArithmeticOperator._
    if (op == Divide && a == 0)
      Left(Error.DivisionByZero)
    else {
      val f: (Int, Int) => Int = op match {
        case Add      => _ + _
        case Subtract => _ - _
        case Multiply => _ * _
        case Divide   => _ / _
      }
      Right(f(b, a))
    }
  }

  private def eval(op: StackOperator, stack: Stack[Int]): Result[Stack[Int]] = {
    import StackOperator._
    op match {
      case Dup => stack match {
        case c :: stack0 => Right(c :: c :: stack0)
        case _ => Left(Error.StackUnderflow)
      }

      case Drop => stack match {
        case _ :: stack0 => Right(stack0)
        case _ => Left(Error.StackUnderflow)
      }

      case Swap => stack match {
        case a :: b :: stack0 => Right(b :: a :: stack0)
        case _ => Left(Error.StackUnderflow)
      }

      case Over => stack match {
        case a :: b :: stack0 => Right(b :: a :: b :: stack0)
        case _ => Left(Error.StackUnderflow)
      }
    }
  }
}