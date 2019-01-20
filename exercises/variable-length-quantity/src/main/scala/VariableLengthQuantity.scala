object VariableLengthQuantity {
  private val sevenBitsMask = 0x7f
  private val eightBitMask = 0x80

  def encode(nums: List[Int]): List[Int] = nums flatMap encode

  private def encode(n: Int): List[Int] = n match {
    case 0 => List(0)
    case _ =>
      Iterator.iterate(n)(_ >>> 7).takeWhile(_ != 0).map(_ & sevenBitsMask).toList match {
        case c :: cs => (c :: (cs map (_ | eightBitMask))).reverse
        case Nil => Nil
      }
  }

  def decode(seq: List[Int]): Either[String, List[Int]] = for {
    seqs <- splitSequences(seq)
  } yield for {
    seq <- seqs
  } yield decodeSequence(seq)

  private def splitSequences(seq: List[Int], acc: List[Int] = Nil): Either[String, List[List[Int]]] =
    seq match {
      case Nil => Right(Nil)
      case i :: Nil if (i & eightBitMask) != 0 => Left("Malformed sequence")
      case i :: is  if (i & eightBitMask) == 0 => for {
        ss <- splitSequences(is)
      } yield (i :: acc).reverse :: ss
      case i :: is => splitSequences(is, i :: acc)
    }

  private def decodeSequence(seq: List[Int]): Int =
    seq.foldLeft(0) { (a, x) => (a << 7) | (x & sevenBitsMask) }
}
