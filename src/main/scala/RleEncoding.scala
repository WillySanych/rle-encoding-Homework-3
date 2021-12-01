import java.rmi.UnmarshalException

object RleEncodingApp extends App {

  val rleEncoding = new RleEncoding

  println(rleEncoding.encode("ABCDE")) // A1B1C1D1E1

  println(rleEncoding.encode("AABBCCDD")) // A2B2C2D2
  println(rleEncoding.encode("XAABBCCDD")) // FIXME: 2XA2AB2BC2CD

  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) // A4B3C2XYZD4E3F3A6B29
  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX")) // FIXME: last symbol is lost
}

trait Block {
  def length: Int
}

case class UncompressedBlock(length: Int, data: Seq[Char]) extends Block
case class CompressedBlock(length: Int, data: Char) extends Block

class RleEncoding {

  def encode(str: String): Seq[Block] = {
    val (prev, optBlock, result) =
      str.toCharArray.foldLeft((None: Option[Char], None: Option[Block], Seq.empty[Block])) {
        case ((None, _, result), char) =>
          (Some(char), None, result)

        case ((Some(prev), None, result), char) if prev == char =>
            (Some(char), Some(CompressedBlock(2, prev)), result)

        case ((Some(prev), None, result), char) =>
            (Some(char), Some(UncompressedBlock(1, Seq(prev))), result)

        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev == char =>
          (Some(char), Some(CompressedBlock(block.length + 1, block.data)), result)

        case ((Some(_), Some(block@CompressedBlock(_, _)), result), char) =>
          (Some(char), None, result :+ block)

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev != char =>
          (Some(char), Some(UncompressedBlock(block.length + 1, block.data :+ prev)), result)

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) =>
          (Some(char), Some(CompressedBlock(2, prev)), result :+ block)
      }


    (prev, optBlock) match {
      case (Some(prev), Some(block@UncompressedBlock(length, data))) =>
        result :+ UncompressedBlock(block.length + 1, block.data ++ Seq(prev))
      case (_, Some(block)) =>
        result :+ block
      case (Some(prev), None) =>
        result :+ UncompressedBlock(1, Seq(prev))
    }

  }

}
