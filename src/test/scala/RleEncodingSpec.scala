import org.scalatest._
import flatspec._
import matchers._

import scala.math.E

class RleEncodingSpec extends AnyFlatSpec with should.Matchers {

  def victim = new RleEncoding

  "RleEncoding" should "encode given string_1" in {
    victim.encode("ABCDE") should be (List(UncompressedBlock(5,List('A', 'B', 'C', 'D', 'E'))))
  }

  it should "encode given string_2" in {
    victim.encode("AABBCCDD") should be (List(CompressedBlock(2,'A'), CompressedBlock(2,'B'), CompressedBlock(2,'C'), CompressedBlock(2,'D')))
  }

  it should "encode given string_3" in {
    victim.encode("XAABBCCDD") should be (List(UncompressedBlock(1,List('X')), CompressedBlock(2,('A')), CompressedBlock(2,('B')), CompressedBlock(2,('C')), CompressedBlock(2,('D'))))
  }

  it should "encode given string_4" in {
    victim.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB") should be (List(CompressedBlock(4,'A'), CompressedBlock(3,'B'), CompressedBlock(2,'C'), UncompressedBlock(3,List('X', 'Y', 'Z')), CompressedBlock(4,'D'), CompressedBlock(3,'E'), CompressedBlock(3,'F'), CompressedBlock(6,'A'), CompressedBlock(29,'B')))
  }

  it should "encode given string_5" in {
    victim.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX") should be (List(CompressedBlock(4,'A'), CompressedBlock(3,'B'), CompressedBlock(2,'C'), UncompressedBlock(3,List('X', 'Y', 'Z')), CompressedBlock(4,'D'), CompressedBlock(3,'E'), CompressedBlock(3,'F'), CompressedBlock(6,'A'), CompressedBlock(29,'B'), UncompressedBlock(1,List('X'))))
  }
}