package moe.irony.resil.utils

class IdentifierGenerator(val radix: Int) {
  private var idCount: Int = 0

  private def incrementIdCountAndReturnCurrent: Int =
    val last = idCount
    idCount += 1
    last

  def currentCount: Int = {
    return idCount
  }


  private def makeDigitListFromNat(i: Int, acc: List[Int]): List[Int] =
    if i < 0 then acc
    else if i < radix then List(i)
    else makeDigitListFromNat((i / radix) - 1, acc) ++ List(i % radix)

  def joinDigitsToString(xs: List[Int]) =
    xs.zipWithIndex.map { (it, i) => (65 + it).toChar } .mkString

  def nextId: String =
    joinDigitsToString(makeDigitListFromNat(incrementIdCountAndReturnCurrent, List()))
}