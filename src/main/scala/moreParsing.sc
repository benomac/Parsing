import cats.data.NonEmptyList
import cats.parse.Rfc5234._
import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.Parser._
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser

case class Discount(amount: Int, forPrice: BigDecimal)
case class StockKeepingunit(itemName: String, price: BigDecimal, amount: Int, forThis: BigDecimal)
case class StockKeepingUnitWithOption(itemName: String, price: BigDecimal, discount:
Option[Discount])

//val skuListParser: Parser[List[String]] = {
//
//}

val skuItemParser: Parser[StockKeepingunit] = {
  val comma = char(',')
  val multiDigitStop = digit.rep.string.map(_.toInt) <* char('.')
  val doubleDigit = digit ~ digit
  val none = string("none")
  (
    (alpha <* comma)
      ~ (multiDigitStop ~ doubleDigit)
      ~ comma
      ~ (digit <* comma)
      ~ (multiDigitStop ~ doubleDigit) | none

    ).string.map(_.split(",").toList match {
      case List(name, price, specialPrice, amountForSpecialPrice) => (name, price, specialPrice, amountForSpecialPrice)
        StockKeepingunit(
          name,
          BigDecimal.valueOf(price.toDouble),
          specialPrice.toInt,
          BigDecimal.valueOf(amountForSpecialPrice.toDouble))
    }
  )
}

val c: Product = skuItemParser.parse("a,00000.10,1,0.00") match {
  case Left(v) => v
  case Right(v) => v._2
}



//val foo: Either[Error, (((Char, String), Char), String)] = skuItemParser.parse("a,00000.10,1,066.00") match {
//  case Left(value) => Left(value)
//  case Right(value) => Right(value._2)
//}
//
//val d: List[String] = foo.getOrElse("error").split(",").toList
//
//def createStockKeepingUnit(list: List[String]): StockKeepingunit = {
//  val params = list match {
//    case List(a, b, c, d) => (a, b, c, d)
//      StockKeepingunit(a, BigDecimal.valueOf(b.toDouble), c.toInt, BigDecimal.valueOf(d.toDouble))
//  }
//  params
//}
//
//val s: StockKeepingunit = createStockKeepingUnit(d)
//
//s.amount