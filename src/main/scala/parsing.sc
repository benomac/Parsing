import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser

/* Product */


// the sp parser won't return the whitespace, it just returns Unit if it successful
val p1: Parser[(Char, Unit)] = alpha ~ sp

p1.parse("t")
// res0: Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Left(Error(1,NonEmptyList(InRange(1, , ))))
p1.parse("t ")
// res1: Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Right((,(t,())))




/* productL, productR */

// The type is just Char because we dropping the space
// to drop the alphabet change the arrow side: alpha *> sp
val p2: Parser[Char] = alpha <* sp

// still error since we need the space even if we drop it
p2.parse("t")
// res2: Either[Error, Tuple2[String, Char]] = Left(Error(1,NonEmptyList(InRange(1, , ))))
p2.parse("t ")
// res3: Either[Error, Tuple2[String, Char]] = Right((,t))



val p4: Parser[Char] = sp *> alpha <* sp
val p5: Parser[Char] = alpha.surroundedBy(sp)

p4.parse(" a ")
// res0: Either[Error, Tuple2[String, Char]] = Right((,a))
p5.parse(" a ")
// res1: Either[Error, Tuple2[String, Char]] = Right((,a))



val p6: Parser[Char] = sp *> alpha <* digit
val p7: Parser[Char] = alpha.between(sp, digit)

p6.parse(" a1")
// res2: Either[Error, Tuple2[String, Char]] = Right((,a))
p7.parse(" a1")
// res3: Either[Error, Tuple2[String, Char]] = Right((,a))



val p3: Parser[AnyVal] = alpha | sp

p3.parse("t")
// res4: Either[Error, Tuple2[String, AnyVal]] = Right((,t))
p3.parse(" ")
// res5: Either[Error, Tuple2[String, AnyVal]] = Right((,()))

