import cats.data.NonEmptyList
import cats.parse.Rfc5234.alpha
import cats.parse.{Parser, Parser0}

val p1: Parser[NonEmptyList[Char]]  = alpha.rep
val p2: Parser0[List[Char]] = alpha.rep0

p1.parse("")
// Left(Error(0,NonEmptyList(InRange(0,A,Z), InRange(0,a,z))))
p2.parse("")
// Right((,List()))
p2.parse("s1mething")
// Right((,List(s, 1, m, e, t, h, i, n, g)))