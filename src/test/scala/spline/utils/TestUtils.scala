package spline.utils

import spline.frontend.*

object TestUtils {
  def parseExpr(input: String): Expr = Expr(input)
  def parseCond(input: String): Cond = Cond(input)
  def parseStat(input: String): Stat = Stat(input)
}
