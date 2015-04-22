package utils

object StringUtils {
  implicit class StringImprovements(val s: String) {
    import scala.util.control.Exception._
    def toBigDecimalOpt = catching(classOf[NumberFormatException]) opt BigDecimal(s)
  }
}
