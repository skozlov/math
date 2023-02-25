import com.github.skozlov.math.arithmetic.Implicits._
import com.github.skozlov.math.arithmetic.Rational

0.1 + 0.2 // 0.30000000000000004
BigDecimal("0.1") + BigDecimal("0.2") // 0.3
Rational("0.1") + Rational("0.2") // 3/10

BigDecimal(1) / 3 * 3 // 0.9999999999999999999999999999999999
Rational("1/3") * 3 // 1
