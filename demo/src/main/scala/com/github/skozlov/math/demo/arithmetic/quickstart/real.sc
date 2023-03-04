import com.github.skozlov.math.arithmetic.{Rational, π}

Math.PI // 3.141592653589793
π.round(precision = Rational(10) ^ -16).toPeriodic() // 3.1415926535897932
