package tutorial.basic

val one = 1 // Int
val threeBillion = 3000000000 // Long
val oneLong = 1L // Long
val oneByte: Byte = 1

val oneMillion = 1_000_000
val creditCardNumber = 1234_5678_9012_3456L
val socialSecurityNumber = 999_99_9999L
val hexBytes = 0xFF_EC_DE_5E
val bytes = 0b11010010_01101001_10010100_10010010

val pi = 3.14 // Double
val e = 2.7182818284 // Double
val eFloat = 2.7182818284f // Float，实际值为 2.7182817

fun main() {
    fun printDouble(d: Double) {
        println(d)
    }

    val i = 1
    val d = 1.1
    val f = 1.1f

    printDouble(d)
//    printDouble(i) // 错误：类型不匹配
//    printDouble(f) // 错误：类型不匹配

    val a: Int = 10000
    println(a === a) // 输出“true”
    val boxedA: Int? = a
    val anotherBoxedA: Int? = a
    println(boxedA === anotherBoxedA) // ！！！输出“false”！！！
    println(boxedA == anotherBoxedA) // 输出“true”

    val b: Byte = 1 // OK, 字面值是静态检测的
//    val i: Int = b // 错误
    val j: Int = b.toInt() // OK：显式拓宽
    println(j)

    val l = 1L + 3 // Long + Int => Long
    println(l)

    val x = 5 / 2
    //println(x == 2.5) // ERROR: Operator '==' cannot be applied to 'Int' and 'Double'
    println(x == 2)

    val y = 5L / 2
    println(y == 2L)

    val z = 5 / 2.toDouble()
    println(z == 2.5)

    val t = (1 shl 2) and 0x000FF000
    println(t)
}