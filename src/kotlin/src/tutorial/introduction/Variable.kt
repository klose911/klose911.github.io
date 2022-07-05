package tutorial.introduction

val PI = 3.14
var y = 0

fun incrementY() {
    y += 1
}

fun main() {
    val a: Int = 1  // 立即赋值
    val b = 2   // 自动推断出 `Int` 类型
    val c: Int  // 如果没有初始值类型不能省略
    c = 3       // 明确赋值
    println("a = $a, b = $b, c = $c")

    var x = 5 // 自动推断出 `Int` 类型
    x += 1
    println("x = $x")

    println("y = $y; PI = $PI")
    incrementY()
    println("incrementY()")
    println("y = $y; PI = $PI")
}