package tutorial.introduction

fun sum1(a: Int, b: Int): Int {
    return a + b
}
//sampleEnd
fun sum2(a: Int, b: Int) = a + b

fun printSum(a: Int, b: Int) {
    println("sum of $a and $b is ${a + b}")
}

fun main() {
    print("sum of 3 and 5 is ")
    println(sum1(3, 5))
    println("sum of 19 and 23 is ${sum2(19, 23)}")
    printSum(-1, 8)
}

