package tutorial.basic

fun decimalDigitValue(c: Char): Int {
    if (c !in '0'..'9')
        throw IllegalArgumentException("Out of range")
    return c.code - '0'.code // 显式转换为数字
}

fun main() {
    val str = "abcd"
    for (c in str) {
        println(c)
    }

    val s1 = "abc" + 1
    println(s1 + "def")

    val s2 = "Hello, world!\n"
    println(s2)

    val text1 = """
    for (c in "foo")
        print(c)
    """
    println(text1)

    val text2 = """
      |Tell me and I forget.
      |Teach me and I remember.
      |Involve me and I learn.
      |(Benjamin Franklin)
      """.trimMargin()
    println(text2)

    val i = 10
    println("i = $i") // 输出“i = 10”

    val s = "abc"
    println("$s.length is ${s.length}") // 输出“abc.length is 3”

    val price = """
        ${'$'}9.99 
        """
    println(price) // $9.99
}

