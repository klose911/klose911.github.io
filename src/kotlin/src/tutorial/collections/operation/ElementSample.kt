package tutorial.collections.operation

fun main() {
    val numbersSet = linkedSetOf("one", "two", "three", "four", "five")
    println(numbersSet.elementAt(3)) // four

    val numbersSortedSet = sortedSetOf("one", "two", "three", "four")
    println(numbersSortedSet.elementAt(0)) // 元素以升序存储 four

    val numbers1 = listOf("one", "two", "three", "four", "five")
    println(numbers1.first()) // one
    println(numbers1.last()) // five

    println(numbers1.elementAtOrNull(5)) // null
    println(numbers1.elementAtOrElse(5) { index ->
        "The value for index $index is undefined"
    }) // The value for index 5 is undefined

    val numbers2 = listOf("one", "two", "three", "four", "five", "six")
    println(numbers2.first { it.length > 3 }) // three
    println(numbers2.last { it.startsWith("f") }) // five
    println(numbers2.firstOrNull { it.length > 6 }) // null

    val numbers3 = listOf(1, 2, 3, 4)
    println(numbers3.find { it % 2 == 0 }) // 2
    println(numbers3.findLast { it % 2 == 0 }) // 4
    println(numbers3.findLast { it % 5 == 0 }) // null

    println(numbers3.random())

    val numbers4 = listOf("one", "two", "three", "four", "five", "six")
    println(numbers4.contains("four")) // true
    println("zero" in numbers4) // false

    println(numbers4.containsAll(listOf("four", "two"))) // true
    println(numbers4.containsAll(listOf("one", "zero"))) // false

    println(numbers4.isEmpty()) // false
    println(numbers4.isNotEmpty()) // true

    val empty = emptyList<String>()
    println(empty.isEmpty()) // true
    println(empty.isNotEmpty()) // false
}