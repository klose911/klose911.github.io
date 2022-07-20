package tutorial.collections.operation

fun main() {
    val numbers1 = listOf("one", "two", "three", "four", "five", "six")
    println(numbers1.slice(1..3)) // [two, three, four]
    println(numbers1.slice(0..4 step 2)) // [one, three, five]
    println(numbers1.slice(setOf(3, 5, 0))) // [four, six, one]

    val numbers2 = listOf("one", "two", "three", "four", "five", "six")
    println(numbers2.take(3)) // [one, two, three]
    println(numbers2.takeLast(3)) // [four, five, six]
    println(numbers2.drop(1)) // [two, three, four, five, six]
    println(numbers2.dropLast(5)) // [one]

    val numbers3 = listOf("one", "two", "three", "four", "five", "six")
    println(numbers3.takeWhile { !it.startsWith('f') }) // [one, two, three]
    println(numbers3.takeLastWhile { it != "three" }) // [four, five, six]
    println(numbers3.dropWhile { it.length == 3 }) // [three, four, five, six]
    println(numbers3.dropLastWhile { it.contains('i') }) // [one, two, three, four]

    val numbers4 = (0..13).toList()
    println(numbers4.chunked(3)) // [[0, 1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11], [12, 13]]
    println(numbers4.chunked(3) { it.sum() })  // `it` 为原始集合的一个块 [3, 12, 21, 30, 25]

    val numbers5 = listOf("one", "two", "three", "four", "five")
    println(numbers5.windowed(3)) // [[one, two, three], [two, three, four], [three, four, five]]

    val numbers6 = (1..10).toList()
    println(
        numbers6.windowed(
            3,
            step = 2,
            partialWindows = true
        )
    ) // [[1, 2, 3], [3, 4, 5], [5, 6, 7], [7, 8, 9], [9, 10]]
    println(numbers6.windowed(3) { it.sum() }) // [6, 9, 12, 15, 18, 21, 24, 27]

    val numbers7 = listOf("one", "two", "three", "four", "five")
    println(numbers7.zipWithNext()) // [(one, two), (two, three), (three, four), (four, five)]
    println(numbers7.zipWithNext() { s1, s2 -> s1.length > s2.length }) // [false, false, true, false]

}