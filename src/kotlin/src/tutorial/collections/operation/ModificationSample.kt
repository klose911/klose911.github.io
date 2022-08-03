package tutorial.collections.operation

fun main() {
    val numbers1 = mutableListOf(1, 2, 3, 4)
    numbers1.add(5)
    println(numbers1) // [1, 2, 3, 4, 5]

    val numbers2 = mutableListOf(1, 2, 5, 6)
    numbers2.addAll(arrayOf(7, 8))
    println(numbers2) // [1, 2, 5, 6, 7, 8]
    numbers2.addAll(2, setOf(3, 4))
    println(numbers2) // [1, 2, 3, 4, 5, 6, 7, 8]

    val numbers3 = mutableListOf("one", "two")
    numbers3 += "three"
    println(numbers3) // [one, two, three]
    numbers3 += listOf("four", "five")
    println(numbers3) // [one, two, three, four, five]

    val numbers4 = mutableListOf(1, 2, 3, 4, 3)
    numbers4.remove(3)                    // 删除了第一个 `3`
    println(numbers4) // [1, 2, 4, 3]
    numbers4.remove(5)                    // 什么都没删除
    println(numbers4) // [1, 2, 4, 3]

    val numbers5 = mutableListOf(1, 2, 3, 4)
    println(numbers5) // [1, 2, 3, 4]
    numbers5.retainAll { it >= 3 }
    println(numbers5) // [3, 4]
    numbers5.clear()
    println(numbers5) // []

    val numbersSet = mutableSetOf("one", "two", "three", "four")
    numbersSet.removeAll(setOf("one", "two"))
    println(numbersSet) // [three, four]

    val numbers6 = mutableListOf("one", "two", "three", "three", "four")
    numbers6 -= "three"
    println(numbers6) // [one, two, three, four]
    numbers6 -= listOf("four", "five")
    println(numbers6) // [one, two, three]

}