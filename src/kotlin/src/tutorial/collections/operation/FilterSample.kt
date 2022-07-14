package tutorial.collections.operation

import java.util.*

fun main() {
    val numbers1 = listOf("one", "two", "three", "four")
    val longerThan3 = numbers1.filter { it.length > 3 }
    println(longerThan3)

    val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key11" to 11)
    val filteredMap = numbersMap.filter { (key, value) -> key.endsWith("1") && value > 10 }
    println(filteredMap)

    val filteredIdx = numbers1.filterIndexed { index, s -> (index != 0) && (s.length < 5) }
    val filteredNot = numbers1.filterNot { it.length <= 3 }

    println(filteredIdx)
    println(filteredNot)

    val numbers2 = listOf(null, 1, "two", 3.0, "four")
    println("All String elements in upper case:")
    numbers2.filterIsInstance<String>().forEach {
        println(it.uppercase(Locale.getDefault()))
    }

    val numbers3 = listOf(null, "one", "two", null)
    numbers3.filterNotNull().forEach {
        println(it.length)   // 对可空的 String 来说长度不可用
    }

    // Partition
    val (match, rest) = numbers1.partition { it.length > 3 }
    println(match)
    println(rest)

    println(numbers1.any { it.endsWith("e") })
    println(numbers1.none { it.endsWith("a") })
    println(numbers1.all { it.endsWith("e") })

    println(emptyList<Int>().all { it > 5 })   // vacuous truth

    val empty = emptyList<String>()

    println(numbers1.any()) // true
    println(empty.any()) // false

    println(numbers1.none()) // false
    println(empty.none()) // true
}