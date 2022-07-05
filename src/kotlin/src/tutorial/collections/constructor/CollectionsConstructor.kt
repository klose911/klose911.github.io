package tutorial.collections.constructor

import java.util.*
import kotlin.collections.HashSet

val numbersSet = setOf("one", "two", "three", "four")
val emptySet = mutableSetOf<String>()

val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key4" to 1)

val mutableNumbersMap = mutableMapOf<String, String>()
    .apply { this["one"] = "1"; this["two"] = "2" }

val emptyList = emptyList<String>()

val linkedList = LinkedList(listOf("one", "two", "three"))
val presizedSet = HashSet<Int>(32)

fun main() {
    val doubled = List(3, { it * 2 })  // 如果你想操作这个集合，应使用 MutableList
    println(doubled)
}