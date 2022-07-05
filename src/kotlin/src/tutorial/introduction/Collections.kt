package tutorial.introduction

import java.util.*

fun main() {
    val items = listOf("apple", "banana", "kiwifruit")
    for (item in items) {
        println(item)
    }

    println()
    when {
        "orange" in items -> println("juicy")
        "apple" in items -> println("apple is fine too")
    }

    println()
    val fruits = listOf("banana", "avocado", "apple", "kiwifruit")
    fruits
        .filter { it.startsWith("a") }
        .sortedBy { it }
        .map { it.uppercase(Locale.getDefault()) }
        .forEach { println(it) }
}