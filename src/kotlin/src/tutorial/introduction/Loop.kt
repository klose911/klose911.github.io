package tutorial.introduction

fun main() {
    val items = listOf("apple", "banana", "kiwifruit")

    for (item in items) {
        println(item)
    }

    for (index in items.indices) {
        println("item at $index is ${items[index]}")
    }

    var index = 0
    while (index < items.size) {
        println("item at $index is ${items[index]}")
        index++
    }
}