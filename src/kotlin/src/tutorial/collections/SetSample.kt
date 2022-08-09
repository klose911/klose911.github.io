package tutorial.collections

fun main() {
    val numbers = setOf("one", "two", "three")

    println(numbers union setOf("four", "five")) // [one, two, three, four, five]
    println(setOf("four", "five") union numbers) // [four, five, one, two, three]

    println(numbers intersect setOf("two", "one")) // [one, two]
    println(numbers subtract setOf("three", "four")) // [one, two]
    println(numbers subtract setOf("four", "three")) // 相同的输出 [one, two]
}