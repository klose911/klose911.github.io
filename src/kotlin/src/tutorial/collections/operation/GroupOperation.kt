package tutorial.collections.operation

import java.util.*

fun main() {
    val numbers = listOf("one", "two", "three", "four", "five")

    println(numbers.groupBy { it.first().uppercaseChar() }) // {O=[one], T=[two, three], F=[four, five]}
    println(
        numbers.groupBy(keySelector = { it.first() },
            valueTransform = { it.uppercase(Locale.getDefault()) })
    ) // {o=[ONE], t=[TWO, THREE], f=[FOUR, FIVE]}

    println(numbers.groupingBy { it.first() }.eachCount()) // {o=1, t=2, f=2}

}