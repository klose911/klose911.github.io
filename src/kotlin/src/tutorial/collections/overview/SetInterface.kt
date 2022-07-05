package tutorial.collections.overview

fun main() {
    val numbers = setOf(1, 2, 3, 4)
    println("Number of elements: ${numbers.size}")
    if (numbers.contains(1)) println("1 is in the set")

    val numbersBackwards = setOf(4, 3, 2, 1)
    println("The sets are equal: ${numbers == numbersBackwards}")

    println(numbers.first() == numbersBackwards.first())
    println(numbers.first() == numbersBackwards.last())
}