package tutorial.collections.iterator

fun main() {
    val numbers = listOf("one", "two", "three", "four")

    val numbersIterator = numbers.iterator()
    while (numbersIterator.hasNext()) {
        println(numbersIterator.next())
    }

    for (item in numbers) {
        println(item)
    }

    numbers.forEach {
        println(it)
    }
}