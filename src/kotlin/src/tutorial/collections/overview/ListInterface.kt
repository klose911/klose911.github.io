package tutorial.collections.overview

data class Person(var name: String, var age: Int)

fun main() {
    val numbers = listOf("one", "two", "three", "four")
    println("Number of elements: ${numbers.size}")
    println("Third element: ${numbers.get(2)}")
    println("Fourth element: ${numbers[3]}")
    println("Index of element \"two\" ${numbers.indexOf("two")}")

    val bob = Person("Bob", 31)
    val people = listOf<Person>(Person("Adam", 20), bob, bob)
    val people2 = listOf<Person>(Person("Adam", 20), Person("Bob", 31), bob)
    println(people == people2)
    bob.age = 32
    println(people == people2)

    val mutableNumbers = mutableListOf(1, 2, 3, 4)
    mutableNumbers.add(5)
    mutableNumbers.removeAt(1)
    mutableNumbers[0] = 0
    mutableNumbers.shuffle()
    println(mutableNumbers)
}