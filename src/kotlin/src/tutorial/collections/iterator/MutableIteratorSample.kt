package tutorial.collections.iterator

fun main() {
    val numbers1 = mutableListOf("one", "two", "three", "four")
    val mutableIterator = numbers1.iterator()

    mutableIterator.next()
    mutableIterator.remove()
    println("After removal: $numbers1")

    val numbers2 = mutableListOf("one", "four", "four")
    val mutableListIterator = numbers2.listIterator()

    mutableListIterator.next()
    mutableListIterator.add("two")
    mutableListIterator.next()
    mutableListIterator.set("three")
    println(numbers2)
}