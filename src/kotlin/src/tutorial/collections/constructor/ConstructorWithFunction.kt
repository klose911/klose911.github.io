package tutorial.collections.constructor

fun main() {
    val numberList = listOf("one", "two", "three", "four")
    val longerThan3 = numberList.filter { it.length > 3 }
    println(longerThan3)

    val numberSet = setOf(1, 2, 3)
    println(numberSet.map { it * 3 })
    println(numberSet.mapIndexed { idx, value -> value * idx })

    println(numberList.associateWith { it.length })
}