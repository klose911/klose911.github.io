package tutorial.collections.operation

class Version(val major: Int, val minor: Int) : Comparable<Version> {
    override fun compareTo(other: Version): Int {
        if (this.major != other.major) {
            return this.major - other.major
        } else if (this.minor != other.minor) {
            return this.minor - other.minor
        } else return 0
    }
}

fun main() {
    println(Version(1, 2) > Version(1, 3)) // false
    println(Version(2, 0) > Version(1, 5)) // true

    val lengthComparator = Comparator { str1: String, str2: String -> str1.length - str2.length }
    println(listOf("aaa", "bb", "c").sortedWith(lengthComparator)) // [c, bb, aaa]
    println(listOf("aaa", "bb", "c").sortedWith(compareBy { it.length })) // [c, bb, aaa]

    val numbers = listOf("one", "two", "three", "four")

    println("Sorted ascending: ${numbers.sorted()}") // Sorted ascending: [four, one, three, two]
    println("Sorted descending: ${numbers.sortedDescending()}") // Sorted descending: [two, three, one, four]

    println("Sorted by length ascending: ${numbers.sortedBy { it.length }}") // Sorted by length ascending: [one, two, four, three]
    println("Sorted by the last letter descending: ${numbers.sortedByDescending { it.last() }}") // Sorted by the last letter descending: [four, two, one, three]

    println("Sorted by length ascending: ${numbers.sortedWith(compareBy { it.length })}") // Sorted by length ascending: [one, two, four, three]

    println(numbers.reversed()) // [four, three, two, one]
    val reversedNumbers = numbers.asReversed()
    println(reversedNumbers) // [four, three, two, one]
    println(numbers) // [one, two, three, four]

    val numbers2 = mutableListOf("one", "two", "three", "four")
    val reversedNumbers2 = numbers2.asReversed()
    println(reversedNumbers2) // [four, three, two, one]
    numbers2.add("five")
    println(reversedNumbers2) // [five, four, three, two, one]

    println(numbers.shuffled())
}