package tutorial.collections

import kotlin.math.sign

data class Product(val name: String, val price: Double)

fun priceComparison(product: Product, price: Double) = sign(product.price - price).toInt()

fun main() {
    val numbers1 = listOf(1, 2, 3, 4)
    println(numbers1.get(0))
    println(numbers1[0])
    //numbers.get(5)                         // exception!
    println(numbers1.getOrNull(5))             // null
    println(numbers1.getOrElse(5) { it })        // 5

    val numbers2 = (0..13).toList()
    println(numbers2.subList(3, 6)) // [3, 4, 5]

    val numbers3 = listOf(1, 2, 3, 4, 2, 5)
    println(numbers3.indexOf(2)) // 1
    println(numbers3.lastIndexOf(2)) // 4

    val numbers4 = mutableListOf(1, 2, 3, 4)
    println(numbers4.indexOfFirst { it > 2 }) // 2
    println(numbers4.indexOfLast { it % 2 == 1 }) // 2

    val numbers5 = mutableListOf("one", "two", "three", "four")
    numbers5.sort()
    println(numbers5) // [four, one, three, two]
    println(numbers5.binarySearch("two"))  // 3
    println(numbers5.binarySearch("z")) // -5
    println(numbers5.binarySearch("two", 0, 2))  // -3

    val productList = listOf(
        Product("WebStorm", 49.0),
        Product("AppCode", 99.0),
        Product("DotTrace", 129.0),
        Product("ReSharper", 149.0)
    )

    println(productList.binarySearch(Product("AppCode", 99.0),
        compareBy<Product> {
            it.price
        }.thenBy {
            it.name
        }) // 1
    )

    val colors = listOf("Blue", "green", "ORANGE", "Red", "yellow")
    println(colors.binarySearch("RED", String.CASE_INSENSITIVE_ORDER)) // 3

    println(productList.binarySearch { priceComparison(it, 99.0) }) // 1

    val numbers6 = mutableListOf("one", "five", "six")
    numbers6.add(1, "two") //
    numbers6.addAll(2, listOf("three", "four"))
    println(numbers6) // [one, two, three, four, five, six]

    val numbers7 = mutableListOf("one", "five", "three")
    numbers7[1] = "two"
    println(numbers7) // [one, two, three]

    val numbers8 = mutableListOf(1, 2, 3, 4)
    numbers8.fill(3)
    println(numbers8) // [3, 3, 3, 3]

    val numbers9 = mutableListOf(1, 2, 3, 4, 3)
    numbers9.removeAt(1)
    println(numbers9) // [1, 3, 4, 3]

    val numbers10 = mutableListOf("one", "two", "three", "four")

    numbers10.sort()
    println("Sort into ascending: $numbers10") // Sort into ascending: [four, one, three, two]
    numbers10.sortDescending()
    println("Sort into descending: $numbers10") // Sort into descending: [two, three, one, four]

    numbers10.sortBy { it.length }
    println("Sort into ascending by length: $numbers10") // Sort into ascending by length: [two, one, four, three]
    numbers10.sortByDescending { it.last() }
    println("Sort into descending by the last letter: $numbers10") // Sort into descending by the last letter: [four, two, one, three]

    numbers10.sortWith(compareBy<String> { it.length }.thenBy { it })
    println("Sort by Comparator: $numbers10")  // Sort by Comparator: [one, two, four, three]

    println("view of Reversed : ${numbers10.asReversed()}") // view of Reversed : [three, four, two, one]
    println("List after as Reversed : $numbers10")   // List after as Reversed : [one, two, four, three]

    numbers10.reverse()
    println("Reverse: $numbers10")   // Reverse: [three, four, two, one]

    numbers10.shuffle()
    println("Shuffle: $numbers10") // Shuffle: [two, three, four, one]

}