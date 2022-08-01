package tutorial.collections.operation

fun main() {
    val numbers1 = listOf(6, 42, 10, 4)

    println("Count: ${numbers1.count()}") // 4
    println("Max: ${numbers1.maxOrNull()}") // 42
    println("Min: ${numbers1.minOrNull()}") // 4
    println("Average: ${numbers1.average()}") // 15.5
    println("Sum: ${numbers1.sum()}") //

    val numbers2 = listOf(5, 42, 10, 4)
    val min3Remainder = numbers2.minByOrNull { it % 3 }
    println(min3Remainder) // 42

    val strings = listOf("one", "two", "three", "four")
    val longestString = strings.maxWithOrNull(compareBy { it.length })
    println(longestString) // three

    val numbers3 = listOf(5, 42, 10, 4)
    println(numbers3.sumOf { it * 2 }) // 122
    println(numbers3.sumOf { it.toDouble() / 2 }) // 30.5

    val numbers4 = listOf(5, 2, 10, 4)

    val sum = numbers4.reduce { sum, element -> sum + element }
    println(sum) // 21
    val sumDoubled = numbers4.fold(0) { sum, element -> sum + element * 2 }
    println(sumDoubled) // 42

//    val sumDoubledReduce = numbers.reduce { sum, element -> sum + element * 2 } //错误：第一个元素在结果中没有加倍
//    println(sumDoubledReduce)

    val numbers5 = listOf(5, 2, 10, 4)
    val sumDoubledRight = numbers5.foldRight(0) { element, sum -> sum + element * 2 }
    println(sumDoubledRight) // 42

    val numbers6 = listOf(5, 2, 10, 4)
    val sumEven = numbers6.foldIndexed(0) { idx, sum, element ->
        if (idx % 2 == 0)
            sum + element
        else
            sum
    }
    println(sumEven) // 15

    val sumEvenRight = numbers6.foldRightIndexed(0) { idx, element, sum ->
        if (idx % 2 == 0)
            sum + element
        else
            sum
    }
    println(sumEvenRight) // 15

}