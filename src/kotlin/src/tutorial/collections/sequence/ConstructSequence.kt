package tutorial.collections.sequence


fun main() {
    val numbersSequence1 = sequenceOf("four", "three", "two", "one")

    val numbers = listOf("one", "two", "three", "four")
    val numbersSequence2 = numbers.asSequence()

    val oddNumbers = generateSequence(1) {
        it + 2
    } // `it` 是上一个元素
    println(oddNumbers.take(5).toList())

//    println(oddNumbers.count())     // 错误：此序列是无限的。

    val oddNumbersLessThan10 = generateSequence(1) {
        if (it < 10)
            it + 2
        else
            null
    }
    println(oddNumbersLessThan10.count())

    val oddNumbersWithCodeBlock = sequence {
        yield(1)
        yieldAll(listOf(3, 5))
        yieldAll(generateSequence(7) {
            it + 2
        })
    }
    println(oddNumbersWithCodeBlock.take(5).toList())
}