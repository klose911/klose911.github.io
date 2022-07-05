package tutorial.collections.operation

import java.util.*

data class FullName(val firstName: String, val lastName: String)

fun parseFullName(fullName: String): FullName {
    val nameParts = fullName.split(" ")
    if (nameParts.size == 2)
        return FullName(nameParts[0], nameParts[1])
    else
        throw Exception("Wrong name format")
}

data class StringContainer(val values: List<String>)

fun main() {
    // map
    val numbers = setOf(1, 2, 3)
    println(numbers.map { it * 3 }) // [3, 6, 9]
    println(numbers.mapIndexed { idx, value -> value * idx }) // [0, 2, 6]

    println(numbers.mapNotNull {
        if (it == 2)
            null
        else
            it * 3
    }) // [3, 9]
    println(numbers.mapIndexedNotNull { idx, value ->
        if (idx == 0)
            null
        else
            value * idx
    }) // [2. 6]

    val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key11" to 11)
    println(numbersMap.mapKeys { it.key.uppercase(Locale.getDefault()) }) // {KEY1=1, KEY2=2, KEY3=3, KEY11=11}
    println(numbersMap.mapValues { it.value + it.key.length }) // {key1=5, key2=6, key3=7, key11=16}

    // zip
    val colors = listOf("red", "brown", "grey")
    val animals = listOf("fox", "bear", "wolf")
    println(colors zip animals) // [(red, fox), (brown, bear), (grey, wolf)]

    val twoAnimals = listOf("fox", "bear")
    println(colors.zip(twoAnimals)) // [(red, fox), (brown, bear)]

    println(colors.zip(animals) { color, animal ->
        "The ${
            animal.replaceFirstChar {
                if (it.isLowerCase()) it.titlecase(
                    Locale.getDefault()
                ) else it.toString()
            }
        } is $color"
    }) // [The Fox is red, The Bear is brown, The Wolf is grey]

    val numberPairs = listOf("one" to 1, "two" to 2, "three" to 3, "four" to 4)
    println(numberPairs.unzip()) // ([one, two, three, four], [1, 2, 3, 4])

    // associate
    val numberList = listOf("one", "two", "three", "four")
    println(numberList.associateWith { it.length }) // {one=3, two=3, three=5, four=4}

    println(numberList.associateBy {
        it.first().uppercaseChar()
    })  // {O=one, T=three, F=four}

    println(
        numberList.associateBy(
            keySelector = {
                it.first().uppercaseChar()
            },
            valueTransform = {
                it.length
            })
    ) // {O=3, T=5, F=4}

    val names = listOf("Alice Adams", "Brian Brown", "Clara Campbell")
    println(names.associate { name ->
        parseFullName(name).let {
            it.lastName to it.firstName
        }
    }) // {Adams=Alice, Brown=Brian, Campbell=Clara}

    // flat
    val numberSets = listOf(setOf(1, 2, 3), setOf(4, 5, 6), setOf(1, 2))
    println(numberSets.flatten()) // [1, 2, 3, 4, 5, 6, 1, 2]

    val containers = listOf(
        StringContainer(listOf("one", "two", "three")),
        StringContainer(listOf("four", "five", "six")),
        StringContainer(listOf("seven", "eight"))
    )
    println(containers.flatMap { it.values }) // [one, two, three, four, five, six, seven, eight]

    // string join
    println(numberList) // [one, two, three, four]
    println(numberList.joinToString()) // one, two, three, four

    val listString = StringBuffer("The list of numbers: ")
    numberList.joinTo(listString)
    println(listString) // The list of numbers: one, two, three, four

    println(
        numberList.joinToString(
            separator = " | ",
            prefix = "start: ",
            postfix = ": end"
        )
    ) // start: one | two | three | four: end

    println((1..100).toList().joinToString(limit = 10, truncated = "<...>")) // 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, <...>

    println(numberList.joinToString {
        "Element: ${it.uppercase(Locale.getDefault())}"
    }) // Element: ONE, Element: TWO, Element: THREE, Element: FOUR

}