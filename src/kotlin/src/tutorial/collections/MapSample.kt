package tutorial.collections

fun main() {
    val numbersMap1 = mapOf("one" to 1, "two" to 2, "three" to 3)
    println(numbersMap1.get("one")) // 1
    println(numbersMap1["one"]) // 1
    println(numbersMap1.getOrDefault("four", 10)) // 10
    println(numbersMap1["five"])               // null
    //numbersMap.getValue("six")      // exception!

    println(numbersMap1.keys)  // [one, two, three]
    println(numbersMap1.values) // [1, 2, 3]

    val numbersMap2 = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key11" to 11)
    val filteredMap = numbersMap2.filter { (key, value) ->
        key.endsWith("1") && value > 10
    }
    println(filteredMap) // {key11=11}

    val filteredKeysMap = numbersMap2.filterKeys { it.endsWith("1") }
    val filteredValuesMap = numbersMap2.filterValues { it < 10 }

    println(filteredKeysMap) // {key1=1, key11=11}
    println(filteredValuesMap) // {key1=1, key2=2, key3=3}


    val numbersMap3 = mapOf("one" to 1, "two" to 2, "three" to 3)
    println(numbersMap3 + Pair("four", 4)) // {one=1, two=2, three=3, four=4}
    println(numbersMap3 + Pair("one", 10)) // {one=10, two=2, three=3}
    println(numbersMap3 + mapOf("five" to 5, "one" to 11)) // {one=11, two=2, three=3, five=5}

    println(numbersMap3 - "one") // {two=2, three=3}
    println(numbersMap3 - listOf("two", "four")) // {one=1, three=3}

    val numbersMap4 = mutableMapOf("one" to 1, "two" to 2)
    numbersMap4.put("three", 3)
    println(numbersMap4) // {one=1, two=2, three=3}

    val numbersMap5 = mutableMapOf("one" to 1, "two" to 2, "three" to 3)
    numbersMap5.putAll(setOf("four" to 4, "five" to 5))
    println(numbersMap5) // {one=1, two=2, three=3, four=4, five=5}

    val numbersMap6 = mutableMapOf("one" to 1, "two" to 2)
    val previousValue = numbersMap6.put("one", 11)
    println("value associated with 'one', before: $previousValue, after: ${numbersMap6["one"]}") // value associated with 'one', before: 1, after: 11
    println(numbersMap6) // {one=11, two=2}

    val numbersMap7 = mutableMapOf("one" to 1, "two" to 2)
    numbersMap7["three"] = 3     // 调用 numbersMap.put("three", 3)
    numbersMap7 += mapOf("four" to 4, "five" to 5)
    println(numbersMap7) // {one=1, two=2, three=3, four=4, five=5}

    val numbersMap8 = mutableMapOf("one" to 1, "two" to 2, "three" to 3)
    numbersMap8.remove("one")
    println(numbersMap8) // {two=2, three=3}
    numbersMap8.remove("three", 4)            //不会删除任何条目
    println(numbersMap8) // {two=2, three=3}

    val numbersMap9 = mutableMapOf("one" to 1, "two" to 2, "three" to 3, "threeAgain" to 3)
    numbersMap9.keys.remove("one")
    println(numbersMap9) // {two=2, three=3, threeAgain=3}
    numbersMap9.values.remove(3)
    println(numbersMap9) // {two=2, threeAgain=3}

    val numbersMap10 = mutableMapOf("one" to 1, "two" to 2, "three" to 3)
    numbersMap10 -= "two"
    println(numbersMap10) // {one=1, three=3}
    numbersMap10 -= "five"             //不会删除任何条目
    println(numbersMap10) // {one=1, three=3}
}