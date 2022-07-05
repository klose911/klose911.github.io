package tutorial.functional


fun foo(bar: Int = 0, baz: Int) { /*……*/
}

fun foo(bar: Int = 0, baz: Int = 1, qux: () -> Unit) { /*……*/
}

fun foo(vararg strings: String) { /*……*/
}

fun double(x: Int) = x * 2

fun <T> asList(vararg ts: T): List<T> {
    val result = ArrayList<T>()
    for (t in ts) // ts is an Array
        result.add(t)
    return result
}

infix fun Int.shl(x: Int): Int {
    //……
    return 1
}


class MyStringCollection {
    infix fun add(s: String) { /*……*/
    }

    fun build() {
        this add "abc"   // 正确
        add("abc")       // 正确
        //add "abc"        // 错误：必须指定接收者
    }
}

//fun dfs(graph: Graph) {
//    val visited = HashSet<Vertex>()
//    fun dfs(current: Vertex) {
//        if (!visited.add(current)) return
//        for (v in current.neighbors)
//            dfs(v)
//    }
//
//    dfs(graph.vertices[0])
//}

val eps = 1E-10 // "good enough", could be 10^-15

private fun findFixPointIteration(): Double {
    var x = 1.0
    while (true) {
        val y = Math.cos(x)
        if (Math.abs(x - y) < eps) return x
        x = Math.cos(x)
    }
}

tailrec fun findFixPointTailRec(x: Double = 1.0): Double =
    if (Math.abs(x - Math.cos(x)) < eps) x else findFixPointTailRec(Math.cos(x))

fun main() {


    foo(baz = 1) // 使用默认值 bar = 0

    foo(1) { println("hello") }     // 使用默认值 baz = 1
    foo(qux = { println("hello") }) // 使用两个默认值 bar = 0 与 baz = 1
    foo { println("hello") }        // 使用两个默认值 bar = 0 与 baz = 1

    foo(strings = *arrayOf("a", "b", "c"))

    // 用中缀表示法调用该函数
    1 shl 2

    // 等同于这样
    1.shl(2)

    println("normal iteration: " + findFixPointIteration())
    println("tail recursion: " + findFixPointTailRec())

}

