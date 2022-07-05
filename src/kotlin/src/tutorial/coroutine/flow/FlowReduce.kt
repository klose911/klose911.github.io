package tutorial.coroutine.flow

import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.reduce
import kotlinx.coroutines.runBlocking

fun main() = runBlocking<Unit> {
    val sum = (1..5).asFlow()
        .map { it * it } // 数字 1 至 5 的平方
        .reduce { a, b -> a + b } // 求和（末端操作符）
    println(sum)
}