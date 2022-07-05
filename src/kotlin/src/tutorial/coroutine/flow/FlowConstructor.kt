package tutorial.coroutine.flow

import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    // 将一个整数区间转化为流
    (1..3).asFlow().collect { value -> println(value) }
}