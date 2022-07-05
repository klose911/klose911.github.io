package tutorial.coroutine.flow

import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.collectLatest
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.runBlocking
import kotlin.system.measureTimeMillis

fun foo10(): Flow<Int> = flow {
    for (i in 1..3) {
        delay(100) // 假装我们异步等待了 100 毫秒
        emit(i) // 发射下一个值
    }
}

fun main() = runBlocking {
    val time = measureTimeMillis {
        foo10()
            .collectLatest { value -> // 取消并重新发射最后一个值
                println("Collecting $value")
                delay(300) // 假装我们花费 300 毫秒来处理它
                println("Done $value")
            }
    }
    println("Collected in $time ms")
}