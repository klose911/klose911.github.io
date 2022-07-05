package tutorial.coroutine.flow

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext

fun foo7(): Flow<Int> = flow {
    // 在流构建器中更改消耗 CPU 代码的上下文的错误方式
    withContext(Dispatchers.Default) {
        for (i in 1..3) {
            Thread.sleep(100) // 假装我们以消耗 CPU 的方式进行计算
            emit(i) // 发射下一个值
        }
    }
}

fun main() = runBlocking {
    foo7().collect { value -> println(value) }
}