package tutorial.coroutine.concurrency

import kotlinx.coroutines.*
import kotlin.system.measureTimeMillis

suspend fun massiveRun(action: suspend () -> Unit) {
    val n = 100 // 启动的协程数量
    val k = 1000 // 每个协程重复执行同一动作的次数
    val time = measureTimeMillis {
        coroutineScope {
            // 协程的作用域
            repeat(n) {
                launch {
                    repeat(k) {
                        action()
                    }
                }
            }
        }
    }
    println("Completed ${n * k} actions in $time ms")
}

var counter1 = 0

fun main() = runBlocking {
    withContext(Dispatchers.Default) {
        massiveRun {
            counter1++
        }
    }
    println("Counter = $counter1")
}
