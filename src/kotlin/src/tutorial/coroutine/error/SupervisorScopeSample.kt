package tutorial.coroutine.error

import kotlinx.coroutines.*

fun main() = runBlocking {
    try {
        supervisorScope {
            launch {
                try {
                    println("Child is sleeping")
                    delay(Long.MAX_VALUE)
                } finally {
                    println("Child is cancelled")
                }
            }
            // 使用 yield 来给我们的子作业一个机会来执行打印
            yield()
            println("Throwing exception from scope")
            throw AssertionError()
        }
    } catch (e: AssertionError) {
        println("Caught assertion error")
    }
} 
