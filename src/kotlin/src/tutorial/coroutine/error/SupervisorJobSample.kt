package tutorial.coroutine.error

import kotlinx.coroutines.*

fun main() = runBlocking {
    val supervisor = SupervisorJob()
    with(CoroutineScope(coroutineContext + supervisor)) {
        // 启动第一个子作业——这个示例将会忽略它的异常（不要在实践中这么做！）
        val firstChild = launch(CoroutineExceptionHandler { _, _ -> }) {
            println("First child is failing")
            throw AssertionError("First child is cancelled")
        }

        // 启动第两个子作业
        val secondChild = launch {
            firstChild.join()
            // 取消了第一个子作业且没有传播给第二个子作业
            println("First child is cancelled: ${firstChild.isCancelled}, but second one is still active")
            try {
                delay(Long.MAX_VALUE)
            } finally { // 但是取消了监督的传播
                println("Second child is cancelled because supervisor is cancelled")
            }
        }

        // 等待直到第一个子作业失败且执行完成
        firstChild.join()
        println("Cancelling supervisor")
        supervisor.cancel()
        secondChild.join()
    }
}
