package tutorial.coroutine

import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    repeat(100_000) {
    // 启动大量的协程
        launch {
            delay(1000L)
            print(".")
        }
    }
}