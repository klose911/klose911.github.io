package org.klose.concurrency.visibility.thisEscape;

public class ThisEscapeTest {
    public static void main(String[] args) throws InterruptedException {
        EventSource<EventListener> source = new EventSource<>();
        ListenerRunnable listRun = new ListenerRunnable(source);
        Thread thread = new Thread(listRun);
        thread.start();
        //　　出错的原因在于escape1可能还没有完全初始化
        // 　但在listRun已经可以看见esacape1的引用，这也就是所谓的this溢出
        ThisEscape escape1 = new ThisEscape(source);
    }
}