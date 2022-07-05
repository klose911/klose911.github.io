package org.klose.concurrency.visibility.thisEscape;


public class ThisEscape {
    public final int id;
    public final String name;

    public ThisEscape(EventSource<EventListener> source) {
        id = 1;
        source.registerListener(new EventListener() {
            public void onEvent(Object obj) {
                System.out.println("id: " + ThisEscape.this.id);
                System.out.println("name: " + ThisEscape.this.name);
            }
        });

        try {
            Thread.sleep(1000); // 调用sleep模拟其他耗时的初始化操作
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        name = "flysqrlboy";
    }

}
