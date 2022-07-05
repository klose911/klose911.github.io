package main

import (
	"fmt"
	"log"
	"net/http"
)

type Hello struct{}

//类型 Hello 实现了 http.Handler, 收到任何请求每次都返回'Hello!'字符串
func (h Hello) ServeHTTP(
	w http.ResponseWriter,
	r *http.Request) {
	fmt.Fprint(w, "Hello!")
}

func main() {
	var h Hello
	//监听4000端口
	err := http.ListenAndServe("localhost:4000", h)
	if err != nil {
		log.Fatal(err)
	}
}
