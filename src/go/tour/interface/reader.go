package main

import (
	"fmt"
	"io"
	"strings"
)

func main() {
	//创建了一个 strings.Reader， 并且以每次 8 字节的速度读取它的输出
	r := strings.NewReader("Hello, Reader!")
	b := make([]byte, 8)

	// n = 8 err = <nil> b = [72 101 108 108 111 44 32 82]
	// b[:n] = "Hello, R"
	// n = 6 err = <nil> b = [101 97 100 101 114 33 32 82]
	// b[:n] = "eader!"
	// n = 0 err = EOF b = [101 97 100 101 114 33 32 82]
	// b[:n] = ""
	for {
		n, err := r.Read(b)
		fmt.Printf("n = %v err = %v b = %v\n", n, err, b)
		fmt.Printf("b[:n] = %q\n", b[:n])
		if err == io.EOF {
			break
		}
	}
}
