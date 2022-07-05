package main

import "fmt"

func main() {
	s := []int{2, 3, 5, 7, 11, 13}
	//s == [2 3 5 7 11 13]
	fmt.Println("s ==", s)
	//s[1:4] == [3 5 7]
	fmt.Println("s[1:4] ==", s[1:4])

	// 省略下标代表从 0 开始
	//s[:3] == [2 3 5]
	fmt.Println("s[:3] ==", s[:3])

	// 省略上标代表到 len(s) 结束
	//s[4:] == [11 13]
	fmt.Println("s[4:] ==", s[4:])
}
