package main

import (
	"fmt"
	"strings"
)

func MapStrToStr(arr []string, fn func(s string) string) []string {
	var newArray = []string{}
	for _, it := range arr {
		newArray = append(newArray, fn(it))
	}
	return newArray
}

func MapStrToInt(arr []string, fn func(s string) int) []int {
	var newArray = []int{}
	for _, it := range arr {
		newArray = append(newArray, fn(it))
	}
	return newArray
}

func main() {
	var list = []string{"Hao", "Chen", "MegaEase"}
	x := MapStrToStr(list, func(s string) string {
		return strings.ToUpper(s)
	})

	fmt.Printf("%v\n", x) //["HAO", "CHEN", "MEGAEASE"]

	y := MapStrToInt(list, func(s string) int {
		return len(s)
	})

	fmt.Printf("%v\n", y) //[3, 4, 8]

}
