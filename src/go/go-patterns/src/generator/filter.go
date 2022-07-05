package generator

import "fmt"

type Employee struct {
	Name     string
	Age      int
	Vacation int
	Salary   int
}

//go:generate ./gen.sh ./template/filter.tmp.go gen Employee filter
func filterEmployeeExample() {
	var list = EmployeeList{
		{"Hao", 44, 0, 8000},
		{"Bob", 34, 10, 5000},
		{"Alice", 23, 5, 9000},
		{"Jack", 26, 0, 4000},
		{"Tom", 48, 9, 7500},
	}
	var filter EmployeeList
	filter = list.Filter(func(e *Employee) bool {
		return e.Age > 40
	})
	fmt.Println("----- Employee.Age > 40 ------")
	for _, e := range filter {
		fmt.Println(e)
	}
	filter = list.Filter(func(e *Employee) bool {
		return e.Salary <= 5000
	})
	fmt.Println("----- Employee.Salary <= 5000 ------")
	for _, e := range filter {
		fmt.Println(e)
	}
}
