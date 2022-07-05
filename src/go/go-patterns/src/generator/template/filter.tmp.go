package PACKAGE_NAME

type GENERIC_NAMEList []GENERIC_TYPE

type GENERIC_NAMEToBool func(*GENERIC_TYPE) bool

func (al GENERIC_NAMEList) Filter(f GENERIC_NAMEToBool) GENERIC_NAMEList {
	var ret GENERIC_NAMEList
	for _, a := range al {
		if f(&a) {
			ret = append(ret, a)
		}
	}
	return ret
}
