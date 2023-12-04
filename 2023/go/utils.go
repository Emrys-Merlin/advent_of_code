package main

// https://stackoverflow.com/questions/34018908/golang-why-dont-we-have-a-set-datastructure
func union[T comparable](s1, s2 map[T]bool) map[T]bool {
	s_union := map[T]bool{}
	for k := range s1{
    	s_union[k] = true
	}
	for k := range s2{
    	s_union[k] = true
	}
	return s_union
}

// https://stackoverflow.com/questions/34018908/golang-why-dont-we-have-a-set-datastructure
func intersection[T comparable](s1, s2 map[T]bool) map[T]bool {
	s_intersection := map[T]bool{}
	if len(s1) > len(s2) {
		s1, s2 = s2, s1 // better to iterate over a shorter set
	}
	for k := range s1 {
		if s2[k] {
	 		s_intersection[k] = true

		}
	}

	return s_intersection
}
