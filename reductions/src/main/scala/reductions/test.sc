def balance(chars: Array[Char]): Boolean = chars.toStream.filter(c => c == '(' || c == ')').scanLeft(0)((res, c) => res + (if (c == '(') 1 else -1)).forall(_ >= 0)


var arr = "abcd".toCharArray

val str = arr.toStream

//arr(1) = 'z'

str.toList

arr(1) = 'z'

str.toList

str


"abc".filter(_ == 'z').forall(_ => {
  println("x")
  true
})
balance(".".toArray)

"(())".toStream.filter(c => c == '(' || c == ')').scanLeft(0)((res, c) => res + (if (c == '(') 1 else -1)).toArray

new Array[Double](2)(0)

List(1,2,3).tail.zipWithIndex
