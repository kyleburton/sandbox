package main

package fun thing () {
}

fun main(args : Array<String>) {
	for(arg in args) {
		println("arg: $arg")
	}

	var ii : String?
	ii = null
	doSomething(ii)
	ii = ""
	doSomething(ii)
	ii = "whoa hey nice"
	doSomething(ii)
}


fun doSomething(s : String?) {
	println("doSomething: this is before all the coming if nonsense")
	if (s?.length == 0) {
		println("doSomething: er, uh that's an empty string there.")
	}
	else {
		// println("doSomething: hey, you've got a length of: " + s.length)
		println("doSomething: hey, you've got a length of: " + s?.length)
		//println("doSomething: hey, you've got a length of: " + s!!.length)
	}

	if (s?.length == null) {
		println("doSomething: haha, it's nulll ;'-)")
	}

	println("doSomething: this is after all that if nonsense")
	println("")
}
