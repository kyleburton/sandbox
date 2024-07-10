package org.example

import kotlin.math.min
import kotlin.math.sqrt

fun isUnique (s: String) : Boolean {
    val seen = s.toSet()
    return seen.size == s.length
}

fun checkPermutation(s1: String, s2: String) : Boolean {
    if (s1.length != s2.length)  {
        return false
    }
    val c1 = s1.toList().sorted()
    val c2 = s2.toList().sorted()
    return c1 == c2
}

fun URLify(s: String) : String {
    val sb = StringBuilder()
    for( ch in s) {
        sb.append(when (ch) {
            ' ' -> "%20"
            '%' -> "%37"
            else -> ch
        })
    }
    return sb.toString()
}

fun URLify2(s : CharArray, slen : Int, mlen : Int) : Int {
    if (slen < 1) {
        return slen
    }
    var src : Int = slen-1;
    var dst : Int = mlen-1;
    // start by filling from right to left
    while (src >= 0) {
        if (s[src] == ' ') {
            s[dst--] = '0'
            s[dst--] = '2'
            s[dst--] = '%'
            src--
            continue
        }
        if (s[src] == '%') {
            s[dst--] = '7'
            s[dst--] = '3'
            s[dst--] = '%'
            src--
            continue
        }

        s[dst--] = s[src--]
    }

    val newLen : Int = mlen - dst - 1

    // copy back to the start
    src = 0
    dst++
    while(dst < mlen) {
        s[src] = s[dst]
        src++
        dst++
    }

    return newLen
}

fun pallindromePermutation(first: String, second: String): Boolean {
    return first.lowercase().toList().sorted() == second.lowercase().toList().sorted()
}

fun printMatrix(matrix: Array<IntArray>, left: String, right: String) {
    println("comp: ${left} vs ${right}")
    for(yy in 0..<matrix.size) {
        for(xx in 0..<matrix[0].size) {
            print(matrix[yy][xx])
        }
        print("\n")
    }
}
fun EditDistance(left: String, right: String, debug: Boolean = false): Int {
    val matrix: Array<IntArray> = Array(right.length+1) { IntArray(left.length+1)}

    for (yy in 0..<matrix.size) {
        for( xx in 0..<matrix[0].size) {
            matrix[yy][xx] = 0
        }
    }

    for (yy in 0..<matrix.size) {
        matrix[yy][0] = yy
    }

    for( xx in 0..<matrix[0].size) {
        matrix[0][xx] = xx
    }

    if (debug) printMatrix(matrix, left, right)
    for(yy in 1..<matrix.size) {
        for(xx in 1..<matrix[0].size) {
            val lch = right[yy-1]
            val rch = left[xx-1]
            val same = lch == rch
            val upLeftCost = matrix[yy-1][xx-1]
            val upCost = matrix[yy-1][xx]
            val leftCost = matrix[yy][xx-1]
            val baseCost = if(same) 0 else 1
            val cost = min(min(upLeftCost+baseCost, upCost+baseCost), leftCost+baseCost)

            if (debug) println("at [${yy}][${xx}] ${lch} vs ${rch} cost=${cost} baseCost=${baseCost}")
            matrix[yy][xx] = cost

        }
    }
    if (debug) printMatrix(matrix, left, right)
    val res = matrix[right.length][left.length]
    if (debug) println("returning result from matrix[${right.length}][${left.length}] = ${res}")
    return res
}

fun OneAway(first: String, second: String): Boolean {
    return EditDistance(first, second) <= 1
}

fun CompressString(s: String): Any {
    if (s == "") {
        return ""
    }
    val sb = StringBuilder()
    var pos = 0
    while (pos < s.length) {
        var ch = s[pos]
        var count = 1
        while(pos<s.length-1 && ch == s[pos+1]) {
            pos++
            count++
        }
        sb.append(ch)
        sb.append(count)
        pos++

    }
    return sb.toString()
}

fun ParseIntPrefix(s: String) : Pair<Int, String> {
    var count = 0;
    var numDigits = 0;
    for (ch in s) {
        if (!ch.isDigit()) { break }
        count *= 10
        count += ch.digitToInt()
        ++numDigits
    }
    return Pair(count, s.substring(numDigits))
}

fun DecompressString(s: String): String {
    val sb = StringBuilder()

    if (s.length < 2) { return s }

    var s2 = s
    while (s2.length > 0) {
        var ch = s2.substring(0, 1)
        s2 = s2.substring(1)
        var count : Int = 0
        var res = ParseIntPrefix(s2)
        count = res.first
        s2 = res.second
        while(count>0) {
            sb.append(ch)
            count--
        }
    }

    return sb.toString()
}

fun intListToMatrix(intArray: Array<Int>): Array<IntArray> {
    val dim = sqrt(intArray.size.toDouble()).toInt()
    val m: Array<IntArray> = Array(dim) { IntArray(dim)}
    for ( ii in 0..<dim ) {
        for ( jj in 0..<dim) {
            m[ii][jj] = intArray[ii*dim + jj]
        }
    }
    return m
}

fun RotateMatrix(mat : Array<IntArray>): Array<IntArray> {
    // start w/the outer, then each inner "square"
    var tmp : Int
    var xlen = mat[0].size

    for (xx in 0..<(xlen/2)) {
        for (yy in 0..<(xlen/2)) {
            tmp = mat[xx][yy]
            mat[xx][yy] = mat[yy][xlen-1-xx]
            mat[yy][xlen-1-xx] = mat[xlen-1-xx][xlen-1-yy]
            mat[xlen-1-xx][xlen-1-yy] = mat[xlen-1-yy][xx]
            mat[xlen-1-yy][xx] = tmp
        }
    }

    return mat
}

fun cloneMatrix(mat : Array<IntArray>): Array<IntArray> {
    val m2: Array<IntArray> = Array(mat.size) { IntArray(mat[0].size)}
    for (yy in 0..<mat.size) {
        for ( xx in 0..<mat[0].size) {
            m2[yy][xx] = mat[yy][xx]
        }
    }
    return m2
}

fun matrixToString(mat : Array<IntArray>) : String {
    val sb = StringBuilder()

    for (yy in 0..<mat.size) {
        for (xx in 0..<mat[0].size) {
            sb.append(mat[yy][xx].toString())
            sb.append(",")
        }
        sb.append("\n")
    }

    return sb.toString()
}

fun main() {
    println("isUnique")
    assert(isUnique("")) { "Expected empty string to be unique" }
    assert(isUnique("a")) { "Expected 'a' to be unique" }
    assert(isUnique("abcdef"))
    assert(!isUnique("aa"))
    assert(!isUnique("abcdefa"))

    println("checkPermutations")
    assert(checkPermutation("", "")) { "Expected blank strings to be permutations of each other" }
    assert(checkPermutation("a", "a"))
    assert(checkPermutation("cat", "act"))
    assert(!checkPermutation("", "act"))
    assert(!checkPermutation("abc", "def"))

    println("URLify")
    assert(URLify("") == "")
    assert(URLify("this that") == "this%20that")
    assert(URLify("this%that") == "this%37that")


    println("URLify2")
    for (pair in listOf(
        Pair("", ""),
        Pair("this that", "this%20that"),
        Pair("this%that", "this%37that")
    )) {
        val chars = CharArray(pair.first.length * 3) // = pair.first.toCharArray()
        pair.first.toCharArray().copyInto(chars);
        // println("String(chars)=${String(chars).substring(0, pair.first.length)}")
        val len = URLify2(chars, pair.first.length, chars.size)
        val result = String(chars).substring(0, len)
        assert(result == pair.second) {
            "URLify2(${pair.first}, ${chars.size}); Expected: ${pair.second}; Got -> ${result}"
        }
    }

    println("pallindromePermutation")
    for (tri in listOf(
        Triple("", "", true),
        Triple("this", "that", false),
        Triple("taco cat", "atco cta", true)
    )) {
        val result = pallindromePermutation(tri.first, tri.second)
        assert(result == tri.third) { "Expected pallindromePermutation(${tri.first}, ${tri.second}) to be true, it was ${result}" }
    }

    println("EditDistance")
    // EditDistance("ab","ac", debug = true)
    // EditDistance("this","that", debug = true)
    for (tri in listOf(
        Triple("", "", 0),
        Triple("a", "", 1),
        Triple("", "a", 1),
        Triple("ab", "ac", 1),
        Triple("thing", "thing", 0),
        Triple("this", "that", 2),
        Triple("then", "than", 1)
    )) {
        val result = EditDistance(tri.first, tri.second)
        assert(tri.third == result) { "Expected EditDistance(${tri.first}, ${tri.second}}) to be ${tri.third}, it was: ${result}} tri=${tri}"}
    }

    println("OneAway")
    for (tri in listOf(
        Triple("", "", true),
        Triple("then", "than", true),
        Triple("something", "something2", true),
        Triple("one","three", false)
    )) {
        val result = OneAway(tri.first, tri.second)
        assert(result == tri.third) { "Expected OneAway(${tri.first}, ${tri.second}) to be ${tri.third} it was ${result}"}
    }

    println("CompressString/DecompressString")
    for (pair in listOf(
        Pair("a", "a1"),
        Pair("aabcccccaaa", "a2b1c5a3"),
        Pair("abcdef", "a1b1c1d1e1f1"),
        Pair("aaaaaaaaaaaaaaaaaaaaaaaaa", "a25")
    )) {
        val res1 = CompressString(pair.first)
        val res2 = DecompressString(pair.second)
        assert(res1 == pair.second) {"Failed CompressString test case ${pair} got ${res1}"}
        assert(res2 == pair.first)  {"Failed DeompressString test case ${pair} got ${res2}"}
    }

    println("intListToMatrix")
    val m = intListToMatrix(arrayOf(
        1,2,3,4,
        5,6,7,8,
        9,10,11,12,
        13,14,15,16
    ))
    assert(m.size == 4)
    assert(m[0].size == 4)

    println("RotateMatrix")
    for (pair : Pair<Array<IntArray>,Array<IntArray>> in listOf(
        Pair(intListToMatrix(arrayOf(
                1,2,3,4,
                5,6,7,8,
                9,10,11,12,
                13,14,15,16
            )),
            intListToMatrix(arrayOf(
                4,8,12,16,
                3,7,11,15,
                2,6,10,14,
                1,5,9,13
            ))),
        Pair(intListToMatrix(
            arrayOf(
                1,2,
                3,4
            )),
            intListToMatrix(
                arrayOf(
                    2,4,
                    1,3
                )))
    )){
        val res = RotateMatrix(cloneMatrix(pair.first))
        val expectedStr = matrixToString((pair.second))
        val resStr = matrixToString(res)
        println("RotateMatrix =====>")
        println(matrixToString(pair.first))
        println("====================")
        println(expectedStr)
        println("====================")
        println(resStr)
        assert(resStr == expectedStr) { "Failed test case ${pair} got ${resStr}" }
    }
}

