package org.example

import java.util.*
import kotlin.random.Random

/**
 * Game of Life
 *
 * data representation options
 *  * java.util.BitSet
 */

class GameOfLife(val size: Int) {
    // val name = "this is a name".also(::println)

    var bitSet : java.util.BitSet
    var round : Int = 0
    companion object {
        val MaxSize = Math.sqrt(Integer.MAX_VALUE.toDouble()).toInt()
    }

    init {
        if (size > MaxSize) {
            throw IllegalArgumentException("Error: size (${size}) must be less than ${MaxSize}")
        }

        bitSet = java.util.BitSet(size*size)
    }

    fun getMaxSize(): Int {
        return MaxSize
    }

    fun randomize() {
        for (idx in 0..<bitSet.size()) {
            val rnd = Random.Default.nextBits(1)
            if ((rnd and 1) == 1) {
                bitSet.set(idx);
            }
            else {
                bitSet.clear(idx)
            }
        }
    }

    fun print() {
        println("Game{size=${size};round=${round}}")
        for (yy in 0..<size) {
            for( xx in 0..<size) {
                if (bitSet.get(size*yy + xx)) {
                    print("O ")
                }
                else {
                    print(" ")
                }
            }
            print("\n")
        }
    }

    fun numLiveNeighbors(yy: Int, xx: Int): Int {
        // aboveLeft = bitSet.get()
        // above
        // aboveRight
        // left
        // right
        // belowLeft
        // below
        // belowRight
        return 0
    }

    fun tick() {
        round++
        val newSet : java.util.BitSet = bitSet.clone() as BitSet
        /**
         * Any live cell with fewer than two live neighbours dies, as if by underpopulation.
         * Any live cell with two or three live neighbours lives on to the next generation.
         * Any live cell with more than three live neighbours dies, as if by overpopulation.
         * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
         */
        for (yy in 0..<size) {
            for (xx in 0..<size) {
                val pos = yy*size + xx
                val numLive = numLiveNeighbors(yy, xx)
                val isAlive = newSet.get(pos)
                if (numLive < 2) {
                    bitSet.clear(pos)
                    continue
                }
                if (numLive == 2 || numLive == 3) {
                    // do nothing
                }
                if (numLive > 3) {
                    bitSet.clear(pos)
                    continue
                }
                if (isAlive && (numLive == 3)) {
                    bitSet.set(pos)
                }
            }
        }
    }
}

fun main() {
    println("MaxSize: ${GameOfLife.MaxSize}")
    var game = GameOfLife(16)
    game.randomize()
    game.print()
    for (round in 0..10) {
        game.tick()
        game.print()
    }
}