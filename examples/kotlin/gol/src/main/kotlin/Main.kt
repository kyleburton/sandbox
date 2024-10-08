package org.example

import java.awt.BorderLayout
import java.awt.Image
import java.awt.image.BufferedImage
import java.lang.Integer.max
import java.util.*
import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.WindowConstants
import kotlin.collections.HashMap
import kotlin.collections.HashSet
import kotlin.random.Random

/**
 * Game of Life
 *
 * data representation options
 *  * two-dimensional array - the JVM only supports an array of arrays
 *  * single dimensional array - compute coordinates (x,y) => (x*size+y)
 *  * java.util.BitSet - space efficient, trade off is method calls, must compute coordinates (x,y) => (x*size+y)
 *  * sparse matrix - after the first generation, most of the canvas/matrix will be dead, a possible optimization is to only track live cells
 */

abstract class GameOfLife(val size: Int) {
    var round : Int = 0
    companion object {
        val MaxSize = Math.sqrt(Integer.MAX_VALUE.toDouble()).toInt()
    }

    fun getMaxSize(): Int {
        return MaxSize
    }

    abstract fun isSet(yy: Int, xx: Int) : Boolean
    abstract fun tick()
    abstract fun randomize()
}

class GameOfLifeBitSet(size: Int) : GameOfLife(size) {
    var bitSet : BitSet
    var newSet : BitSet

    init {
        if (size > MaxSize) {
            throw IllegalArgumentException("Error: size (${size}) must be less than ${MaxSize}")
        }

        bitSet = java.util.BitSet(size*size)
        newSet = java.util.BitSet(size*size)
    }

    override fun isSet(yy: Int, xx: Int) : Boolean {
        return bitSet.get(yy*size+xx)
    }

    override fun randomize() {
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
                    print("  ")
                }
            }
            print("\n")
        }
    }

    fun numLiveNeighbors(inpSet: BitSet, msize: Int, yy: Int, xx: Int): Int {
        val debug = false
        var count :Int = 0
        var up = yy - 1
        var left = xx - 1
        var right = xx + 1
        var down = yy + 1

        if (up == -1) {
            up = msize-1
        }
        if (left == -1) {
            left = msize-1
        }

        if (down == msize) {
            down = 0
        }
        if (right == msize) {
            right = 0
        }

        val ulVal = inpSet.get(up    * msize + left)
        var upVal = inpSet.get(up    * msize + xx)
        var urVal = inpSet.get(up    * msize + right)
        var llVal = inpSet.get(yy    * msize + left)
        var rrVal = inpSet.get(yy    * msize + right)
        var dlVal = inpSet.get(down  * msize + left)
        var dnVal = inpSet.get(down  * msize + xx)
        var drVal = inpSet.get(down  * msize + right)

        if (debug) {
            println("${round}: [${yy},${xx}] up left    [${up},${left}] => ${up * msize + left} @${ulVal}")
            println("${round}: [${yy},${xx}] up         [${up},${xx}] => ${up * msize + xx} @${upVal}")
            println("${round}: [${yy},${xx}] up right   [${up},${right}] => ${up * msize + right} @${urVal}")

            println("${round}: [${yy},${xx}] left       [${yy},${left}] => ${yy * msize + left} @${llVal}")
            println("${round}: [${yy},${xx}] right      [${yy},${right}] => ${yy * msize + right} @${rrVal}")

            println("${round}: [${yy},${xx}] down left  [${down},${left}] => ${down * msize + left} @${dlVal}")
            println("${round}: [${yy},${xx}] down       [${down},${xx}] => ${down * msize + xx} @${dnVal}")
            println("${round}: [${yy},${xx}] down right [${down},${right}] => ${down * msize + right} @${drVal}")
        }

        if (inpSet.get(up    * msize + left))  { count++ }
        if (inpSet.get(up    * msize + xx))    { count++ }
        if (inpSet.get(up    * msize + right)) { count++ }
        if (inpSet.get(yy    * msize + left))  { count++ }
        if (inpSet.get(yy    * msize + right)) { count++ }
        if (inpSet.get(down  * msize + left))  { count++ }
        if (inpSet.get(down  * msize + xx))    { count++ }
        if (inpSet.get(down  * msize + right)) { count++ }

        return count
    }

    override fun tick() {
        newSet.clear()
        round++
        var liveCount : Int = 0
        var deadCount : Int = 0
        // val newSet : java.util.BitSet = set.clone() as BitSet
        /**
         * Any live cell with fewer than two live neighbours dies, as if by underpopulation.
         * Any live cell with two or three live neighbours lives on to the next generation.
         * Any live cell with more than three live neighbours dies, as if by overpopulation.
         * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
         */
        for (yy in 0..<size) {
            for (xx in 0..<size) {
                val pos = yy*size + xx
                val numLive = numLiveNeighbors(bitSet, size, yy, xx)
                val isAlive = bitSet.get(pos)
                if (isAlive && numLive < 2) {
                    // underpopulation
                    newSet.clear(pos)
                    deadCount++
                    continue
                }
                if (isAlive && (numLive == 2 || numLive == 3)) {
                    // live on to the next generation
                    newSet.set(pos)
                    liveCount++
                    continue
                }
                if (isAlive && numLive > 3) {
                    // overpopulation
                    newSet.clear(pos)
                    deadCount++
                    continue
                }
                if (!isAlive && (numLive == 3)) {
                    // reproduction
                    newSet.set(pos)
                    liveCount++
                    continue
                }
                // println("[${yy},${xx}] = ${isAlive} :: ${numLive}")
                deadCount++
            }
        }
        var tmp : BitSet = bitSet
        bitSet = newSet
        newSet = tmp
        println("tick(): liveCount=${liveCount}; deadCount=${deadCount}; #cells=${size*size}; density=${liveCount.toDouble() / (size.toDouble()*size.toDouble())}")
    }
}

typealias Coord = Pair<Int,Int>
class GameOfLifeSparse(size: Int) : GameOfLife(size) {
    var liveCells : MutableSet<Coord>
    init {
        liveCells = HashSet<Coord>()
    }

    override fun isSet(yy: Int, xx: Int): Boolean {
        return liveCells.contains((Coord(yy, xx)))
    }

    fun countLiveNeighbors(): Map<Coord,Int> {
        var liveNeighborCounts : MutableMap<Coord, Int> = (HashMap<Coord,Int>() as Map<Coord, Int>).toMutableMap()
        for (coord in liveCells) {
            var yy = coord.first
            var xx = coord.second
            var up = yy - 1
            var left = xx - 1
            var right = xx + 1
            var down = yy + 1

            if (up == -1) {
                up = size-1
            }
            if (left == -1) {
                left = size-1
            }

            if (down == size) {
                down = 0
            }
            if (right == size) {
                right = 0
            }

            for (cc in listOf(
                Coord(up,left),   Coord(up,xx),    Coord(up,right),
                Coord(yy,left),  /*Coord(yy,xx),*/ Coord(yy,right),
                Coord(down,left), Coord(down,xx),  Coord(down,right))) {
                liveNeighborCounts[cc] = liveNeighborCounts.getOrDefault(cc, 0) + 1
            }
        }

        return liveNeighborCounts
    }

    override fun tick() {
        ++round
        var liveNeighborCounts = countLiveNeighbors()
        var newLiveCells = HashSet<Coord>()
        var liveCount : Int = 0
        var deadCount : Int = 0

        for ( (coord,numLiveNeighbors) in liveNeighborCounts) {
            val isAlive = liveCells.contains(coord)
            if (isAlive && numLiveNeighbors < 2) {
                // underpopulation
                deadCount++
                continue
            }
            if (isAlive && (numLiveNeighbors == 2 || numLiveNeighbors == 3)) {
                // live on to the next generation
                newLiveCells.add(coord)
                liveCount++
                continue
            }
            if (isAlive && numLiveNeighbors > 3) {
                // overpopulation
                deadCount++
                continue
            }
            if (!isAlive && (numLiveNeighbors == 3)) {
                // reproduction
                newLiveCells.add(coord)
                liveCount++
                continue
            }
            // println("[${yy},${xx}] = ${isAlive} :: ${numLive}")
            deadCount++

        }
        println("tick(): liveCount=${liveCount}; deadCount=${deadCount}; #cells=${size*size}; density=${liveCount.toDouble() / (size.toDouble()*size.toDouble())}")

        liveCells = newLiveCells
    }

    override fun randomize() {
        for (yy in 0..<size) {
            for (xx in 0..<size) {
                val rnd = Random.Default.nextBits(1)
                if ((rnd and 1) == 1) {
                    liveCells.add(Coord(yy, xx))
                }
            }
        }
    }
}

/*
private static JFrame frame;
private static JLabel label;
public static void display(BufferedImage image){
   if(frame==null){
       frame=new JFrame();
       frame.setTitle("stained_image");
       frame.setSize(image.getWidth(), image.getHeight());
       frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
       label=new JLabel();
       label.setIcon(new ImageIcon(image));
       frame.getContentPane().add(label,BorderLayout.CENTER);
       frame.setLocationRelativeTo(null);
       frame.pack();
       frame.setVisible(true);
   }else label.setIcon(new ImageIcon(image));
}
*/

class GolViewer(val size: Int) {
    lateinit var frame : JFrame
    lateinit var label : JLabel

    init {
        label = JLabel()
        frame = JFrame()
        frame.setTitle("game of life")
        val dims = max(size, 1024)
        var image = BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
        // frame.setSize(size, size)
        frame.setSize(dims, dims)
        frame.defaultCloseOperation = WindowConstants.EXIT_ON_CLOSE
        if (size < dims) {
            println("Scaling the image from ${size} to ${dims}")
            label.icon = ImageIcon(image.getScaledInstance(dims, dims, Image.SCALE_SMOOTH))
        }
        else {
            label.icon = ImageIcon(image)
        }

        frame.contentPane.add(label, BorderLayout.CENTER)
        frame.setLocationRelativeTo(null)
        frame.pack()
        frame.isVisible = true

    }

    fun display(gol : GameOfLife) {
        val frameSize = frame.contentPane.size
        var image = BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)

        // img.setRGB(x, y, 0xff0000)
        for (yy in 0..<gol.size) {
            for (xx in 0..<gol.size) {
                var color = 0x00000000
                if (gol.isSet(yy, xx)) {
                    color = 0xFFFFFF
                }
                image.setRGB(xx, yy, color)
            }
        }
        // println("[${gol.round}] Scaling the image from ${size} to ${frameSize}")
        label.icon = ImageIcon(image.getScaledInstance(/* width = */ frameSize.width, /* height = */ frameSize.height, /* hints = */ Image.SCALE_SMOOTH))

    }
}

fun main() {
    println("MaxSize: ${GameOfLife.MaxSize}")
    // val msize = 4
    // val msize = 16
    // val msize = 32
    // val msize = 256
    val msize = 512
    // val msize = 1024
    var game : GameOfLife = GameOfLifeBitSet(msize) // 2.3k rounds 35 fps
    // var game : GameOfLife = GameOfLifeSparse(msize) // 5k rounds 31 fps
    var viewer = GolViewer(game.size)
    game.randomize()
    viewer.display(game)
     // game.print()
    // var maxTicks = 10
    // var maxTicks = 1024
    var maxTicks = Int.MAX_VALUE
    var start = System.nanoTime()
    for (round in 0..maxTicks) {
        game.tick()
        // game.print()
        viewer.display(game)
        // Thread.sleep(10)
        var elapsed = System.nanoTime() - start
        var elapsedSeconds: Double = elapsed / (1000.0*1000.0*1000.0)
        var frameRate: Double = game.round.toDouble() / elapsedSeconds.toDouble()

        println("frameRate=${frameRate} round=${round} elapsed=${elapsedSeconds}")
    }
}