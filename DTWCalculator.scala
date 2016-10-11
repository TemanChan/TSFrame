def DTWCalculator[T](dist: (T, T) => Double)(A: Array[T], B: Array[T], cb: Array[Double], window_size: Int, bsf: Double): Double = {
	import scala.math.{min, max}
	val INF: Double = scala.Double.MaxValue
	val m: Int = A.length
	val r: Int = min(window_size, m-1)
	var cost: Array[Double] = Array.ofDim[Double](2 * r + 1)
	var cost_prev: Array[Double] = Array.ofDim[Double](2 * r + 1)
	cost.map(x => INF)
	cost_prev.map(x => INF)
	cost_prev(r) = 0
	for(i <- 0 until m){
		var k: Int = max(0, r-i)
		var min_cost: Double = INF
		for(j <- max(0, i-r) to min(m-1, i+r)){
			// accumulative_cost(i)(j-1)
			val x: Double = if(k > 0) cost(k-1) else INF
			// accumulative_cost(i-1)(j-1)
			val y: Double = cost_prev(k)
			// accumulative_cost(i-1)(j)
			val z: Double = if(k < 2*r) cost_prev(k+1) else INF
			cost(k) = min(x, min(y, z)) + dist(A(i), B(j))
			if(cost(k) < min_cost) min_cost = cost(k)
			k += 1
		}
		if(i+r < m-1 && min_cost + cb(i+r+1) >= bsf){
			return min_cost + cb(i+r+1)
		}
		cost_prev = cost
	}
	cost(r)
}

// test case
// note that A and B must have the same length because we use the cummulative bound obtained from LBKeogh
val A = Array(1, 1, 2, 3, 2, 0)
val B = Array(0, 1, 1, 2, 3, 2)
val cb = Array.ofDim[Double](a.length)
val window_size = 6
val bsf = scala.Double.MaxValue
val dist = (a: Int, b: Int) => ((a - b) * (a - b)).toDouble
DTWCalculator(dist)(A, B, cb, window_size, bsf)