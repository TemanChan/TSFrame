/**
 * @param candidate A circular array storing the candidate time series
 * @param start_index The start index of the circular array
 * @param order The order in which the query time series is sorted
 * @param upper_ordered The upper envelope of sorted query
 * @param lower_ordered The lower envelope of sorted query
 * @param mean The mean of current candidate
 * @param std The standart deviation of current candidate
 * @param bsf The best-so-far distance
 * @return cb Current bound at each position which will be used later for early abandoning
 * @return lb The Koegh lower bound of the DTW distance
 */
def LBKoegh(candidate: Array[MDVector], start_index: Int, order: Array[Int], upper_ordered: Array[MDVector], lower_ordered: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double): Double = {
    val len: Int = candidate.length
    val cb: Array[Double] = Array.ofDim[Double](len)
    var lb: Double = 0
    var i: Int = 0
    while(i < len && lb < bsf){
        val x: MDVector = (candidate(order(i) + start_index) - mean) / std
        val d: Double = x match {
            case _ if x > upper_ordered(i) => dist(x, upper_ordered(i))
            case _ if x < lower_ordered(i) => dist(x, lower_ordered(i))
            case _ => 0
        }
        lb += d
        cb(order(i)) = d
        i += 1
    }
    (cb, lb)
}

/**
 * @param upper The upper envelope of current candidate
 * @param lower The lower envelope of current candidate
 * @param query_order The sorted query
 * @param order The order in which query is sorted
 * @param mean The mean of current candidate
 * @param std The standard deviation of current candidate
 * @param bsf The best-so-far distance
 * @return cb Current bound at each position which will be used later for early abandoning
 * @return lb Lower bound of the DTW distance
 */
def LBKoegh2(upper: Array[MDVector], lower: Array[MDVector], query_ordered: Array[MDVector], order: Array[Int], mean: MDVector, std: MDVector, bsf: Double): Double = {
    val len: Int = upper.length
    val cb: Array[Double] = Array.ofDim[Double](len)
    var lb: Double = 0
    var i: Int = 0
    while(i < len && lb < bsf){
        val u_norm: MDVector = (upper(order(i)) - mean) / std
        val l_norm: MDVector = (lower(order(i)) - mean) / std
        val qi: MDVector = query_ordered(i)
        val d: Double = qi match {
            case _ if qi > u_norm => dist(qi, u_norm)
            case _ if qi < l_norm => dist(qi, l_norm)
            case _ => 0
        }
        lb += d
        cb(order(i)) = d
        i += 1
    }
    (cb, lb)
}
