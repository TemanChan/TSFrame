package tsframe

import tsframe.DTW._

class TSFrame(data: DataFrame) extends java.io.Serializable
{
    def repartition(n: Int) = ???
    def normalize(x: MDVector, mean: MDVector, std: MDVector): MDVector = (x - mean) / std
    def dist(a: MDVector, b: MDVector): Double = (a - b).magnitudeSquared
    def toVector(row: Row): MDVector = ???
    def DTW(query: DataFrame, window_size: Int): (Long, Double) =  {
        val q: OrderedQuery = new OrderedQuery(query)
        val query_ordered: Array[MDVector] = q.query_ordered
        val order: Array[Int] = q.order
        val (upper: Array[MDVector], lower: Array[MDVector]) = envolope(query_ordered, window_size)
        data.foreachPartition(it => sequentialDTW(it, query_ordered, order, upper, lower, window_size))
        // retrieve bsf from the parameter server
        // return (starting index of nearest candidate, distance)
    }
    def sequentialDTW(it: Iterator[Row], query_ordered: Array[MDVector], order: Array[Int], upper: Array[MDVector], lower: Array[MDVector], window_size: Int): Unit = {
        val local_bsf: Double = scala.Double.MaxValue
        // a circular array used to store the current candidate
        // double the size for avoiding using modulo % operator
        val buffer: Array[MDVector] = Array.ofDim[MDVector](2 * m)
        var i: Long = 0
        val m: Int = query_ordered.length
        val dimension: Int = query_ordered(0).dimension
        val ex: MDVector = new MDVector(dimension)
        val ex2: MDVector = new MDVector(dimension)
        while(it.hasNext && i < m-1){
            val current_row: MDVector = toMDVector(it.next)
            buffer(i) = current_row
            buffer(i + m) = current_row
            ex += current_row
            ex2 += (current_row * current_row)
            i += 1
        }
        while(it.hasNext){
            val current_row: MDVector = toMDVector(it.next)
            buffer(i % m) = current_row
            buffer(i % m + m) = current_row
            ex += current_row
            ex2 += (current_row * current_row)
            i += 1

            var current_distance: Double = scala.Double.MaxValue
            val mean: MDVector = ex / m
            val std: MDVector = (ex2 / m - mean * mean).sqrt
            val start_index: Int = (i + 1) % m;
            val lb_kim: Double = LBKim(buffer, start_index, query, mean, std, local_bsf)
            if(lb_kim < local_bsf){
                val (cb1: Array[MDVector], lb_keogh: Double) = LBKoegh(buffer, start_index, order, upper, lower, mean, std, local_bsf)
                if(lb_keogh < local_bsf){
                    val (c_upper: Array[MDVector], c_lower: Array[MDVector]) = envolope(buffer, window_size)
                    val (cb2: Array[MDVector], lb_keogh2: Double) = LBKoegh2(c_upper, c_lower, query_ordered, order, mean, std, local_bsf)
                    if(lb_keogh2 < local_bsf){
                        // Choose better lower bound between lb_keogh and lb_keogh2 to be used in early abandoning DTW
                        // Note that cb1 or cb2 will be cumulative summed here.
                        // The size of cb is m+1 (while the size of cb1 and cb2 is m), but it does not matter
                        // since DTWCalculator will not use the last element (it only uses the first m elements)
                        val cb: Array[MDVector] = (if(lb_keogh < lb_keogh2) cb2 else cb1)
                                                .scanRight(new MDVector(dimension)){ (x, acc) => acc + x }
                        current_distance = DTWCalculator(buffer, start_index, query, mean, std, local_bsf)
                    }
                }
            }
            val first_row = buffer(start_index)
            ex -= first_row
            ex -= (first_row * first_row)
            if(current_distance < local_bsf){
                local_bsf = dist
                // update global bsf
            }
            // retrieve global bsf
        }
    }
}
