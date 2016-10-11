class TSFrame(data: DataFrame) extends java.io.Serializable
{
    def repartition(n: Int) = ???
    def LBKim(candidate: Array[MDVector], start_index: Int, query: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double) = ???
    def envolope(query: DataFrame, window_size: Int): (upper: Array[MDVector], lower: Array[MDVector])
    def LBKoegh(candidate: Array[MDVector], start_index: Int, upper: Array[MDVector], lower: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double) = ???
    def LBKoegh2(candidate: Array[MDVector], start_index: Int, query: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double) = ???/.
    def toVector(row: Row): MDVector = ???
    def DTW(query: DataFrame): (Long, Double) =  {
        data.foreachPartition(it => sequentialDTW(it, query))
        // retrieve bsf from the parameter server
        // return (starting index of nearest candidate, distance)
    }
    def DTWCalculator(buffer: Array[MDVector], start_index: Int, query: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double): Double = ???
    def sequentialDTW(it: Iterator[Row], query: DataFrame): Unit = {
        val (upper: Array[MDVector], lower: Array[MDVector]) = envolope(query)
        val local_bsf = scala.Double.MaxValue
        val m = query.size
        // a circular array used to store the current candidate
        val buffer = Array.ofDim[MDVector](m)
        var i = 0
        val ex: MDVector
        val ex2: MDVector
        while(it.hasNext && i < m-1){
            val current_row = toMDVector(it.next)
            buffer[i] = current_row
            ex += current_row
            ex2 += (current_row * current_row)
            ++i
        }
        while(it.hasNext){
            val current_row = toMDVector(it.next)
            buffer[i % m] = current_row
            ex += current_row
            ex2 += (current_row * current_row)
            ++i

            var dist = scala.Double.MaxValue
            val mean = ex / m
            val std = (ex2 / m - mean * mean).sqrt
            val start_index = (i + 1) % m;
            val lb_kim = LBKim(buffer, start_index, query, mean, std, local_bsf)
            if(lb_kim < local_bsf){
                val lb_keogh = LBKoegh(buffer, start_index, upper, lower, mean, std, local_bsf)
                if(lb_keogh < local_bsf){
                    val lb_keogh2 = LBKoegh2(buffer, start_index, query, mean, std, local_bsf)
                    if(lb_keogh2 < local_bsf){
                        // TODO: optimization 4.1.4 in the UCR paper
                        dist = DTWCalculator(buffer, start_index, query, mean, std, local_bsf)
                    }
                }
            }
            val first_row = buffer[start_index]
            ex -= first_row
            ex -= (first_row * first_row)
            if(dist < local_bsf){
                local_bsf = dist
                // update global bsf
            }
            // retrieve global bsf
        }
    }
}
