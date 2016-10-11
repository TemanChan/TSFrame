def LBKim(candidate: Array[MDVector], start_index: Int, query: Array[MDVector], mean: MDVector, std: MDVector, bsf: Double) = {

    def min(a: Double, b: Double) = scala.math.min(a, b)
    def normalize(x: MDVector) = (x - mean) / std

    val clen: Int = candidate.length
    val qlen: Int = query.length
    // 1 point at front and back
    val x0: MDVector = normalize(candidate(start_index))
    val y0: MDVector = normalize(candidate(start_index + clen - 1))
    var lb: Double = dist(x0, query(0)) + dist(y0, query(qlen - 1))

    if(lb < bsf){
        // 2 points at front
        val x1: MDVector = normalize(candidate(start_index + 1))
        val d: Double = Array(dist(x1, query(0)), dist(x0, query(1)), dist(x1, query(1))).reduce(min)
        lb += d
        if(lb < bsf){
            // 2 points at back
            val y1: MDVector = normalize(candidate(start_index + clen - 2))
            val d: Double = Array(dist(y1, query(qlen - 1)), dist(y0, query(qlen - 2)), dist(y1, query(qlen - 2))).reduce(min);
            lb += d
            if(lb < bsf){
                // 3 points at front
                val x2: MDVector = normalize(candidate(start_index + 2))
                val d: Double = Array(dist(x0, query(2)), dist(x1, query(2)), dist(x2, query(2)),
                                      dist(x2, query(1)), dist(x2, query(0))).reduce(min)
                lb += d
                if(lb < bsf){
                    // 3 points at back
                    val y2: MDVector = normalize(candidate(start_index + clen - 3))
                    val d: Double = Array(dist(y0, query(qlen - 3)), dist(y1, query(qlen - 3)), dist(y2, query(qlen - 3)),
                                          dist(y2, query(qlen - 2)), dist(y2, query(qlen - 1))).reduce(min)
                    lb += d
                }
            }
        }
    }
    lb
}
